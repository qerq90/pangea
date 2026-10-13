package pangea.service.state.states.parcel

import pangea.engine.SceneContent
import pangea.model.hero.{Hero, HeroId}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.chat.ChatCommand
import pangea.service.chat.ChatCommand.Money
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler}
import pangea.test._
import zio.ZIO
import zio.test._

/** Деньги из рук в руки: «Передать серебро 100» и «Передать дублоны 5» в
  * ответ на сообщение игрока в общей беседе. В сумке их не ищут — серебро
  * ложится получателю в ячейку (или ждёт его в городе), дублоны ждут в городе. */
object MoneyTransferSpec extends ZIOSpecDefault {

  private val userId     = UserId(1L)
  private val sender     = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val friendUs   = UserId(2L)
  private val friendHero = HeroId(2L)
  private val friend     = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))

  /** Беседа целиком: отправитель в Торговом доме, у получателя свой герой.
    * Ячейка в тестах одна на всех — по ней видно, что серебро легло получателю. */
  private def chat(silver: Long = 0L, doubloons: Long = 0L, vaultCells: Int = 1,
                   state: StateType = StateType.TradeHouse) =
    for {
      heroDao  <- TestHeroDao.withHero(userId,
                    TestFixtures.hero(userId, state = state).copy(silver = silver, doubloons = doubloons))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friendHero))
      userRepo <- TestUserRepository.withUsers(sender, friend)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      lock     <- PlayerLock.make
      bank      = TestBankRepository.withCells(vaultCells)
      payoutDao = TestPayoutDao.empty
      payouts   = Payouts(payoutDao, heroDao, content)
      players   = new TestPlayers
      inv       = TestInventoryRepository.accepting
      parcels   = Parcels(TestParcelDao.empty, bank, content)
      transfers = Transfers(inv, userRepo, parcels, players, content, heroDao, bank, payouts)
      handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                    parcels, transfers, Map.empty[StateType, State], AdminPanel.disabled, content, lock)
    } yield (handler, heroDao, bank, payoutDao, players, api)

  /** Что после «Передать …» написали в беседе: текст после первого слова. */
  private def say(handler: StateHandler, text: String, eventId: Long) =
    handler.transferFromChat(sender.vkId, friend.vkId,
      ChatCommand.transferQuery(text).getOrElse(""), eventId)

  private def heroOf(dao: TestHeroDao, id: UserId): zio.Task[Hero] =
    dao.getHeroByUserId(id).map(_.get)

  override def spec = suite("Передача денег")(

    test("разбор: валюта по слову или значку, сумма — числом, «1 000» — тысяча") {
      assertTrue(ChatCommand.moneyQuery("серебро 100").contains(Money.Silver -> Some(100L))) &&
      assertTrue(ChatCommand.moneyQuery("100 серебра").contains(Money.Silver -> Some(100L))) &&
      assertTrue(ChatCommand.moneyQuery("🪙 1 000").contains(Money.Silver -> Some(1000L))) &&
      assertTrue(ChatCommand.moneyQuery("Дублоны 5").contains(Money.Doubloons -> Some(5L))) &&
      assertTrue(ChatCommand.moneyQuery("5 дублонов").contains(Money.Doubloons -> Some(5L))) &&
      // суммы нет — спросим, сколько
      assertTrue(ChatCommand.moneyQuery("серебро").contains(Money.Silver -> None)) &&
      assertTrue(ChatCommand.moneyQuery("дублоны 0").contains(Money.Doubloons -> None)) &&
      // «серебряное кольцо» — вещь, а не деньги; обе валюты разом не разобрать
      assertTrue(ChatCommand.moneyQuery("серебряное кольцо").isEmpty) &&
      assertTrue(ChatCommand.moneyQuery("надколотый череп 2 штуки").isEmpty) &&
      assertTrue(ChatCommand.moneyQuery("серебро и дублоны").isEmpty)
    },

    test("серебро: с отправителя списано, получателю легло в ячейку, беседа знает") {
      for {
        t <- chat(silver = 500L)
        (handler, dao, bank, payoutDao, players, api) = t
        _      <- say(handler, "Передать серебро 100", -1L)
        me     <- heroOf(dao, userId)
        sent   <- api.sentMessages
      } yield assertTrue(me.silver == 400L) &&
              assertTrue(bank.silverSnapshot == 100L && payoutDao.snapshot.isEmpty) &&
              assertTrue(sent.exists(_._1.contains("🪙 100 отправлено игроку"))) &&
              assertTrue(players.announced.exists(m => m.contains("передал 🪙 100") &&
                m.contains("[id" + sender.vkId.value) && m.contains("[id" + friend.vkId.value))) &&
              // получатель знает, где теперь его деньги
              assertTrue(players.sentLetters.exists { case (to, text) =>
                to == friendUs && text.contains("🪙 100") && text.contains("ячейке") })
    },

    test("ячейки у получателя нет — серебро ждёт его в городе") {
      for {
        t <- chat(silver = 500L, vaultCells = 0)
        (handler, dao, bank, payoutDao, players, _) = t
        _  <- say(handler, "Передать 250 серебра", -1L)
        me <- heroOf(dao, userId)
      } yield assertTrue(me.silver == 250L && bank.silverSnapshot == 0L) &&
              assertTrue(payoutDao.snapshot.get(friendHero).contains((250L, 0L))) &&
              assertTrue(players.sentLetters.exists { case (to, text) =>
                to == friendUs && text.contains("в городе") })
    },

    test("дублоны всегда ждут в городе: ячейка держит только серебро") {
      for {
        t <- chat(doubloons = 10L)
        (handler, dao, bank, payoutDao, _, api) = t
        _    <- say(handler, "Передать дублоны 3", -1L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages
      } yield assertTrue(me.doubloons == 7L && bank.silverSnapshot == 0L) &&
              assertTrue(payoutDao.snapshot.get(friendHero).contains((0L, 3L))) &&
              assertTrue(sent.exists(_._1.contains("🟡 3 отправлено игроку")))
    },

    test("денег не хватает или сумму не назвали — ничего не списано, вещей не ищем") {
      for {
        t <- chat(silver = 50L, doubloons = 1L)
        (handler, dao, bank, payoutDao, players, api) = t
        _    <- say(handler, "Передать серебро 100", -1L)
        _    <- say(handler, "Передать дублоны 5", -2L)
        _    <- say(handler, "Передать серебро", -3L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(me.silver == 50L && me.doubloons == 1L) &&
              assertTrue(bank.silverSnapshot == 0L && payoutDao.snapshot.isEmpty) &&
              assertTrue(players.announced.isEmpty) &&
              assertTrue(sent.contains("Столько серебра у вас нет") && sent.contains("🪙 50")) &&
              assertTrue(sent.contains("Столько дублонов у вас нет") && sent.contains("🟡 1")) &&
              assertTrue(sent.contains("Сколько передать?")) &&
              // в сумке «серебро» не искали
              assertTrue(!sent.contains("В сумке нет ничего похожего"))
    },

    test("из лабиринта денег не передать — только из города") {
      for {
        t <- chat(silver = 500L, state = StateType.Dungeon)
        (handler, dao, bank, _, _, api) = t
        _    <- say(handler, "Передать серебро 100", -1L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(me.silver == 500L && bank.silverSnapshot == 0L) &&
              assertTrue(sent.contains("только из города"))
    }
  )
}
