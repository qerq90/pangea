package pangea.service.state.states.parcel

import pangea.engine.SceneContent
import pangea.model.artifact.HeroArtifacts
import pangea.model.hero.HeroId
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.chat.ChatCommand
import pangea.service.parcel.Parcels
import pangea.service.state.UserAction
import pangea.test._
import zio.ZIO
import zio.test._

/** Передача вещей между игроками: команда из общей беседы, выбор в личке и
  * посылка, которая сама ложится получателю в банковскую ячейку. */
object TransferSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val heroId   = HeroId(1L)
  private val friendUs = UserId(2L)
  private val friend   = HeroId(2L)
  private val friendUser = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def skull(id: Long): Item =
    Item(id, "Расколотый череп", 1L, Rarity.Gray, ItemType.Trophy, attack = 0, accuracy = 0,
      energy = 0, armor = 0, defence = 0, evasion = 0)

  private def sword(id: Long, name: String = "Меч"): Item =
    Item(id, name, 3L, Rarity.Blue, ItemType.Weapon, attack = 7, accuracy = 2, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private def scene(query: String, count: Int) =
    TransferState.TransferScene(
      toHeroId = friend.value, toUserId = friendUs.value, toName = "Пётр", query = query, count = count)

  private def transfer(items: List[Item], query: String, count: Int = 1) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.TradeHouse))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friend))
      _        <- heroDao.writeSceneData(userId, io.circe.syntax.EncoderOps(scene(query, count)).asJson)
      invRepo   = TestInventoryRepository.withItems(items)
      userRepo <- TestUserRepository.withUsers(testUser, friendUser)
      parcelDao = TestParcelDao.empty
      bank      = TestBankRepository.withCells(1)
      content  <- ZIO.attempt(SceneContent.load())
      players   = new TestPlayers
      renderer <- TestRenderer.make
      parcels   = Parcels(parcelDao, bank, content)
    } yield (TransferState(heroDao, invRepo, userRepo, parcels, players, content),
             invRepo, parcelDao, bank, players, renderer)

  override def spec = suite("Передача вещей")(

    test("разбор фразы: что передают и сколько") {
      assertTrue(ChatCommand.transferQuery("Передать расколотый череп 2 штук").contains("расколотый череп 2 штук")) &&
      assertTrue(ChatCommand.transferQuery("привет").isEmpty) &&
      assertTrue(ChatCommand.count("расколотый череп 2 штук") == 2) &&
      assertTrue(ChatCommand.count("меч х3") == 3 && ChatCommand.count("меч") == 1) &&
      assertTrue(ChatCommand.itemQuery("расколотый череп 2 штук") == "расколотый череп") &&
      assertTrue(ChatCommand.matches("череп", "Расколотый череп")) &&
      assertTrue(ChatCommand.matches("меч", "⚔ [Ур.3] Меч") && !ChatCommand.matches("щит", "Меч")) &&
      // пустой запрос подходит всему: игрок выберет вещь кнопкой
      assertTrue(ChatCommand.matches("", "Что угодно"))
    },

    test("список показывает только подходящее, одинаковые названия — разными кнопками") {
      val two = List(sword(1L).copy(attack = 12), sword(2L).copy(attack = 9), skull(3L))
      for {
        t <- transfer(two, query = "меч")
        (state, _, _, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(ids.contains("Give_1") && ids.contains("Give_2") && !ids.contains("Give_3")) &&
              assertTrue(screens.last.text.contains("Пётр"))
    },

    test("выбор и подтверждение: вещи уходят посылками, получателю письмо") {
      val skulls = (1L to 5L).toList.map(skull)
      for {
        t <- transfer(skulls, query = "череп", count = 2)
        (state, inv, parcelDao, _, players, renderer) = t
        _       <- state.action(testUser, tap("Give_1"), renderer)
        confirm <- renderer.sentScreens.map(_.last)
        next    <- state.action(testUser, tap("TransferYes"), renderer)
      } yield assertTrue(confirm.inline && confirm.text.contains("×2")) &&
              // ушли ровно две из пяти
              assertTrue(inv.snapshot.map(_.id) == List(3L, 4L, 5L)) &&
              assertTrue(parcelDao.snapshot.map(_.heroId).distinct == List(friend)) &&
              assertTrue(parcelDao.snapshot.size == 2) &&
              assertTrue(players.sentLetters.map(_._1) == List(friendUs)) &&
              assertTrue(players.sentLetters.head._2.contains("Расколотый череп")) &&
              // возвращаемся туда, откуда пришли
              assertTrue(next == StateType.GlobalMap)
    },

    test("посылка сама ложится в банковскую ячейку получателя") {
      for {
        heroDao  <- TestHeroDao.withHero(friendUs, TestFixtures.hero(friendUs).copy(id = friend))
        parcelDao = TestParcelDao.empty
        bank      = TestBankRepository.withCells(1)
        content  <- ZIO.attempt(SceneContent.load())
        renderer <- TestRenderer.make
        parcels   = Parcels(parcelDao, bank, content)
        hero     <- heroDao.getHeroByUserId(friendUs).map(_.get)
        _        <- parcels.send(friend, "Иван", sword(10L), 0L)
        _        <- parcels.deliver(friendUser, hero, renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(bank.itemsSnapshot.map(_.id) == List(10L)) &&
              assertTrue(parcelDao.snapshot.isEmpty) &&
              assertTrue(screens.last.text.contains("банковскую ячейку"))
    },

    test("ячейки нет или она полна — посылка ждёт на почте") {
      for {
        heroDao  <- TestHeroDao.withHero(friendUs, TestFixtures.hero(friendUs).copy(id = friend))
        parcelDao = TestParcelDao.empty
        bank      = TestBankRepository.empty            // ячейка не куплена
        content  <- ZIO.attempt(SceneContent.load())
        renderer <- TestRenderer.make
        parcels   = Parcels(parcelDao, bank, content)
        hero     <- heroDao.getHeroByUserId(friendUs).map(_.get)
        _        <- parcels.send(friend, "Иван", sword(10L), 0L)
        _        <- parcels.deliver(friendUser, hero, renderer)
        screens  <- renderer.sentScreens
        waiting  <- parcels.waitingCount(friend)
      } yield assertTrue(waiting == 1L && bank.itemsSnapshot.isEmpty) &&
              // молча: ничего не выдали — и говорить не о чем
              assertTrue(screens.isEmpty)
    },

    test("почта: забрать одну и забрать всё, что влезет") {
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        parcelDao = TestParcelDao.empty
        content  <- ZIO.attempt(SceneContent.load())
        renderer <- TestRenderer.make
        parcels   = Parcels(parcelDao, TestBankRepository.empty, content)
        invRepo   = TestInventoryRepository.accepting
        state     = MailState(heroDao, invRepo, parcels, content)
        _        <- parcels.send(heroId, "Иван", sword(10L), 0L)
        _        <- parcels.send(heroId, "Иван", sword(11L), 0L)
        _        <- state.enter(testUser, renderer)
        list     <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap("MailTake", "id" -> "1"), renderer)
        afterOne  = (invRepo.snapshot.map(_.id), parcelDao.snapshot.size)
        _        <- state.action(testUser, tap("MailAll"), renderer)
      } yield assertTrue(list.choices.map(_.id).count(_ == "MailTake") == 2) &&
              assertTrue(afterOne == (List(10L), 1)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(10L, 11L) && parcelDao.snapshot.isEmpty)
    },

    test("в Торговом доме почта видна, только когда на ней что-то есть") {
      import pangea.service.state.states.bank.TradeHouseState
      def house(withMail: Boolean) =
        for {
          heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
          parcelDao = TestParcelDao.empty
          content  <- ZIO.attempt(SceneContent.load())
          renderer <- TestRenderer.make
          bank      = TestBankRepository.withCells(1)
          parcels   = Parcels(parcelDao, bank, content)
          _        <- ZIO.when(withMail)(parcels.send(heroId, "Иван", sword(10L), 0L).unit)
          state     = TradeHouseState(heroDao, bank, parcels, content)
          _        <- state.enter(testUser, renderer)
          screens  <- renderer.sentScreens
        } yield screens.last.choices.map(_.id)
      for {
        without <- house(withMail = false)
        with_   <- house(withMail = true)
      } yield assertTrue(!without.contains("Mail")) && assertTrue(with_.contains("Mail")) &&
              assertTrue(HeroArtifacts.MaxTier == 4)   // артефакты на месте, ничего не поехало
    }
  )
}
