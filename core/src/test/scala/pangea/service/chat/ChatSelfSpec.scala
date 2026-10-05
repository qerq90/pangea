package pangea.service.chat

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.states.EquipmentState
import pangea.service.state.{PlayerLock, State, StateHandler}
import pangea.test._
import zio.ZIO
import zio.test._

/** «Мой профиль» и «Моё снаряжение» в общей беседе: ответ уходит туда же, в
  * беседу, и показывает ровно то, что игрок видит у себя, — профиль целиком,
  * а снаряжение без характеристик. */
object ChatSelfSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("42")
  private val testUser = User(userId, vkId, TelegramId("tg"))

  private def blade: Item =
    Item(1L, "🔵 Меч Рыцаря", lvl = 7L, Rarity.Blue, ItemType.Weapon,
      attack = 13, accuracy = 4, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def heroWith(item: Option[Item]): Hero = {
    val base = TestFixtures.hero(userId).copy(lvl = 7L)
    base.copy(equipment = item.fold(TestFixtures.emptyEquipment)(i =>
      TestFixtures.emptyEquipment.copy(weapon = i)))
  }

  private def handler(h: Hero) =
    for {
      heroDao   <- TestHeroDao.withHero(userId, h)
      heroRepo  <- TestHeroRepository.withHero(userId, h)
      userRepo  <- TestUserRepository.withUser(testUser)
      api       <- TestApi.make
      content   <- ZIO.attempt(SceneContent.load())
      lock      <- PlayerLock.make
      payouts    = Payouts(TestPayoutDao.empty, heroDao, content)
      parcels    = Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers  = Transfers(TestInventoryRepository.accepting, userRepo, parcels, new TestPlayers, content)
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers,
               Map.empty[StateType, State], AdminPanel.disabled, content, lock), api, content)

  override def spec = suite("Команды о себе в беседе")(

    test("команду узнают в любом написании, а чужой текст не трогают") {
      assertTrue(ChatCommand.selfCommand("Мой профиль").contains(ChatCommand.Self.Profile)) &&
      assertTrue(ChatCommand.selfCommand("  мой   профиль  ").contains(ChatCommand.Self.Profile)) &&
      // «ё» набирают и так и этак, а восклицательный знак делу не мешает
      assertTrue(ChatCommand.selfCommand("Моё снаряжение").contains(ChatCommand.Self.Gear)) &&
      assertTrue(ChatCommand.selfCommand("мое снаряжение!").contains(ChatCommand.Self.Gear)) &&
      // а вот это уже не команда, а разговор
      assertTrue(ChatCommand.selfCommand("мой профиль друга").isEmpty) &&
      assertTrue(ChatCommand.selfCommand("передать меч").isEmpty) &&
      assertTrue(ChatCommand.selfCommand("").isEmpty)
    },

    test("о себе рассказывают откуда угодно, не только из города") {
      // Город кончается за воротами, а беседа — нет: в бою, в лабиринте и на
      // дне канализации «Моё снаряжение» отвечает так же, как на площади.
      val outside = List(StateType.Battle, StateType.Dungeon, StateType.MonsterCave,
                         StateType.QuestRoad, StateType.Thieves, StateType.Loot)
      ZIO.foreach(outside) { where =>
        for {
          t <- handler(heroWith(Some(blade)).copy(state = where))
          (state, api, _) = t
          _    <- state.selfToChat(vkId, ChatCommand.Self.Gear, eventId = 77L)
          gear <- api.chatMessages
          _    <- state.selfToChat(vkId, ChatCommand.Self.Profile, eventId = 78L)
          both <- api.chatMessages
        } yield (where, gear, both)
      }.map { results =>
        assertTrue(results.forall { case (_, gear, _) => gear.exists(_.contains("снаряжение")) }) &&
        assertTrue(results.forall { case (_, gear, _) => gear.exists(_.contains(blade.displayTitle)) }) &&
        assertTrue(results.forall { case (_, _, both) => both.sizeIs == 2 }) &&
        // ни одно из этих состояний городским не считается — и это неважно
        assertTrue(outside.forall(!StateType.cityStates.contains(_)))
      }
    },

    test("«Мой профиль» уходит в беседу и несёт то же, что экран «Персонаж»") {
      val h = heroWith(None)
      for {
        t <- handler(h)
        (state, api, content) = t
        _    <- state.selfToChat(vkId, ChatCommand.Self.Profile, eventId = 7L)
        said <- api.chatMessages
        dm   <- api.sentMessages
      } yield assertTrue(said.size == 1 && dm.isEmpty) &&
              assertTrue(said.head.startsWith(ChatProfile.profileHeader("Иван Иванов"))) &&
              // ровно то же тело, что и у кнопки: характеристики, опыт, очки
              assertTrue(said.head.contains(h.getInfo(0L))) &&
              assertTrue(ChatProfile.profile("Иван Иванов", h, 0L, blessed = false, 0, content) ==
                         ChatProfile.profileHeader("Иван Иванов") + "\n" + h.getInfo(0L))
    },

    test("«Моё снаряжение» перечисляет все слоты и молчит о характеристиках") {
      val h = heroWith(Some(blade))
      for {
        t <- handler(h)
        (state, api, _) = t
        _    <- state.selfToChat(vkId, ChatCommand.Self.Gear, eventId = 8L)
        said <- api.chatMessages
      } yield assertTrue(said.size == 1) &&
              assertTrue(said.head.startsWith(ChatProfile.gearHeader("Иван Иванов"))) &&
              // все четырнадцать слотов на месте, каждый своей строкой
              assertTrue(EquipmentState.slots.forall(s => said.head.contains(s"${s.name}: "))) &&
              assertTrue(said.head.linesIterator.size == EquipmentState.slots.size + 1) &&
              // надетое названо, пустое помечено прочерком
              assertTrue(said.head.contains(s"Оружие: ${blade.displayTitle}")) &&
              assertTrue(said.head.contains(s"Шлем: ${ChatProfile.Empty}")) &&
              // а характеристик нет: беседе знать их незачем
              assertTrue(!said.head.contains("⚔") && !said.head.contains("🎯"))
    },

    test("повтор того же события и чужак в беседе остаются без ответа") {
      for {
        t <- handler(heroWith(None))
        (state, api, _) = t
        _      <- state.selfToChat(vkId, ChatCommand.Self.Gear, eventId = 9L)
        _      <- state.selfToChat(vkId, ChatCommand.Self.Gear, eventId = 9L)
        twice  <- api.chatMessages
        // писал не игрок — беседа не место для сообщений об ошибках
        _      <- state.selfToChat(VkId("777"), ChatCommand.Self.Profile, eventId = 10L)
        after  <- api.chatMessages
      } yield assertTrue(twice.size == 1 && after.size == 1)
    }
  )
}
