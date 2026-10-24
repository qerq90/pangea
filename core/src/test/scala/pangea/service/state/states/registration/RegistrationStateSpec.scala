package pangea.service.state.states.registration

import pangea.engine.SceneContent
import pangea.model.hero.LoreData
import pangea.model.item.{BeltSlot, FlaskKind, Grip, ItemDetails, ItemType, Rarity}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestJournal, TestPlayers, TestRenderer}
import zio.test._
import zio.ZIO

object RegistrationStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(actionId: String): UserAction =
    UserAction("", Some(s"""{"action":"$actionId"}"""))

  private def raceDescription(race: Race): UserAction =
    UserAction(race.entryName, Some("""{"action":"RaceDescription"}"""))

  private def confirmRace(race: Race): UserAction =
    UserAction("", Some(s"""{"action":"ConfirmRace","race":"${race.entryName}"}"""))

  private def makeState =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.make
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = TestInventoryRepository.accepting
    } yield (RegistrationState(new TestPlayers, heroDao, invRepo, TestItemRepository.make, journal, content), renderer, heroDao, invRepo)

  private def makeStateWithHero =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = TestInventoryRepository.accepting
    } yield (RegistrationState(new TestPlayers, heroDao, invRepo, TestItemRepository.make, journal, content), renderer, heroDao, invRepo)

  /** Пробуждение до последней кнопки: обыск, мысли вслух. */
  private val arrivalPath = List("ArrivalSearch", "Monologue1", "Monologue2", "Monologue3")

  override def spec = suite("RegistrationState")(

    test("неизвестный ввод → приветствие, остаётся в Registration") {
      for {
        quad                        <- makeState
        (state, renderer, _, _)      = quad
        result                      <- state.action(testUser, UserAction("привет", None), renderer)
        screens                     <- renderer.sentScreens
      } yield assertTrue(result == StateType.Registration) &&
              assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.startsWith("Добро пожаловать на Пангею!"))
    },

    test("Race → показывает выбор расы, остаётся в Registration") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        result                 <- state.action(testUser, tap("Race"), renderer)
        screens                <- renderer.sentScreens
      } yield assertTrue(result == StateType.Registration) &&
              assertTrue(screens.nonEmpty)
    },

    test("RaceDescription → показывает описание выбранной расы") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        result                 <- state.action(testUser, raceDescription(Race.Human), renderer)
        screens                <- renderer.sentScreens
      } yield assertTrue(result == StateType.Registration) &&
              assertTrue(screens.head.text == Race.Human.description)
    },

    // ── Пролог ───────────────────────────────────────────────────────────────
    test("«Начать» ведёт в первый пассаж, а не в выбор расы") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        _       <- state.action(testUser, UserAction("привет", None), renderer)
        welcome <- renderer.sentScreens.map(_.last)
        _       <- state.action(testUser, tap("P1_Intro"), renderer)
        intro   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(welcome.choices.map(_.id) == List("P1_Intro")) &&
              assertTrue(intro.text.contains("Всякая жизнь заканчивается смертью")) &&
              assertTrue(intro.choices.map(_.id) == List("P2_Dagger"))
    },

    test("«Кинжал в спину»: четыре смерти, подписи без подсказок о расе и не длиннее 40 знаков") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        _      <- state.action(testUser, tap("P2_Dagger"), renderer)
        dagger <- renderer.sentScreens.map(_.last)
      } yield assertTrue(dagger.choices.map(_.id).toSet ==
                Set("Pick_Gnome", "Pick_Orc", "Pick_Elf", "Pick_Human")) &&
              assertTrue(dagger.choices.forall(_.label.length <= 40)) &&
              assertTrue(!dagger.choices.exists(_.label.toLowerCase.contains("выбрать")))
    },

    test("выбранная смерть запоминается, и «Всё равно помереть» будит в нужном теле") {
      def wakeFor(pick: String) =
        for {
          quad                   <- makeState
          (state, renderer, _, _) = quad
          _        <- state.action(testUser, tap(pick), renderer)
          _        <- state.action(testUser, tap("Remember"), renderer)
          remember <- renderer.sentScreens.map(_.last)
        } yield remember.choices.map(_.id)
      for {
        gnome <- wakeFor("Pick_Gnome")
        orc   <- wakeFor("Pick_Orc")
        elf   <- wakeFor("Pick_Elf")
        human <- wakeFor("Pick_Human")
      } yield assertTrue(gnome == List("Wake_Gnome")) &&
              assertTrue(orc   == List("Wake_Orc")) &&
              assertTrue(elf   == List("Wake_Elf")) &&
              assertTrue(human == List("Wake_Human"))
    },

    test("ветка орка доходит до портала, где две кнопки: выбрать расу или прожить заново") {
      val path = List("P1_Intro", "P2_Dagger", "Pick_Orc", "Remember", "Wake_Orc", "Orc_Face",
                      "Orc_Castle", "Orc_Sounds", "Orc_Recruit", "Orc_Stump", "Orc_Traitors", "Portal")
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        results <- ZIO.foreach(path)(id => state.action(testUser, tap(id), renderer))
        portal  <- renderer.sentScreens.map(_.last)
      } yield assertTrue(results.forall(_ == StateType.Registration)) &&
              assertTrue(portal.text.contains("Портал")) &&
              assertTrue(portal.choices.map(_.id) == List("Race", "P1_Intro"))
    },

    test("каждая из четырёх веток и каждая её развилка доходит до портала") {
      // Все кнопки всех битов ведут на существующие биты, и из любого бита есть
      // путь в Portal — тупиков в истории нет.
      for {
        content <- ZIO.attempt(SceneContent.load())
        beats    = content.beats("registration.prologue").toMap
        graph   <- ZIO.foreach(beats.toList) { case (key, beat) =>
                     beat.buildChoices(testUser).map(cs => key -> cs.map(_.id))
                   }
      } yield {
        // «Remember» в yaml без кнопок: единственную кнопку по ветке строит код.
        // Для обхода графа подставляем все четыре пробуждения.
        val wakes = List("Wake_Gnome", "Wake_Orc", "Wake_Elf", "Wake_Human")
        val edges = graph.toMap.updated("Remember", wakes)
        val known = edges.keySet ++ Set("Race")
        def reaches(from: String, seen: Set[String] = Set.empty): Boolean =
          from == "Portal" || (!seen(from) && edges.getOrElse(from, Nil).exists(reaches(_, seen + from)))
        assertTrue(edges.values.flatten.forall(known.contains)) &&
        assertTrue(edges.keys.filter(_ != "Portal").forall(k => reaches(k)))
      }
    },

    test("назад ходить нельзя: ни один бит не ведёт в уже пройденный, кроме «прожить заново»") {
      for {
        content <- ZIO.attempt(SceneContent.load())
        beats    = content.beats("registration.prologue").toMap
        graph   <- ZIO.foreach(beats.toList) { case (key, beat) =>
                     beat.buildChoices(testUser).map(cs => key -> cs.map(_.id).filter(_ != "P1_Intro"))
                   }
      } yield {
        val wakes = List("Wake_Gnome", "Wake_Orc", "Wake_Elf", "Wake_Human")
        val edges = graph.toMap.updated("Remember", wakes)
        def cyclic(node: String, stack: Set[String]): Boolean =
          stack(node) || edges.getOrElse(node, Nil).exists(cyclic(_, stack + node))
        assertTrue(!edges.keys.exists(k => cyclic(k, Set.empty)))
      }
    },

    test("«Прожить это заново» с портала возвращает к самому первому пассажу") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        _     <- state.action(testUser, tap("Portal"), renderer)
        _     <- state.action(testUser, tap("P1_Intro"), renderer)
        intro <- renderer.sentScreens.map(_.last)
      } yield assertTrue(intro.text.contains("Всякая жизнь заканчивается смертью"))
    },

    test("после портала раса выбирается из всех восьми смертных, с описаниями") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        _      <- state.action(testUser, tap("Race"), renderer)
        select <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, raceDescription(Race.Khajiit), renderer)
        desc   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(select.choices.size == Race.mortals.size) &&
              assertTrue(select.choices.map(_.label).toSet == Race.mortals.map(_.toString).toSet) &&
              assertTrue(desc.text == Race.Khajiit.description) &&
              assertTrue(desc.choices.map(_.id) == List("ConfirmRace", "Race"))
    },

    test("подтверждение расы: раса сохранена, а герой падает на крышу сторожки — пока с пустыми руками") {
      for {
        quad                           <- makeStateWithHero
        (state, renderer, heroDao, invRepo) = quad
        _       <- state.action(testUser, tap("Pick_Elf"), renderer) // в сцене лежит ветка
        result  <- state.action(testUser, confirmRace(Race.Demon), renderer)
        snap    <- heroDao.raceSnapshot
        lore    <- heroDao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption))
        screen  <- renderer.sentScreens.map(_.last)
      } yield assertTrue(result == StateType.Registration) &&
              // раса — та, что выбрана в конце, а не та, чью ветку прошли
              assertTrue(snap.get(testUser.userId).contains(Race.Demon)) &&
              // а ветка пролога остаётся с героем навсегда — сюжету пригодится
              assertTrue(lore.exists(_.prologueBranch.contains("Elf"))) &&
              assertTrue(screen.text.contains("соломенная крыша") && screen.choices.map(_.id) == List("ArrivalSearch")) &&
              // старого снаряжения новобранца больше нет
              assertTrue(invRepo.snapshot.isEmpty)
    },

    test("обыск сторожки: меч и щит в руках, пояс с флягой целителя на четыре глотка, топор и лук или арбалет в сумке — и только раз") {
      for {
        quad                           <- makeStateWithHero
        (state, renderer, heroDao, invRepo) = quad
        _      <- state.action(testUser, confirmRace(Race.Human), renderer)
        _      <- state.action(testUser, tap("ArrivalSearch"), renderer)
        first  <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("ArrivalSearch"), renderer) // ВК прислал кнопку дважды
        hero   <- heroDao.getHeroByUserId(userId).map(_.get)
        screen <- renderer.sentScreens.map(_.last)
      } yield {
        val eq    = hero.equipment
        val slots = eq.belt.details match { case b: ItemDetails.Belt => b.slots; case _ => Nil }
        val gear  = List(eq.weapon, eq.leftHand, eq.belt) ++ invRepo.snapshot
        val ranged = invRepo.snapshot.filter(_.ranged)
        val noun   = ranged.headOption.map(_.name.split(" ")(2)).getOrElse("")
        val shot   = ranged.flatMap(_.activeSkill).forall(s => pangea.model.skill.Skill.rangedSkills.contains(s))
        assertTrue(eq.weapon.name == "⚪ Простой Меч Искателя" && eq.weapon.grip.contains(Grip.OneHanded)) &&
        assertTrue(eq.leftHand.name == "⚪ Простой Круглый Щит Искателя" && eq.leftHand.itemType == ItemType.Shield) &&
        assertTrue(eq.belt.name == "⚪ Простой Пояс Искателя") &&
        assertTrue(slots == List(BeltSlot.Flask(FlaskKind.Healer, Rarity.White, 4))) &&
        assertTrue(invRepo.snapshot.map(_.name).head == "⚪ Простой Двуручный Топор Искателя") &&
        // и лук или арбалет — со своим умением стрелка
        assertTrue(ranged.size == 1 && (noun == "Лук" || noun == "Арбалет")) &&
        assertTrue(shot) &&
        assertTrue(invRepo.snapshot.size == 2 && invRepo.snapshot.forall(_.twoHanded)) &&
        // сцена называет то, что лежит в сумке, — и при повторе тоже
        assertTrue(first.text.contains(s"Простой $noun Искателя") && screen.text.contains(s"Простой $noun Искателя")) &&
        assertTrue(!screen.text.contains("{ranged")) &&
        assertTrue(gear.forall(i => i.rarity == Rarity.White && i.lvl == 1L)) &&
        // защита щита легла в боевые статы
        assertTrue(hero.fightStats.defence >= TestFixtures.hero(userId).fightStats.defence + 3L) &&
        assertTrue(screen.choices.map(_.id) == List("Monologue1"))
      }
    },

    test("мысли вслух и «Встать»: задание «Встать на ноги» с тремя пунктами — и герой в лабиринте") {
      for {
        quad                         <- makeStateWithHero
        (state, renderer, heroDao, _) = quad
        _      <- state.action(testUser, confirmRace(Race.Human), renderer)
        _      <- ZIO.foreach(arrivalPath)(id => state.action(testUser, tap(id), renderer))
        beat3  <- renderer.sentScreens.map(_.last)
        result <- state.action(testUser, tap("ArrivalStand"), renderer)
        shown  <- renderer.sentScreens
        quest   = shown.init.last
        guide   = shown.last
        lore   <- heroDao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption))
        scene  <- heroDao.readSceneData(userId)
      } yield assertTrue(beat3.choices.map(_.id) == List("ArrivalStand")) &&
              assertTrue(result == StateType.Dungeon) &&
              // следом за заданием — туториал и приглашение в общую беседу, без кнопок
              assertTrue(guide.text.contains(pangea.service.chat.Links.Tutorial) &&
                guide.text.contains(pangea.service.chat.Links.Chat) && guide.text.contains("«Туториал»")) &&
              assertTrue(guide.choices.isEmpty && !guide.text.contains("{")) &&
              assertTrue(quest.text.contains("📜 Новое задание: «Встать на ноги»")) &&
              assertTrue(quest.text.contains("⬜ Окрепнуть — дойти до 5 уровня")) &&
              assertTrue(quest.text.contains("⬜ Обзавестись логовом — купить дом")) &&
              assertTrue(quest.text.contains("горожан (0/5)")) &&
              assertTrue(lore.exists(_.firstSteps)) &&
              // сцена регистрации после выхода не тянется за героем
              assertTrue(scene.contains(io.circe.Json.Null))
    },

    test("полный флоу: пролог гнома → портал → раса → сторожка → лабиринт") {
      val path = List("P1_Intro", "P2_Dagger", "Pick_Gnome", "Remember", "Wake_Gnome", "Gnome_Shafts",
                      "Gnome_Sass", "Gnome_Obey", "Gnome_Listen", "Portal", "Race")
      for {
        quad                            <- makeStateWithHero
        (state, renderer, heroDao, _)    = quad
        _      <- ZIO.foreach(path)(id => state.action(testUser, tap(id), renderer))
        _      <- state.action(testUser, raceDescription(Race.Human), renderer)
        _      <- state.action(testUser, confirmRace(Race.Human), renderer)
        _      <- ZIO.foreach(arrivalPath)(id => state.action(testUser, tap(id), renderer))
        result <- state.action(testUser, tap("ArrivalStand"), renderer)
        snap   <- heroDao.raceSnapshot
        lore   <- heroDao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption))
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(snap.get(testUser.userId).contains(Race.Human)) &&
              assertTrue(lore.exists(_.prologueBranch.contains("Gnome")))
    },

    test("«прожить заново» перезаписывает ветку: в финале остаётся последняя") {
      for {
        quad                         <- makeStateWithHero
        (state, renderer, heroDao, _) = quad
        _      <- state.action(testUser, tap("Pick_Orc"), renderer)
        _      <- state.action(testUser, tap("P1_Intro"), renderer)
        _      <- state.action(testUser, tap("Pick_Human"), renderer)
        _      <- state.action(testUser, confirmRace(Race.Elf), renderer)
        lore   <- heroDao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption))
      } yield assertTrue(lore.exists(_.prologueBranch.contains("Human")))
    }
  )
}
