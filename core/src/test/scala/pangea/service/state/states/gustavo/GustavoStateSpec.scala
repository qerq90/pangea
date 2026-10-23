package pangea.service.state.states.gustavo

import io.circe.syntax.EncoderOps
import pangea.engine.{ChoiceColor, SceneContent}
import pangea.model.item.{BeltSlot, FlaskKind, Item, ItemDetails, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.{ParamsBuff, StatBoost, StatBoosts}
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object GustavoStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def tapStat(key: String, stat: String): UserAction =
    UserAction("", Some(s"""{"action":"$key","stat":"$stat"}"""))

  // lvl 10 → цена зелья = 10 × 100 = 1000
  private def hero(silver: Long = 5000L, traumaUntil: Option[Long] = None, traumaNames: List[String] = Nil) =
    TestFixtures.hero(userId).copy(lvl = 10L, silver = silver, traumaUntil = traumaUntil, traumaNames = traumaNames)

  private val farFuture = 1_000_000_000L
  private def hurtHero  = hero(traumaUntil = Some(farFuture), traumaNames = List(Trauma.SmashedFinger.name))

  /** Пояс с одной флягой в ячейке: столько зарядов в ней и осталось. Серый
    * запас фляги целителя — три глотка, как раз для тестов пополнения. */
  private def belt(charges: Int, maxCharges: Int): Item = {
    val rarity = Rarity.values.find(r => FlaskKind.chargesFor(r) == maxCharges).getOrElse(Rarity.Gray)
    Item.NoItem.copy(name = "Пояс", itemType = ItemType.Belt, rarity = Rarity.Blue,
      details = ItemDetails.Belt(List(BeltSlot.Flask(FlaskKind.Healer, rarity, charges))))
  }

  /** Сколько зарядов у фляги в первой ячейке пояса. */
  private def beltCharges(i: Item): Option[Int] = i.details match {
    case b: ItemDetails.Belt => b.slots.collectFirst { case f: BeltSlot.Flask => f.charges }
    case _                   => None
  }
  private def heroWithBelt(b: Item, silver: Long = 5000L) =
    hero(silver = silver).copy(equipment = TestFixtures.emptyEquipment.copy(belt = b))

  private def env(h: pangea.model.hero.Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, h)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (heroDao, renderer, content)

  override def spec = suite("Gustavo")(

    // ── Меню ────────────────────────────────────────────────────────────────
    suite("GustavoState (меню)")(

      test("enter → зелёная кнопка лечения, баф, травы, пояс, задание, Назад") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoState(heroDao, TestInventoryRepository.accepting, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          ids      = screens.last.choices.map(_.id)
          heal     = screens.last.choices.find(_.id == "Heal")
        } yield assertTrue(heal.exists(_.color == ChoiceColor.Positive)) &&
                assertTrue(ids == List("Heal", "Boost", "Herbs", "HerbsTalk", "Belt", "GusQuest",
                  "GustavoDaily", "Back", "GoToCity")) &&
                // «Пополнить припасы» больше нет: фляги живут в поясе
                assertTrue(screens.last.choices.exists(c => c.id == "Belt" && c.label == "Пополнить пояс"))
      },

      test("«В город» уводит прямо на площадь — и от прилавка, и с предложения зелья") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          menu   <- GustavoState(heroDao, TestInventoryRepository.accepting, content)
                      .action(testUser, tap("GoToCity"), renderer)
          heal   <- GustavoHealState(heroDao, content).action(testUser, tap("GoToCity"), renderer)
          boost  <- GustavoBoostState(heroDao, content).action(testUser, tap("GoToCity"), renderer)
        } yield assertTrue(menu == StateType.GlobalMap && heal == StateType.GlobalMap) &&
                assertTrue(boost == StateType.GlobalMap)
      },

      test("Herbs → заглушка, остаёмся в меню; «Пополнить пояс» → GustavoBelt") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          state = GustavoState(heroDao, TestInventoryRepository.accepting, content)
          herbs   <- state.action(testUser, tap("Herbs"), renderer)
          belt    <- state.action(testUser, tap("Belt"), renderer)
          // кнопка из старого меню «Пополнить припасы» ведёт туда же
          stale   <- state.action(testUser, tap("Supplies"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(herbs == StateType.Gustavo) &&
                assertTrue(belt == StateType.GustavoBelt && stale == StateType.GustavoBelt) &&
                assertTrue(screens.exists(_.text.contains("трав у тебя нет")))
      },

      test("кнопка лечения краснеет на кулдауне") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- heroDao.writeGustavoData(userId, GustavoData(Some(farFuture), Nil).asJson)
          _       <- GustavoState(heroDao, TestInventoryRepository.accepting, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          heal     = screens.last.choices.find(_.id == "Heal")
        } yield assertTrue(heal.exists(_.color == ChoiceColor.Negative))
      },

      test("Heal → GustavoHeal, Boost → GustavoBoost, Back → к уличным торговцам") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          state = GustavoState(heroDao, TestInventoryRepository.accepting, content)
          h    <- state.action(testUser, tap("Heal"), renderer)
          b    <- state.action(testUser, tap("Boost"), renderer)
          back <- state.action(testUser, tap("Back"), renderer)
        } yield assertTrue(h == StateType.GustavoHeal) &&
                assertTrue(b == StateType.GustavoBoost) &&
                assertTrue(back == StateType.StreetMerchants)
      }
    ),

    // ── Лечение травм ─────────────────────────────────────────────────────────
    suite("GustavoHealState")(

      test("enter → предложение зелья с ценой и кнопками Купить/Назад") {
        for {
          t <- env(hurtHero)
          (heroDao, renderer, content) = t
          _       <- GustavoHealState(heroDao, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          ids      = screens.last.choices.map(_.id).toSet
        } yield assertTrue(screens.last.text.contains("1000")) &&
                assertTrue(ids == Set("BuyPotion", "Back", "GoToCity"))
      },

      test("BuyPotion с травмой → снимает травму, списывает цену, ставит кулдаун, уходит в меню") {
        for {
          t <- env(hurtHero)
          (heroDao, renderer, content) = t
          st      <- GustavoHealState(heroDao, content).action(testUser, tap("BuyPotion"), renderer)
          h       <- heroDao.getHeroByUserId(userId)
          data    <- heroDao.readGustavoData(userId).map(_.flatMap(_.as[GustavoData].toOption))
          screens <- renderer.sentScreens
        } yield assertTrue(st == StateType.Gustavo) &&
                assertTrue(h.exists(_.traumaNames.isEmpty)) &&
                assertTrue(h.exists(_.silver == 4000L)) &&
                assertTrue(data.exists(_.healCooldownUntil.isDefined)) &&
                assertTrue(screens.exists(_.text.contains("Исцелена травма")))
      },

      test("BuyPotion без активных травм → «шутник», без списаний и кулдауна") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoHealState(heroDao, content).action(testUser, tap("BuyPotion"), renderer)
          h       <- heroDao.getHeroByUserId(userId)
          data    <- heroDao.readGustavoData(userId)
          screens <- renderer.sentScreens
        } yield assertTrue(h.exists(_.silver == 5000L)) &&
                assertTrue(data.isEmpty) &&
                assertTrue(screens.exists(_.text.contains("шутник")))
      },

      test("BuyPotion на кулдауне → сообщение про час, серебро не списано") {
        for {
          t <- env(hurtHero)
          (heroDao, renderer, content) = t
          _       <- heroDao.writeGustavoData(userId, GustavoData(Some(farFuture), Nil).asJson)
          _       <- GustavoHealState(heroDao, content).action(testUser, tap("BuyPotion"), renderer)
          h       <- heroDao.getHeroByUserId(userId)
          screens <- renderer.sentScreens
        } yield assertTrue(h.exists(_.silver == 5000L)) &&
                assertTrue(h.exists(_.traumaNames.nonEmpty)) &&
                assertTrue(screens.exists(_.text.contains("раз в час")))
      },

      test("Back → GustavoState") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          st <- GustavoHealState(heroDao, content).action(testUser, tap("Back"), renderer)
        } yield assertTrue(st == StateType.Gustavo)
      }
    ),

    // ── Увеличение характеристик ───────────────────────────────────────────────
    suite("GustavoBoostState")(

      test("enter → 4 кнопки характеристик (первая бесплатная — зелёная) + Назад") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoBoostState(heroDao, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          ids      = screens.last.choices.map(_.id)
          str      = screens.last.choices.find(_.data.get("stat").contains("str"))
        } yield assertTrue(ids == List("BoostBuy", "BoostBuy", "BoostBuy", "BoostBuy", "Back", "GoToCity")) &&
                assertTrue(str.exists(_.color == ChoiceColor.Positive))
      },

      test("BoostBuy str (первое, бесплатно) → баф активен, серебро не списано, +15% СИЛ") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _    <- GustavoBoostState(heroDao, content).action(testUser, tapStat("BoostBuy", "str"), renderer)
          h    <- heroDao.getHeroByUserId(userId).map(_.get)
          data <- heroDao.readGustavoData(userId).map(_.flatMap(_.as[GustavoData].toOption))
        } yield assertTrue(h.silver == 5000L) &&
                assertTrue(h.statBoosts.hasActive("gustavo:str", 0L)) &&
                assertTrue(data.exists(_.freeBoostsUsed.contains("str"))) &&
                assertTrue(h.effectiveBaseStats(0L).str == 11L) // 10 × 1.15 = 11
      },

      test("BoostBuy str после израсходованного бесплатного → списывает 1000") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _ <- heroDao.writeGustavoData(userId, GustavoData(None, List("str")).asJson)
          _ <- GustavoBoostState(heroDao, content).action(testUser, tapStat("BoostBuy", "str"), renderer)
          h <- heroDao.getHeroByUserId(userId).map(_.get)
        } yield assertTrue(h.silver == 4000L) &&
                assertTrue(h.statBoosts.hasActive("gustavo:str", 0L))
      },

      test("BoostBuy str пока баф активен → «готовит новое», без списаний") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- heroDao.updateStatBoosts(userId,
                       StatBoosts(List(StatBoost("gustavo:str", ParamsBuff(15, 0, 0, 0), farFuture))))
          _       <- heroDao.writeGustavoData(userId, GustavoData(None, List("str")).asJson)
          _       <- GustavoBoostState(heroDao, content).action(testUser, tapStat("BoostBuy", "str"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.silver == 5000L) &&
                assertTrue(screens.exists(_.text.contains("готовится")))
      },

      test("BoostBuy платное без серебра → сообщение, баф не выдан") {
        for {
          t <- env(hero(silver = 0L))
          (heroDao, renderer, content) = t
          _       <- heroDao.writeGustavoData(userId, GustavoData(None, List("str")).asJson)
          _       <- GustavoBoostState(heroDao, content).action(testUser, tapStat("BoostBuy", "str"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.silver == 0L) &&
                assertTrue(!h.statBoosts.hasActive("gustavo:str", 0L)) &&
                assertTrue(screens.exists(_.text.contains("Столько серебра нет")))
      },

      test("Back → GustavoState") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          st <- GustavoBoostState(heroDao, content).action(testUser, tap("Back"), renderer)
        } yield assertTrue(st == StateType.Gustavo)
      }
    ),

    // ── Пополнить припасы ──────────────────────────────────────────────────────
    suite("GustavoSuppliesState (меню)")(

      test("enter → 3 кнопки: Фляга, Пояс, Назад") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoSuppliesState(heroDao, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          ids      = screens.last.choices.map(_.id)
        } yield assertTrue(ids == List("Belt", "Back", "GoToCity"))
      },

      test("Belt → GustavoBelt; Back → Gustavo") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          state = GustavoSuppliesState(heroDao, content)
          beltR   <- state.action(testUser, tap("Belt"), renderer)
          backR   <- state.action(testUser, tap("Back"), renderer)
        } yield assertTrue(beltR == StateType.GustavoBelt) &&
                assertTrue(backR == StateType.Gustavo)
      }
    ),

    // ── Пополнение пояса ───────────────────────────────────────────────────────
    suite("GustavoBeltState")(

      test("enter с неполным поясом → предложение с ценой (2 бутыли × 100 = 200) и Купить/Назад") {
        for {
          t <- env(heroWithBelt(belt(1, 3)))
          (heroDao, renderer, content) = t
          _       <- GustavoBeltState(heroDao, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          ids      = screens.last.choices.map(_.id).toSet
        } yield assertTrue(screens.last.text.contains("200")) &&
                assertTrue(ids == Set("Refill", "Back"))
      },

      test("Refill → пополняет бутыли до максимума, списывает 200, возвращает к Густаво") {
        for {
          t <- env(heroWithBelt(belt(1, 3)))
          (heroDao, renderer, content) = t
          st      <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(st == StateType.Gustavo) &&
                assertTrue(beltCharges(h.equipment.belt).contains(3)) &&
                assertTrue(h.silver == 4800L) &&
                assertTrue(screens.exists(_.text.contains("забил бутыли")))
      },

      test("Refill без серебра → сообщение, бутыли и серебро не меняются") {
        for {
          t <- env(heroWithBelt(belt(1, 3), silver = 10L))
          (heroDao, renderer, content) = t
          _       <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.silver == 10L) &&
                assertTrue(beltCharges(h.equipment.belt).contains(1)) &&
                assertTrue(screens.exists(_.text.contains("Столько серебра нет")))
      },

      test("Refill с полным поясом → «все полны», серебро не списано") {
        for {
          t <- env(heroWithBelt(belt(3, 3)))
          (heroDao, renderer, content) = t
          _       <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.silver == 5000L) &&
                assertTrue(screens.exists(_.text.contains("полны")))
      },

      test("Refill без пояса → «пояса нет», серебро не списано") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.silver == 5000L) &&
                assertTrue(screens.exists(_.text.contains("пояса-то у тебя и нет")))
      },

      test("Back → в меню Густаво") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          st <- GustavoBeltState(heroDao, content).action(testUser, tap("Back"), renderer)
        } yield assertTrue(st == StateType.Gustavo)
      }
    )
  )
}
