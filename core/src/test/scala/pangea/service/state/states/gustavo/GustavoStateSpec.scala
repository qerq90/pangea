package pangea.service.state.states.gustavo

import io.circe.syntax.EncoderOps
import pangea.engine.{ChoiceColor, SceneContent}
import pangea.model.item.{FlaskEffect, Item, ItemDetails, ItemType, PotionKind}
import pangea.model.state.StateType
import pangea.model.stats.{ParamsBuff, StatBoost, StatBoosts}
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object GustavoStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def tapStat(key: String, stat: String): UserAction =
    UserAction("", Some(s"""{"action":"$key","stat":"$stat"}"""))

  // lvl 10 → цена зелья = 10 × 100 = 1000
  private def hero(gold: Long = 5000L, traumaUntil: Option[Long] = None, traumaNames: List[String] = Nil) =
    TestFixtures.hero(userId).copy(lvl = 10L, gold = gold, traumaUntil = traumaUntil, traumaNames = traumaNames)

  private val farFuture = 1_000_000_000L
  private def hurtHero  = hero(traumaUntil = Some(farFuture), traumaNames = List(Trauma.SmashedFinger.name))

  // фляга с charges/maxCharges для тестов пополнения
  private def flask(charges: Int, maxCharges: Int): Item =
    Item.NoItem.copy(name = "Фляга", itemType = ItemType.Flask,
      details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = charges, maxCharges = maxCharges))

  private def flaskCharges(i: Item): Option[Int] = i.details match {
    case f: ItemDetails.Flask => Some(f.charges)
    case _                    => None
  }
  private def heroWithFlask(f: Item, gold: Long = 5000L) =
    hero(gold = gold).copy(equipment = TestFixtures.emptyEquipment.copy(flask = f))

  // пояс с charges/maxCharges для тестов пополнения
  private def belt(charges: Int, maxCharges: Int): Item =
    Item.NoItem.copy(name = "Пояс", itemType = ItemType.Belt,
      details = ItemDetails.Belt(PotionKind.Healing, charges = charges, maxCharges = maxCharges))

  private def beltCharges(i: Item): Option[Int] = i.details match {
    case b: ItemDetails.Belt => Some(b.charges)
    case _                   => None
  }
  private def heroWithBelt(b: Item, gold: Long = 5000L) =
    hero(gold = gold).copy(equipment = TestFixtures.emptyEquipment.copy(belt = b))

  private def env(h: pangea.model.hero.Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, h)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (heroDao, renderer, content)

  override def spec = suite("Gustavo")(

    // ── Меню ────────────────────────────────────────────────────────────────
    suite("GustavoState (меню)")(

      test("enter → зелёная кнопка лечения, баф, травы, припасы, Назад") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoState(heroDao, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          ids      = screens.last.choices.map(_.id)
          heal     = screens.last.choices.find(_.id == "Heal")
        } yield assertTrue(heal.exists(_.color == ChoiceColor.Positive)) &&
                assertTrue(ids == List("Heal", "Boost", "Herbs", "Supplies", "Back"))
      },

      test("Herbs → заглушка, остаёмся в меню; Supplies → GustavoSupplies") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          state = GustavoState(heroDao, content)
          herbs   <- state.action(testUser, tap("Herbs"), renderer)
          supplies <- state.action(testUser, tap("Supplies"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(herbs == StateType.Gustavo) &&
                assertTrue(supplies == StateType.GustavoSupplies) &&
                assertTrue(screens.exists(_.text.contains("трав у тебя нет")))
      },

      test("кнопка лечения краснеет на кулдауне") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- heroDao.writeGustavoData(userId, GustavoData(Some(farFuture), Nil).asJson)
          _       <- GustavoState(heroDao, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          heal     = screens.last.choices.find(_.id == "Heal")
        } yield assertTrue(heal.exists(_.color == ChoiceColor.Negative))
      },

      test("Heal → GustavoHeal, Boost → GustavoBoost, Back → MarketSquare") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          state = GustavoState(heroDao, content)
          h    <- state.action(testUser, tap("Heal"), renderer)
          b    <- state.action(testUser, tap("Boost"), renderer)
          back <- state.action(testUser, tap("Back"), renderer)
        } yield assertTrue(h == StateType.GustavoHeal) &&
                assertTrue(b == StateType.GustavoBoost) &&
                assertTrue(back == StateType.MarketSquare)
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
                assertTrue(ids == Set("BuyPotion", "Back"))
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
                assertTrue(h.exists(_.gold == 4000L)) &&
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
        } yield assertTrue(h.exists(_.gold == 5000L)) &&
                assertTrue(data.isEmpty) &&
                assertTrue(screens.exists(_.text.contains("шутник")))
      },

      test("BuyPotion на кулдауне → сообщение про 30 минут, золото не списано") {
        for {
          t <- env(hurtHero)
          (heroDao, renderer, content) = t
          _       <- heroDao.writeGustavoData(userId, GustavoData(Some(farFuture), Nil).asJson)
          _       <- GustavoHealState(heroDao, content).action(testUser, tap("BuyPotion"), renderer)
          h       <- heroDao.getHeroByUserId(userId)
          screens <- renderer.sentScreens
        } yield assertTrue(h.exists(_.gold == 5000L)) &&
                assertTrue(h.exists(_.traumaNames.nonEmpty)) &&
                assertTrue(screens.exists(_.text.contains("30 минут")))
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
        } yield assertTrue(ids == List("BoostBuy", "BoostBuy", "BoostBuy", "BoostBuy", "Back")) &&
                assertTrue(str.exists(_.color == ChoiceColor.Positive))
      },

      test("BoostBuy str (первое, бесплатно) → баф активен, золото не списано, +15% СИЛ") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _    <- GustavoBoostState(heroDao, content).action(testUser, tapStat("BoostBuy", "str"), renderer)
          h    <- heroDao.getHeroByUserId(userId).map(_.get)
          data <- heroDao.readGustavoData(userId).map(_.flatMap(_.as[GustavoData].toOption))
        } yield assertTrue(h.gold == 5000L) &&
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
        } yield assertTrue(h.gold == 4000L) &&
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
        } yield assertTrue(h.gold == 5000L) &&
                assertTrue(screens.exists(_.text.contains("готовится")))
      },

      test("BoostBuy платное без золота → сообщение, баф не выдан") {
        for {
          t <- env(hero(gold = 0L))
          (heroDao, renderer, content) = t
          _       <- heroDao.writeGustavoData(userId, GustavoData(None, List("str")).asJson)
          _       <- GustavoBoostState(heroDao, content).action(testUser, tapStat("BoostBuy", "str"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.gold == 0L) &&
                assertTrue(!h.statBoosts.hasActive("gustavo:str", 0L)) &&
                assertTrue(screens.exists(_.text.contains("Столько золота нет")))
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
        } yield assertTrue(ids == List("Flask", "Belt", "Back"))
      },

      test("Flask → GustavoFlask; Belt → GustavoBelt; Back → Gustavo") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          state = GustavoSuppliesState(heroDao, content)
          flaskR  <- state.action(testUser, tap("Flask"), renderer)
          beltR   <- state.action(testUser, tap("Belt"), renderer)
          backR   <- state.action(testUser, tap("Back"), renderer)
        } yield assertTrue(flaskR == StateType.GustavoFlask) &&
                assertTrue(beltR == StateType.GustavoBelt) &&
                assertTrue(backR == StateType.Gustavo)
      }
    ),

    // ── Пополнение фляги ───────────────────────────────────────────────────────
    suite("GustavoFlaskState")(

      test("enter с неполной флягой → предложение с ценой (2 глотка × 25 = 50) и Купить/Назад") {
        for {
          t <- env(heroWithFlask(flask(1, 3)))
          (heroDao, renderer, content) = t
          _       <- GustavoFlaskState(heroDao, content).enter(testUser, renderer)
          screens <- renderer.sentScreens
          ids      = screens.last.choices.map(_.id).toSet
        } yield assertTrue(screens.last.text.contains("50")) &&
                assertTrue(ids == Set("Refill", "Back"))
      },

      test("Refill → пополняет заряды до максимума, списывает 50, уходит в припасы") {
        for {
          t <- env(heroWithFlask(flask(1, 3)))
          (heroDao, renderer, content) = t
          st      <- GustavoFlaskState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(st == StateType.GustavoSupplies) &&
                assertTrue(flaskCharges(h.equipment.flask).contains(3)) &&
                assertTrue(h.gold == 4950L) &&
                assertTrue(screens.exists(_.text.contains("наполнил флягу")))
      },

      test("Refill без золота → сообщение, заряды и золото не меняются") {
        for {
          t <- env(heroWithFlask(flask(1, 3), gold = 10L))
          (heroDao, renderer, content) = t
          _       <- GustavoFlaskState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.gold == 10L) &&
                assertTrue(flaskCharges(h.equipment.flask).contains(1)) &&
                assertTrue(screens.exists(_.text.contains("Столько золота нет")))
      },

      test("Refill с полной флягой → «уже полная», золото не списано") {
        for {
          t <- env(heroWithFlask(flask(3, 3)))
          (heroDao, renderer, content) = t
          _       <- GustavoFlaskState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.gold == 5000L) &&
                assertTrue(screens.exists(_.text.contains("полная")))
      },

      test("Refill без фляги → «фляги нет», золото не списано") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoFlaskState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.gold == 5000L) &&
                assertTrue(screens.exists(_.text.contains("фляги-то у тебя и нет")))
      },

      test("Back → GustavoSupplies") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          st <- GustavoFlaskState(heroDao, content).action(testUser, tap("Back"), renderer)
        } yield assertTrue(st == StateType.GustavoSupplies)
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

      test("Refill → пополняет бутыли до максимума, списывает 200, уходит в припасы") {
        for {
          t <- env(heroWithBelt(belt(1, 3)))
          (heroDao, renderer, content) = t
          st      <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(st == StateType.GustavoSupplies) &&
                assertTrue(beltCharges(h.equipment.belt).contains(3)) &&
                assertTrue(h.gold == 4800L) &&
                assertTrue(screens.exists(_.text.contains("забил бутыли")))
      },

      test("Refill без золота → сообщение, бутыли и золото не меняются") {
        for {
          t <- env(heroWithBelt(belt(1, 3), gold = 10L))
          (heroDao, renderer, content) = t
          _       <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.gold == 10L) &&
                assertTrue(beltCharges(h.equipment.belt).contains(1)) &&
                assertTrue(screens.exists(_.text.contains("Столько золота нет")))
      },

      test("Refill с полным поясом → «все полны», золото не списано") {
        for {
          t <- env(heroWithBelt(belt(3, 3)))
          (heroDao, renderer, content) = t
          _       <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.gold == 5000L) &&
                assertTrue(screens.exists(_.text.contains("полны")))
      },

      test("Refill без пояса → «пояса нет», золото не списано") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          _       <- GustavoBeltState(heroDao, content).action(testUser, tap("Refill"), renderer)
          h       <- heroDao.getHeroByUserId(userId).map(_.get)
          screens <- renderer.sentScreens
        } yield assertTrue(h.gold == 5000L) &&
                assertTrue(screens.exists(_.text.contains("пояса-то у тебя и нет")))
      },

      test("Back → GustavoSupplies") {
        for {
          t <- env(hero())
          (heroDao, renderer, content) = t
          st <- GustavoBeltState(heroDao, content).action(testUser, tap("Back"), renderer)
        } yield assertTrue(st == StateType.GustavoSupplies)
      }
    )
  )
}
