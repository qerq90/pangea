package pangea.service.state.states.artifact

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{Achievement, FetPotion, Hero}
import pangea.model.stats.{BaseStats, ParamsBuff, StatBoost, StatBoosts}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{NpcQuestLog, UserAction}
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Зелья Фета: остановить опыт, запустить опыт, сбросить вложенные очки. */
object FetPotionSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((Map("action" -> key) ++ data).asJson.noSpaces))

  private def potion(key: String, p: FetPotion): UserAction = tap(key, "potion" -> p.entryName)

  private def shop(hero: Hero) =
    for {
      dao      <- TestHeroDao.withHero(userId, hero)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (FetShopState(dao, TestArtifactRepository.empty, TestInventoryRepository.accepting, content), dao, renderer)

  private def heroOf(dao: TestHeroDao) = dao.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)   = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Зелья Фета")(

    test("на прилавке — три зелья, каждое своей кнопкой") {
      for {
        t <- shop(TestFixtures.hero(userId))
        (state, _, r) = t
        _    <- state.action(testUser, tap("FetShop"), r)
        last <- r.sentScreens.map(_.last)
      } yield assertTrue(last.choices.filter(_.id == "FetPotion").flatMap(_.data.get("potion")) ==
                FetPotion.values.map(_.entryName).toList)
    },

    test("Зелье застоя: 100 дублонов, опыт стоит; второй раз не продаётся; Зелье движения его запускает") {
      for {
        t <- shop(TestFixtures.hero(userId).copy(doubloons = 300L))
        (state, dao, r) = t
        _       <- state.action(testUser, potion("FetPotionYes", FetPotion.ExpStop), r)
        stopped <- heroOf(dao)
        _       <- state.action(testUser, potion("FetPotion", FetPotion.ExpStop), r)
        card    <- r.sentScreens.map(_.last)
        _       <- state.action(testUser, potion("FetPotionYes", FetPotion.ExpStop), r)
        again   <- heroOf(dao)
        _       <- state.action(testUser, potion("FetPotionYes", FetPotion.ExpStart), r)
        started <- heroOf(dao)
        said    <- texts(r)
      } yield assertTrue(stopped.expFrozen && stopped.doubloons == 200L) &&
              // опыт и так стоит — купить нечего
              assertTrue(card.text.contains("и так стоит") && !card.choices.exists(_.id == "FetPotionBuy")) &&
              assertTrue(again.doubloons == 200L) &&
              assertTrue(!started.expFrozen && started.doubloons == 100L) &&
              assertTrue(said.contains("Опыт остановлен") && said.contains("Опыт снова идёт"))
    },

    test("пока опыт стоит, его не дают ниоткуда — и «Персонаж» об этом говорит") {
      val hero   = TestFixtures.hero(userId).copy(lvl = 3L, exp = 10L, upgradePoints = 0L, dungeonLevel = 5)
      val frozen = hero.copy(expFrozen = true)
      assertTrue(frozen.gainExp(100000L) == frozen && frozen.expFrom(500L) == 0L) &&
      assertTrue(NpcQuestLog.expReward(frozen) == 0L && NpcQuestLog.expReward(hero) > 0L) &&
      assertTrue(hero.gainExp(100000L).lvl > hero.lvl) &&
      assertTrue(frozen.getInfo(0L).contains("Опыт остановлен") && !hero.getInfo(0L).contains("Опыт остановлен"))
    },

    test("Зелье забвения: вложенное — в свободные; зелья Густаво и достижения не трогает; здоровье — под новый потолок") {
      val boost  = StatBoost("gustavo:agi", ParamsBuff(0, 0, 15, 0), Long.MaxValue)
      val base   = TestFixtures.hero(userId)
      val hero   = base.copy(lvl = 10L, doubloons = 100L, upgradePoints = 0L,
                     baseStats = BaseStats(agi = 8, vit = 30, str = 10, int = 4),
                     statBoosts = StatBoosts.none.add(boost, 0L),
                     fightStats = base.fightStats.copy(hp = 5000L))
                     .withAchievement(Achievement.MurlocFavorite)
      for {
        t <- shop(hero)
        (state, dao, r) = t
        _     <- state.action(testUser, potion("FetPotion", FetPotion.Respec), r)
        card  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, potion("FetPotionYes", FetPotion.Respec), r)
        after <- heroOf(dao)
        said  <- texts(r)
      } yield assertTrue(card.text.contains("Вложено очков: 36")) &&
              // (8 + 30 + 10 + 4) − 16 стартовых = 36 очков — ровно столько и вернулось
              assertTrue(after.baseStats == Hero.StartBaseStats && after.upgradePoints == 36L) &&
              // зелье Густаво и достижение остались — и по-прежнему ложатся сверху
              assertTrue(after.statBoosts == hero.statBoosts && after.achievements == hero.achievements) &&
              assertTrue(after.effectiveBaseStats(0L).agi > Hero.StartBaseStats.agi) &&
              assertTrue(after.fightStats.hp == after.effectiveMaxHp(0L) && after.fightStats.hp < 5000L) &&
              assertTrue(after.doubloons == 0L && said.contains("Свободных очков прибавилось: 36"))
    },

    test("сбрасывать нечего — Зелье забвения не продаётся; дублонов мало — отказ") {
      val spent = TestFixtures.hero(userId).copy(doubloons = 50L, baseStats = BaseStats(agi = 5, vit = 4, str = 4, int = 4))
      for {
        t1    <- shop(TestFixtures.hero(userId).copy(doubloons = 500L, baseStats = Hero.StartBaseStats))
        (s1, _, r1) = t1
        _     <- s1.action(testUser, potion("FetPotion", FetPotion.Respec), r1)
        card  <- r1.sentScreens.map(_.last)
        t2    <- shop(spent)
        (s2, d2, r2) = t2
        _     <- s2.action(testUser, potion("FetPotionBuy", FetPotion.Respec), r2)
        _     <- s2.action(testUser, potion("FetPotionYes", FetPotion.Respec), r2)
        after <- heroOf(d2)
        said  <- texts(r2)
      } yield assertTrue(card.text.contains("Сбрасывать нечего") && !card.choices.exists(_.id == "FetPotionBuy")) &&
              assertTrue(said.contains("нужно 🟡 100")) &&
              assertTrue(after.baseStats == spent.baseStats && after.doubloons == 50L)
    }
  )
}
