package pangea.service.state.states.events

import io.circe.Json
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.LoreData
import pangea.model.monster.{MiniBoss, Race}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

object ElementalLairStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(heroLvl: Long = 15L) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(lvl = heroLvl))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (ElementalLairState(heroDao, content), heroDao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  // Вид элементаля логово тянет случайным индексом по MiniBoss.values.
  private val fireIdx  = MiniBoss.elementals.indexOf(MiniBoss.FireElemental)
  private val stoneIdx = MiniBoss.elementals.indexOf(MiniBoss.StoneElemental)

  override def spec = suite("ElementalLairState")(

    test("enter → предчувствие и две кнопки: подойти или уйти") {
      for {
        t <- makeState()
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("привкус железа")) &&
              assertTrue(screens.last.choices.map(_.id) == List("ApproachElemental", "LeaveLair"))
    },

    test("подойти → виден вид элементаля и три кнопки: напасть, персонаж, уйти") {
      for {
        t <- makeState()
        (state, _, renderer) = t
        _       <- state.action(testUser, tap("ApproachElemental"), renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(screens.last.text.contains("элементаля крушащего всё вокруг")) &&
              assertTrue(ids == List("AttackElemental", "OpenCharacter", "LeaveLair"))
    },

    test("вид подставляется в родительном падеже — «огненного», а не «огненный»") {
      def approachText(idx: Int) =
        for {
          t <- makeState()
          (state, _, renderer) = t
          _    <- TestRandom.feedInts(idx)
          _    <- state.action(testUser, tap("ApproachElemental"), renderer)
          text <- renderer.sentScreens.map(_.last.text)
        } yield text
      for {
        fire  <- approachText(fireIdx)
        stone <- approachText(stoneIdx)
      } yield assertTrue(fire.contains("огненного элементаля")) &&
              assertTrue(stone.contains("каменного элементаля")) &&
              assertTrue(!fire.contains("огненный элементаля"))
    },

    test("первая встреча даёт особую реплику, вторая — уже нет") {
      for {
        t <- makeState()
        (state, dao, renderer) = t
        _        <- state.action(testUser, tap("ApproachElemental"), renderer)
        first    <- renderer.sentScreens.map(_.last.text)
        lore     <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).get)
        // Второй заход в логово: сцену чистим, чтобы событие началось заново.
        _        <- dao.writeSceneData(userId, Json.Null)
        _        <- state.action(testUser, tap("ApproachElemental"), renderer)
        second   <- renderer.sentScreens.map(_.last.text)
      } yield assertTrue(first.contains("пока работал в ордене")) &&
              assertTrue(lore.metElemental) &&
              assertTrue(!second.contains("пока работал в ордене"))
    },

    test("«Персонаж» возвращает в ту же сцену с тем же элементалем, а не в начало") {
      for {
        t <- makeState()
        (state, dao, renderer) = t
        _        <- state.action(testUser, tap("ApproachElemental"), renderer)
        result   <- state.action(testUser, tap("OpenCharacter"), renderer)
        back     <- dao.readReturnState(userId)
        // Возврат из «Персонажа» = повторный enter в это состояние.
        _        <- state.enter(testUser, renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(back.contains(StateType.ElementalLair)) &&
              assertTrue(screens.last.choices.map(_.id).contains("AttackElemental"))
    },

    test("напасть на огненного → бой: раса, статы по BossLvL и вид записаны") {
      // Уровень героя 15 → BossLvL = (15−1)/5 = 2.
      for {
        t <- makeState(heroLvl = 15L)
        (state, dao, renderer) = t
        _      <- TestRandom.feedInts(fireIdx)
        _      <- state.action(testUser, tap("ApproachElemental"), renderer)
        result <- state.action(testUser, tap("AttackElemental"), renderer)
        battle <- battleOf(dao).map(_.get)
        scene  <- dao.readSceneData(userId)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(battle.monsterRace == Race.Elemental.entryName) &&
              assertTrue(battle.boss.contains(MiniBoss.FireElemental)) &&
              assertTrue(battle.monsterStats.hp == 1500L * 2L) &&
              assertTrue(battle.monsterStats.armor == 750L * 2L) &&
              assertTrue(battle.monsterStats.atk == 250L * 2L) &&
              assertTrue(battle.monsterStats.accuracy == 250L * 2L) &&
              assertTrue(battle.monsterStats.evasion == 100L * 2L) &&
              assertTrue(battle.monsterStats.energy == 150L * 2L) &&
              assertTrue(battle.monsterStats.defence == 0L) &&
              assertTrue(scene.contains(Json.Null)) // сцену освободили для боя
    },

    test("напасть на каменного → его собственные статы по BossLvL") {
      for {
        t <- makeState(heroLvl = 15L)
        (state, dao, renderer) = t
        _      <- TestRandom.feedInts(stoneIdx)
        _      <- state.action(testUser, tap("ApproachElemental"), renderer)
        result <- state.action(testUser, tap("AttackElemental"), renderer)
        battle <- battleOf(dao).map(_.get)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(battle.boss.contains(MiniBoss.StoneElemental)) &&
              assertTrue(battle.monsterStats.hp == 1250L * 2L) &&
              assertTrue(battle.monsterStats.armor == 1500L * 2L) &&
              assertTrue(battle.monsterStats.atk == 350L * 2L) &&
              assertTrue(battle.monsterStats.accuracy == 200L * 2L) &&
              assertTrue(battle.monsterStats.evasion == 50L * 2L) &&
              assertTrue(battle.monsterStats.energy == 100L * 2L) &&
              assertTrue(battle.monsterStats.defence == 0L) &&
              // энергии он копит по 7 за уровень босса, как и огненный
              assertTrue(MiniBoss.StoneElemental.energyRegen(2L) == 14L) &&
              assertTrue(MiniBoss.StoneElemental.expReward(2L) == 400L)
    },

    test("в логове поровну шансов встретить огненного и каменного") {
      // Вид тянется случайным индексом по элементалям — их ровно два, значит 50/50.
      assertTrue(MiniBoss.elementals.size == 2) &&
      assertTrue(MiniBoss.elementals.toSet[MiniBoss] == Set[MiniBoss](MiniBoss.FireElemental, MiniBoss.StoneElemental)) &&
      // Гнилой Джо — тоже минибосс, но в логове не водится
      assertTrue(!MiniBoss.elementals.contains(MiniBoss.RottenJoe))
    },

    test("розыгрыш логова равномерен: каждый элементаль выпадает ровно с одного броска") {
      // Логово тянет вид через Random.nextIntBounded(elementals.size) — бросок
      // равномерный, поэтому равенство шансов сводится к биекции «индекс → вид».
      // Гоняем весь диапазон бросков: если в игру добавят третьего элементаля, он
      // обязан занять свой отдельный индекс, иначе тест упадёт. Так любой новый вид
      // автоматически входит в розыгрыш с той же долей, что и остальные.
      ZIO.foreach(MiniBoss.elementals.indices.toList) { idx =>
        for {
          t <- makeState()
          (state, dao, renderer) = t
          _      <- TestRandom.feedInts(idx)
          _      <- state.action(testUser, tap("ApproachElemental"), renderer)
          _      <- state.action(testUser, tap("AttackElemental"), renderer)
          battle <- battleOf(dao).map(_.get)
          boss   <- ZIO.fromOption(battle.boss).orElseFail(new Throwable("в бою нет минибосса"))
        } yield boss
      }.map { drawn =>
        // каждый бросок дал свой вид, и вместе они покрывают всех элементалей игры
        assertTrue(drawn.distinct.size == drawn.size) &&
        assertTrue(drawn == MiniBoss.elementals.toList) &&
        assertTrue(drawn.toSet == MiniBoss.values.filter(_.race == Race.Elemental).toSet)
      }
    },

    test("уйти → возврат в лабиринт, сцена очищена") {
      for {
        t <- makeState()
        (state, dao, renderer) = t
        _      <- state.action(testUser, tap("ApproachElemental"), renderer)
        result <- state.action(testUser, tap("LeaveLair"), renderer)
        scene  <- dao.readSceneData(userId)
      } yield assertTrue(result == StateType.Dungeon) && assertTrue(scene.contains(Json.Null))
    },

    // ── Уровень босса ─────────────────────────────────────────────────────────
    test("BossLvL = (уровень героя − 1) / 5 вниз, но не меньше 1") {
      val cases = List(
        1L -> 1L, 5L -> 1L, 6L -> 1L, 10L -> 1L, // до 10 уровня босс держится на первом
        11L -> 2L, 15L -> 2L, 16L -> 3L, 51L -> 10L, 150L -> 29L)
      assertTrue(cases.forall { case (heroLvl, expected) => MiniBoss.bossLvl(heroLvl) == expected })
    }
  )
}
