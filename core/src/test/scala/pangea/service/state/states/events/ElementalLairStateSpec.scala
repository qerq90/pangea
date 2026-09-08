package pangea.service.state.states.events

import io.circe.Json
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.LoreData
import pangea.model.monster.{Elemental, Race}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

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

    test("напасть → бой с элементалем: раса, статы по BossLvL и вид записаны") {
      // Уровень героя 15 → BossLvL = (15−1)/7 = 2.
      for {
        t <- makeState(heroLvl = 15L)
        (state, dao, renderer) = t
        _      <- state.action(testUser, tap("ApproachElemental"), renderer)
        result <- state.action(testUser, tap("AttackElemental"), renderer)
        battle <- battleOf(dao).map(_.get)
        scene  <- dao.readSceneData(userId)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(battle.monsterRace == Race.Elemental.entryName) &&
              assertTrue(battle.elemental.contains(Elemental.Fire)) &&
              assertTrue(battle.monsterStats.hp == 1500L * 2L) &&
              assertTrue(battle.monsterStats.armor == 750L * 2L) &&
              assertTrue(battle.monsterStats.atk == 250L * 2L) &&
              assertTrue(battle.monsterStats.accuracy == 250L * 2L) &&
              assertTrue(battle.monsterStats.evasion == 100L * 2L) &&
              assertTrue(battle.monsterStats.energy == 150L * 2L) &&
              assertTrue(battle.monsterStats.defence == 0L) &&
              assertTrue(scene.contains(Json.Null)) // сцену освободили для боя
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
    test("BossLvL = (уровень героя − 1) / 7 вниз, но не меньше 1") {
      val cases = List(1L -> 1L, 7L -> 1L, 8L -> 1L, 14L -> 1L, 15L -> 2L, 21L -> 2L, 22L -> 3L, 71L -> 10L)
      assertTrue(cases.forall { case (heroLvl, expected) => Elemental.bossLvl(heroLvl) == expected })
    }
  )
}
