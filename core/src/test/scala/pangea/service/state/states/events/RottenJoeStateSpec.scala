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

/** Встреча с Гнилым Джо на раскопках: реплика, выбор из трёх кнопок и бой с его
 *  собственными статами. */
object RottenJoeStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(heroLvl: Long = 15L) =
    for {
      dao      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(lvl = heroLvl))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (RottenJoeState(dao, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def loreOf(dao: TestHeroDao) =
    dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  override def spec = suite("RottenJoeState")(

    test("вход: описание встречи и три кнопки — напасть, персонаж, уйти") {
      for {
        t <- makeState()
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.map(_.text).mkString.contains("резкий запах гнили")) &&
              assertTrue(screens.last.choices.map(_.id) == List("AttackJoe", "OpenCharacter", "LeaveJoe"))
    },

    test("первая встреча даёт отдельную реплику, вторая — уже нет") {
      for {
        t <- makeState()
        (state, dao, renderer) = t
        _      <- state.enter(testUser, renderer)
        first  <- renderer.sentScreens.map(_.map(_.text).mkString)
        lore   <- loreOf(dao)
        _      <- state.enter(testUser, renderer)
        second <- renderer.sentScreens.map(_.map(_.text).mkString)
      } yield assertTrue(first.contains("надо будет спросить об этом Трактирщика")) &&
              assertTrue(lore.metJoe) && // встречу запомнили — трактирщик откроет кнопку
              // во второй заход реплика больше не добавляется
              assertTrue(second.split("надо будет спросить").length == 2)
    },

    test("напасть → бой с Гнилым Джо: раса, статы по BossLvL и вид записаны") {
      for {
        t <- makeState(heroLvl = 15L) // BossLvL = 2
        (state, dao, renderer) = t
        result <- state.action(testUser, tap("AttackJoe"), renderer)
        battle <- battleOf(dao).map(_.get)
        scene  <- dao.readSceneData(userId)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(battle.monsterRace == Race.Undead.entryName) &&
              assertTrue(battle.boss.contains(MiniBoss.RottenJoe)) &&
              assertTrue(battle.monsterStats.hp == 2200L * 2L) &&
              assertTrue(battle.monsterStats.atk == 200L * 2L) &&
              assertTrue(battle.monsterStats.armor == 0L) &&
              assertTrue(battle.monsterName == "Гнилой Джо") &&
              assertTrue(scene.contains(Json.Null)) // сцену освободили для боя
    },

    test("уйти → возврат в лабиринт, сцена очищена") {
      for {
        t <- makeState()
        (state, dao, renderer) = t
        result <- state.action(testUser, tap("LeaveJoe"), renderer)
        scene  <- dao.readSceneData(userId)
      } yield assertTrue(result == StateType.Dungeon) && assertTrue(scene.contains(Json.Null))
    },

    test("раскопки: Джо забрал свои 5% у схрона, могила осталась прежней") {
      import pangea.service.state.states.events.treasure.TreasureDigState
      assertTrue(TreasureDigState.JoePct == 5) &&
      assertTrue(TreasureDigState.SchronPct == 75) &&
      // схрон + Джо + могила = ровно сто
      assertTrue(TreasureDigState.SchronPct + TreasureDigState.JoePct + 20 == 100)
    },

    test("нежить не игровая раса: в выборе при создании персонажа её нет") {
      assertTrue(!Race.mortals.contains(Race.Undead)) &&
      assertTrue(Race.bossRaces.contains(Race.Undead)) &&
      assertTrue(Race.immuneToDots(Race.Undead))
    }
  )
}
