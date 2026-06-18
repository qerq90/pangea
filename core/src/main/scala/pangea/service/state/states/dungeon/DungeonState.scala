package pangea.service.state.states.dungeon

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.ActiveBattle
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DungeonState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "FindEvent"     -> Target.Run { (user, _, renderer) => findEvent(user, renderer) },
      "GoDarker"      -> Target.Run { (user, _, renderer) => changeDungeonLevel(user, renderer, +1) },
      "GoLighter"     -> Target.Run { (user, _, renderer) => changeDungeonLevel(user, renderer, -1) },
      "GoToCity"       -> Target.Goto(StateType.GlobalMap),
      "OpenInventory"  -> Target.Goto(StateType.Inventory),
      "CharacterInfo"  -> Target.Goto(StateType.HeroStats),
      "Rest"           -> Target.Goto(StateType.Rest)
    ),
    fallback = Target.Goto(StateType.Dungeon)
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero     <- getHero(user)
      _        <- ZIO.when(hero.traumaActive(now)) {
                    val remaining  = hero.traumaRemainingText(now).getOrElse("?")
                    val traumaName = hero.traumaName.getOrElse("Травма")
                    renderer.show(user, Screen(
                      content.format("dungeon.traumaActive",
                        "remaining"  -> remaining,
                        "traumaName" -> traumaName), Nil))
                  }
      enterScr  = content.screen("dungeon.enter")
      text      = content.format("dungeon.enter.text", "level" -> hero.dungeonLevel.toString)
      _        <- renderer.show(user, Screen(text, enterScr.choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def findEvent(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      _      <- renderer.show(user, Screen(content.text("dungeon.findEvent"), Nil))
      idx    <- Random.nextIntBounded(StateType.events.size)
      event   = StateType.events(idx)
      result <- event match {
        case StateType.Battle => startBattle(user, hero)
        case StateType.Spring => healAtSpring(user, hero, now, renderer)
        case other            => ZIO.succeed(other)
      }
    } yield result

  private def healAtSpring(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] = {
    val maxHp      = hero.effectiveMaxHp(nowMs)
    val healed     = (maxHp - hero.fightStats.hp).max(0L)
    val hasFlask   = hero.equipment.flask.itemType != pangea.model.item.ItemType.NoItem
    for {
      _ <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp))
      _ <- ZIO.when(hasFlask)(heroDao.updateFlaskCharges(user.userId, Hero.MaxFlaskCharges))
      _ <- renderer.show(user, Screen(
             content.format("dungeon.spring",
               "healed"      -> healed.toString,
               "flaskFilled" -> (if (hasFlask) content.text("dungeon.springFlaskFilled") else "")), Nil))
    } yield StateType.Dungeon
  }

  private def startBattle(user: User, hero: Hero): Task[StateType] =
    for {
      seed         <- Random.nextLong
      (monster, _)  = MonsterGenerator.generate(hero.dungeonLevel, Rng(seed))
      battle        = ActiveBattle.fromMonster(monster)
      _            <- heroDao.writeActiveBattle(user.userId, battle.asJson)
    } yield StateType.Battle

  private def changeDungeonLevel(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero     <- getHero(user)
      newLevel  = math.max(1, math.min(150, hero.dungeonLevel + delta))
      _        <- heroDao.updateDungeonLevel(user.userId, newLevel)
      enterScr  = content.screen("dungeon.enter")
      text      = content.format("dungeon.enter.text", "level" -> newLevel.toString)
      _        <- renderer.show(user, Screen(text, enterScr.choices))
    } yield StateType.Dungeon

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
