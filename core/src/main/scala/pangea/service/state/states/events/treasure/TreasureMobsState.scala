package pangea.service.state.states.events.treasure

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Random, Task}

/**
 * Событие «мобы, выкопавшие сокровище» (5%). Интро: встречаем 2–3 мобов одной
 * случайной расы. Две кнопки — «Атаковать» (начать цепочку боёв) и «Уйти»
 * (вернуться в лабиринт). Прогресс цепочки (раса/число боёв/диапазон дублонов)
 * кладём в `scene_data` как [[TreasureMobsChain]]; спавном боёв заведует
 * [[TreasureMobsFightState]].
 */
case class TreasureMobsState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "AttackMobs" -> Target.Goto(StateType.TreasureMobsFight),
      "LeaveMobs"  -> Target.Run { (user, _, renderer) =>
        heroDao.writeSceneData(user.userId, Json.Null) *>
          renderer.show(user, Screen(content.text("treasureMobs.left"), Nil)).as(StateType.Dungeon)
      }
    ),
    fallback = Target.Goto(StateType.Dungeon)
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets + StateType.Dungeon + StateType.TreasureMobsFight

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      raceIdx <- Random.nextIntBounded(Race.values.size)
      race     = Race.values(raceIdx)
      count   <- Random.nextIntBetween(2, 4) // 2 или 3
      chain    = TreasureMobsChain(race.entryName, remaining = count, doubloonMin = 2, doubloonMax = 3)
      _       <- heroDao.writeSceneData(user.userId, chain.asJson)
      _       <- renderer.show(user, Screen(
                   content.format("treasureMobs.enter.text",
                     "count" -> count.toString,
                     "race"  -> race.toString),
                   content.screen("treasureMobs.enter").choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
