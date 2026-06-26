package pangea.service.state.states.guild

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.Task

/**
 * Меню Гильдии Искателей: вход в [[TrophyExchangeState]], тренировочный зал и
 * возврат в город.
 */
case class GuildState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "TrophyExchange" -> Target.Goto(StateType.TrophyExchange),
      "TrainingHall"   -> Target.Goto(StateType.TrainingHall),
      "OpenCharacter"  -> Target.Run { (user, _, _) => CharacterMenu.open(heroDao, user.userId, StateType.Guild) },
      "LeaveGuild"     -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.Guild) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.HeroStats

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("guild.menu"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
