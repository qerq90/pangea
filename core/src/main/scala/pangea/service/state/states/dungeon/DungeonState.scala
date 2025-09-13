package pangea.service.state.states.dungeon

import io.circe.jawn.decode
import pangea.model.state.StateType
import pangea.model.state.StateType.{events, Dungeon, HeroStats}
import pangea.model.user.User
import pangea.service.sender.Api
import pangea.service.state.states.dungeon.keyboard.EnterKeyboard
import pangea.service.state.{State, UserAction}
import pangea.util._
import zio.{Task, ZIO}

case class DungeonState(api: Api) extends State {

  override def enter(user: User): Task[Unit] =
    api.sendMessage(
      user,
      "Вы попадаете в великий лабиринт",
      List.empty,
      Some(EnterKeyboard.keyboard)
    )

  private def getAction(action: UserAction) =
    action.payload match {
      case Some(json) => decode[Action](json).toOption.get
      case None       => Action.Text
    }

  override def action(user: User, action: UserAction): Task[StateType] =
    getAction(action) match {
      case Action.FindEvent   => findEvent(user)
      case Action.MakeBonfire => ZIO.succeed(HeroStats)
      case Action.Text        => ZIO.succeed(Dungeon)
    }

  private def findEvent(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "Вы пробираетесь по коридорам лабиринта, нервно оглядываясь и чувствуя чей то взгляд на затылке.",
        List.empty,
        None
      )
      .as(events.random)

}
