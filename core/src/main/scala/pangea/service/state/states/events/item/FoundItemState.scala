package pangea.service.state.states.events.item

import io.circe.jawn
import pangea.model.state.StateType
import pangea.model.state.StateType.{Dungeon, FoundItem}
import pangea.model.user.User
import pangea.service.sender.Api
import pangea.service.state.states.events.item.keyboard.FoundItemKeyboard
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class FoundItemState(api: Api) extends State {
  override def enter(user: User): Task[Unit] =
    api.sendMessage(
      user,
      "Вы находите предмет",
      List.empty,
      Some(FoundItemKeyboard.keyboard)
    )

  private def getAction(userAction: UserAction): Action =
    userAction.payload match {
      case Some(payload) => jawn.decode[Action](payload).toOption.get
      case None          => Action.Text
    }

  override def action(user: User, action: UserAction): Task[StateType] =
    getAction(action) match {
      case Action.TakeItem     => ??? // TODO think about how to save item
      case Action.DontTakeItem => dontTakeItem(user)
      case Action.Text         => ZIO.succeed(FoundItem)
    }

  private def dontTakeItem(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "Вы уходите, так и не забрав найденный предмет",
        List.empty,
        None
      )
      .as(Dungeon)
}
