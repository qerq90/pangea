package pangea.service.state.states.hero

import cats.implicits.catsSyntaxOptionId
import io.circe.jawn
import pangea.dao.hero.HeroDao
import pangea.model.state.StateType
import pangea.model.state.StateType.{Dungeon, HeroStats}
import pangea.model.user.User
import pangea.service.sender.Api
import pangea.service.state.states.hero.keyboard.HeroStatsKeyboard
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class HeroStatsState(api: Api, heroDao: HeroDao) extends State {
  // main window of hero stats here
  override def enter(user: User): Task[Unit] =
    for {
      hero <- heroDao
        .getHeroByUserId(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))

      heroInfo = hero.getInfo
      _ <- api.sendMessage(
        user,
        heroInfo,
        List.empty,
        HeroStatsKeyboard.keyboard(hero.upgradePoints).some
      )
    } yield ()

  private def matchUserAction(action: UserAction): Action =
    action.payload match {
      case Some(payload) => jawn.decode[Action](payload).toOption.get
      case None          => Action.Text
    }

  override def action(user: User, action: UserAction): Task[StateType] =
    matchUserAction(action) match {
      case Action.Back    => ZIO.succeed(Dungeon)
      case Action.Text    => enter(user).as(HeroStats)
      case Action.Unknown => enter(user).as(HeroStats)
    }
}
