package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.dao.user.UserDao
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, VkId}
import pangea.service.state.states.StatesMap
import zio.{Task, ZIO, ZLayer}

class StateHandler(
  userDao: UserDao,
  heroDao: HeroDao,
  states: Map[StateType, State]
) {

  def makeActionVK(vkId: VkId, action: String): Task[Unit] =
    for {
      userOp <- userDao.getUserByVkId(vkId)
      user <- ZIO
        .fromOption(userOp)
        .orElseFail(new Throwable(s"Not found user with vkId ${vkId}"))

      _ <- makeAction(user, action)
    } yield ()

  def makeActionTelegram(telegramId: TelegramId, action: String): Task[Unit] =
    for {
      userOp <- userDao.getUserByTelegramId(telegramId)
      user <- ZIO
        .fromOption(userOp)
        .orElseFail(
          new Throwable(s"Not found user with telegramId ${telegramId}")
        )

      _ <- makeAction(user, action)
    } yield ()

  private def makeAction(user: User, action: String): Task[Unit] =
    for {
      heroOp <- heroDao.getHeroByUserId(user.userId)
      hero <- ZIO
        .fromOption(heroOp)
        .orElseFail(
          new Throwable(
            s"Not found hero for user with userId ${user.userId}"
          )
        )
      stateOp = states.get(hero.state)
      state <- ZIO
        .fromOption(stateOp)
        .orElseFail(
          new Throwable(
            s"Not found state of hero with id ${hero.id} of user ${user.userId}: state - ${hero.state}"
          )
        )

      potentiallyNewState <- state.action(user, action)
      newState <- ZIO
        .fromOption(states.get(potentiallyNewState))
        .orElseFail(
          new Throwable(
            s"Not found new state of hero with id ${hero.id} of user ${user.userId}: state - ${hero.state}"
          )
        )

      _ <-
        userDao
          .updateState(user.userId, potentiallyNewState)
          .unless(potentiallyNewState == hero.state)

      _ <- newState
        .enter()
        .unless(potentiallyNewState == hero.state)
    } yield ()
}

object StateHandler {
  val live: ZLayer[StatesMap with HeroDao with UserDao, Nothing, StateHandler] =
    ZLayer.fromZIO(
      for {
        userDao   <- ZIO.service[UserDao]
        heroDao   <- ZIO.service[HeroDao]
        statesMap <- ZIO.service[StatesMap]
      } yield new StateHandler(userDao, heroDao, statesMap.states)
    )
}
