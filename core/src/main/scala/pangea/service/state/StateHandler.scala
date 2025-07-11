package pangea.service.state

import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, VkId}
import pangea.repository.hero.HeroRepository
import pangea.repository.user.UserRepository
import pangea.service.state.states.StatesMap
import zio.{Task, ZIO, ZLayer}

class StateHandler(
  userRepo: UserRepository,
  heroRepo: HeroRepository,
  states: Map[StateType, State]
) {

  def makeActionVK(vkId: VkId, action: String): Task[Unit] =
    for {
      userOp <- userRepo.getUserByVkId(vkId)
      user <- userOp match {
        case Some(value) => ZIO.succeed(value)
        case None        => userRepo.insertUserByVk(vkId)
      }

      _ <- makeAction(user, action)
    } yield ()

  def makeActionTelegram(telegramId: TelegramId, action: String): Task[Unit] =
    for {
      userOp <- userRepo.getUserByTelegramId(telegramId)
      user <- userOp match {
        case Some(value) => ZIO.succeed(value)
        case None        => userRepo.insertUserByTelegramId(telegramId)
      }

      _ <- makeAction(user, action)
    } yield ()

  private def makeAction(user: User, action: String): Task[Unit] =
    for {
      heroOp <- heroRepo.getHero(user.userId)
      hero <- heroOp match {
        case Some(value) => ZIO.succeed(value)
        case None        => heroRepo.registerNewHero(user.userId)
      }
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
        userRepo
          .updateState(user.userId, potentiallyNewState)
          .unless(potentiallyNewState == hero.state)

      _ <- newState
        .enter()
        .unless(potentiallyNewState == hero.state)
    } yield ()
}

object StateHandler {
  val live: ZLayer[
    StatesMap with HeroRepository with UserRepository,
    Nothing,
    StateHandler
  ] =
    ZLayer.fromZIO(
      for {
        userRepo  <- ZIO.service[UserRepository]
        heroRepo  <- ZIO.service[HeroRepository]
        statesMap <- ZIO.service[StatesMap]
      } yield new StateHandler(userRepo, heroRepo, statesMap.states)
    )
}
