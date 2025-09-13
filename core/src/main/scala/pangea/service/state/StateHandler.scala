package pangea.service.state

import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, VkId}
import pangea.repository.hero.HeroRepository
import pangea.repository.user.UserRepository
import pangea.service.sender.Api
import pangea.service.state.states.StatesMap
import zio.{Task, ZIO, ZLayer}

class StateHandler(
  api: Api,
  userRepo: UserRepository,
  heroRepo: HeroRepository,
  states: Map[StateType, State]
) {

  def makeActionVK(vkId: VkId, action: UserAction): Task[Unit] =
    for {
      userOp <- userRepo.getUserByVkId(vkId)
      user <- userOp match {
        case Some(value) => ZIO.succeed(value)
        case None        => userRepo.insertUserByVk(vkId)
      }

      _ <- makeAction(user, action)
        .catchAll(err =>
          ZIO
            .logError(
              s"Error occurred while making action: ${err.getMessage}"
            ) *> api.sendMessage(
            user,
            "Произошла ошибка, пропишите /home или обратитесь в техподдержку",
            List.empty,
            None
          )
        )
    } yield ()

  def makeActionTelegram(
      telegramId: TelegramId,
      action: UserAction
  ): Task[Unit] =
    for {
      userOp <- userRepo.getUserByTelegramId(telegramId)
      user <- userOp match {
        case Some(value) => ZIO.succeed(value)
        case None        => userRepo.insertUserByTelegramId(telegramId)
      }

      _ <- makeAction(user, action)
    } yield ()

  private def makeAction(user: User, action: UserAction): Task[Unit] =
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
        heroRepo
          .updateState(user.userId, potentiallyNewState)
          .unless(potentiallyNewState == hero.state)

      _ <- newState
        .enter(user)
        .unless(potentiallyNewState == hero.state)
    } yield ()
}

object StateHandler {
  val live: ZLayer[
    Api with StatesMap with HeroRepository with UserRepository,
    Nothing,
    StateHandler
  ] =
    ZLayer.fromZIO(
      for {
        api       <- ZIO.service[Api]
        userRepo  <- ZIO.service[UserRepository]
        heroRepo  <- ZIO.service[HeroRepository]
        statesMap <- ZIO.service[StatesMap]
      } yield new StateHandler(api, userRepo, heroRepo, statesMap.states)
    )
}
