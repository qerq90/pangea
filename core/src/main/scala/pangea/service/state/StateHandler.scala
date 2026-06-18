package pangea.service.state

import pangea.engine.Renderer
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, VkId}
import pangea.repository.hero.HeroRepository
import pangea.repository.user.UserRepository
import pangea.service.sender.Api
import pangea.service.sender.vk.VkRenderer
import pangea.service.state.states.StatesMap
import zio.{Ref, Semaphore, Task, ZIO, ZLayer}

class StateHandler(
  api:      Api,
  userRepo: UserRepository,
  heroRepo: HeroRepository,
  states:   Map[StateType, State],
  lock:     PlayerLock
) {

  def makeActionVK(vkId: VkId, eventId: Long, action: UserAction): Task[Unit] =
    lock.withLock(vkId) {
      val renderer = VkRenderer(api)
      for {
        userOp <- userRepo.getUserByVkId(vkId)
        user <- userOp match {
          case Some(u) => ZIO.succeed(u)
          case None    => userRepo.insertUserByVk(vkId)
        }
        isNew <- userRepo.checkAndRecordEvent(user.userId, eventId)
        _ <- ZIO.when(isNew) {
          makeAction(user, action, renderer)
            .catchAll(err =>
              ZIO.logError(s"Error occurred while making action: ${err.getMessage}") *>
                api.sendMessage(user, "Произошла ошибка, пропишите /home или обратитесь в техподдержку", List.empty, None)
            )
        }
      } yield ()
    }

  def makeActionTelegram(telegramId: TelegramId, action: UserAction): Task[Unit] = {
    val renderer = VkRenderer(api)
    for {
      userOp <- userRepo.getUserByTelegramId(telegramId)
      user <- userOp match {
        case Some(u) => ZIO.succeed(u)
        case None    => userRepo.insertUserByTelegramId(telegramId)
      }
      _ <- makeAction(user, action, renderer)
    } yield ()
  }

  private def makeAction(user: User, action: UserAction, renderer: Renderer): Task[Unit] =
    for {
      heroOp <- heroRepo.getHero(user.userId)
      hero <- heroOp match {
        case Some(h) => ZIO.succeed(h)
        case None    => heroRepo.registerNewHero(user.userId)
      }
      state <- ZIO
        .fromOption(states.get(hero.state))
        .orElseFail(new Throwable(s"Not found state of hero with id ${hero.id} of user ${user.userId}: state - ${hero.state}"))

      potentiallyNewState <- state.action(user, action, renderer)
      newState <- ZIO
        .fromOption(states.get(potentiallyNewState))
        .orElseFail(new Throwable(s"Not found new state of hero with id ${hero.id} of user ${user.userId}: state - ${hero.state}"))

      _ <- heroRepo.updateState(user.userId, potentiallyNewState).unless(potentiallyNewState == hero.state)
      _ <- newState.enter(user, renderer).unless(potentiallyNewState == hero.state)
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
        lock      <- Ref.make(Map.empty[VkId, Semaphore]).map(new PlayerLock(_))
      } yield new StateHandler(api, userRepo, heroRepo, statesMap.states, lock)
    )
}
