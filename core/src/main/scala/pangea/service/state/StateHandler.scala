package pangea.service.state

import pangea.engine.Renderer
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.repository.hero.HeroRepository
import pangea.repository.user.UserRepository
import pangea.service.sender.Api
import pangea.service.sender.vk.VkRenderer
import pangea.service.state.states.StatesMap
import zio.{Ref, Semaphore, Task, ZIO, ZLayer}

class StateHandler(
  api: Api,
  userRepo: UserRepository,
  heroRepo: HeroRepository,
  states: Map[StateType, State],
  lock: PlayerLock
) {

  def makeActionVK(
      vkId: VkId,
      eventId: Long,
      action: UserAction
  ): Task[Unit] = {
    val renderer = VkRenderer(api)
    for {
      userOp <- userRepo.getUserByVkId(vkId)
      user <- userOp match {
        case Some(u) => ZIO.succeed(u)
        case None    => userRepo.insertUserByVk(vkId)
      }
      _ <- lock.withLock(user.userId) {
        for {
          isNew <- userRepo.checkAndRecordEvent(user.userId, eventId)
          _     <- ZIO.when(isNew)(makeActionSafe(user, action, renderer))
        } yield ()
      }
    } yield ()
  }

  def makeActionTelegram(
      telegramId: TelegramId,
      action: UserAction
  ): Task[Unit] = {
    val renderer = VkRenderer(api)
    for {
      userOp <- userRepo.getUserByTelegramId(telegramId)
      user <- userOp match {
        case Some(u) => ZIO.succeed(u)
        case None    => userRepo.insertUserByTelegramId(telegramId)
      }
      _ <- lock.withLock(user.userId)(makeActionSafe(user, action, renderer))
    } yield ()
  }

  /** Исполняет отложенную задачу (см. `Scheduler`) под локом игрока. Действие
    * применяется только если текущее состояние героя совпадает с
    * `expectedState` — иначе задача потеряла смысл (игрок ушёл в другой стейт)
    * и тихо пропускается. Ошибки `makeAction` пробрасываются наружу, чтобы
    * планировщик мог посчитать попытку.
    */
  def runScheduled(
      userId: UserId,
      expectedState: StateType,
      action: UserAction
  ): Task[StateHandler.RunResult] = {
    val renderer = VkRenderer(api)
    lock.withLock(userId) {
      userRepo.getUserById(userId).flatMap {
        case None => ZIO.succeed(StateHandler.RunResult.Skipped)
        case Some(user) =>
          heroRepo.getHero(userId).flatMap {
            case Some(hero) if hero.state == expectedState =>
              makeAction(user, action, renderer)
                .as(StateHandler.RunResult.Executed)
            case _ =>
              ZIO.succeed(StateHandler.RunResult.Skipped)
          }
      }
    }
  }

  private def makeActionSafe(
      user: User,
      action: UserAction,
      renderer: Renderer
  ): Task[Unit] =
    makeAction(user, action, renderer)
      .catchAll(err =>
        ZIO.logError(
          s"Error occurred while making action: ${err.getMessage}"
        ) *>
          api.sendMessage(
            user,
            "Произошла ошибка, пропишите /home или обратитесь в техподдержку",
            List.empty,
            None
          )
      )

  private def makeAction(
      user: User,
      action: UserAction,
      renderer: Renderer
  ): Task[Unit] =
    for {
      heroOp <- heroRepo.getHero(user.userId)
      hero <- heroOp match {
        case Some(h) => ZIO.succeed(h)
        case None    => heroRepo.registerNewHero(user.userId)
      }
      state <- ZIO
        .fromOption(states.get(hero.state))
        .orElseFail(
          new Throwable(
            s"Not found state of hero with id ${hero.id} of user ${user.userId}: state - ${hero.state}"
          )
        )

      potentiallyNewState <- state.action(user, action, renderer)
      _ <- transitionTo(user, hero.state, potentiallyNewState, renderer)
    } yield ()

  /** Performs a state transition (persist + enter) and follows any
    * `autoAdvance` chain so effect nodes route onward without a player action.
    * The fuel guard stops a misconfigured cycle of auto-advancing states.
    */
  private def transitionTo(
      user: User,
      from: StateType,
      to: StateType,
      renderer: Renderer,
      fuel: Int = 16
  ): Task[Unit] =
    (for {
      target <- ZIO
        .fromOption(states.get(to))
        .orElseFail(
          new Throwable(s"Not found state '$to' for user ${user.userId}")
        )
      _ <- heroRepo.updateState(user.userId, to)
      _ <- target.enter(user, renderer)
      _ <- target.autoAdvance match {
        case Some(next) if fuel > 0 =>
          transitionTo(user, to, next, renderer, fuel - 1)
        case _ => ZIO.unit
      }
    } yield ()).unless(to == from).unit
}

object StateHandler {

  /** Исход исполнения отложенной задачи. */
  sealed trait RunResult
  object RunResult {

    /** Действие применено (состояние совпало с ожидаемым). */
    case object Executed extends RunResult

    /** Задача неактуальна (нет игрока/героя или состояние сменилось) — закрыть
      * как Done.
      */
    case object Skipped extends RunResult
  }

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
        lock <- Ref.make(Map.empty[UserId, Semaphore]).map(new PlayerLock(_))
      } yield new StateHandler(api, userRepo, heroRepo, statesMap.states, lock)
    )
}
