package pangea.service.state

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Choice, ChoiceColor, Renderer, Screen}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.repository.hero.HeroRepository
import pangea.repository.user.UserRepository
import io.circe.syntax.EncoderOps
import pangea.service.chat.ChatCommand
import pangea.service.parcel.{Parcels, TransferTarget, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.states.parcel.TransferState
import pangea.service.sender.Api
import pangea.service.sender.vk.VkRenderer
import pangea.service.state.states.StatesMap
import zio.{Ref, Semaphore, Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

class StateHandler(
  api: Api,
  userRepo: UserRepository,
  heroRepo: HeroRepository,
  heroDao: HeroDao,
  payouts: Payouts,
  parcels: Parcels,
  transfers: Transfers,
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

  /** «Передать» из общей беседы: адресата бот берёт из процитированного
    * сообщения, а выбирать вещь отправитель будет у себя в личке — там у
    * каждой кнопки видны характеристики, и одинаковые названия не путаются.
    *
    * Отдавать можно только из города; получатель может быть где угодно, вещь
    * придёт ему посылкой. */
  def transferFromChat(senderVk: VkId, targetVk: VkId, query: String, eventId: Long): Task[Unit] = {
    val renderer = VkRenderer(api)
    userRepo.getUserByVkId(senderVk).flatMap {
      case None => ZIO.unit  // писал не игрок — молчим
      case Some(sender) =>
        lock.withLock(sender.userId) {
          // Номера сообщений у беседы свои, и с номерами из лички они
          // пересекаются — поэтому чатовые события считаем отрицательными,
          // иначе повтор-защита приняла бы их за уже виденные.
          userRepo.checkAndRecordEvent(sender.userId, -eventId).flatMap { fresh =>
            ZIO.when(fresh)(startTransfer(sender, targetVk, query, renderer)).unit
          }
        }
    }
  }

  private def startTransfer(sender: User, targetVk: VkId, query: String, renderer: Renderer): Task[Unit] =
    for {
      heroOp   <- heroRepo.getHero(sender.userId)
      targetOp <- userRepo.getUserByVkId(targetVk)
      targetHero <- targetOp.fold(ZIO.none: Task[Option[pangea.model.hero.Hero]])(u => heroRepo.getHero(u.userId))
      _ <- (heroOp, targetOp, targetHero) match {
        case (Some(hero), Some(target), Some(theirHero)) if target.userId != sender.userId =>
          if (!StateType.cityStates.contains(hero.state))
            api.sendMessage(sender, StateHandler.TransferNotInCity, List.empty, None)
          else
            for {
              name   <- api.getName(target).map(r => s"${r.response.head.firstName} ${r.response.head.lastName}")
                          .orElse(ZIO.succeed(StateHandler.TransferSomeone))
              to      = TransferTarget(theirHero.id, target.userId, name)
              item    = ChatCommand.itemQuery(query)
              count   = ChatCommand.count(query)
              now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
              // Понятное название уходит сразу; экран нужен только там, где
              // вещь можно спутать, — в первую очередь у экипировки.
              outcome <- transfers.quickSend(sender, hero, to, item, count, now)
              _ <- outcome match {
                case Transfers.Outcome.Sent(message) =>
                  api.sendMessage(sender, message, List.empty, None)
                case Transfers.Outcome.Empty =>
                  api.sendMessage(sender, StateHandler.transferNothing(item), List.empty, None)
                case Transfers.Outcome.NeedPick =>
                  val scene = TransferState.TransferScene(
                    toHeroId = theirHero.id.value,
                    toUserId = target.userId.value,
                    toName   = name,
                    query    = item,
                    count    = count)
                  heroDao.writeSceneData(sender.userId, scene.asJson) *>
                    // Куда вернуть после передачи — туда же, откуда позвали.
                    heroDao.writeReturnState(sender.userId, Some(hero.state)) *>
                    enterState(sender, StateType.Transfer, renderer)
              }
            } yield ()
        case (Some(_), _, _) =>
          api.sendMessage(sender, StateHandler.TransferNoTarget, List.empty, None)
        case _ => ZIO.unit
      }
    } yield ()

  /** Показать экран состояния и записать его герою (без проверки «уже там»). */
  private def enterState(user: User, to: StateType, renderer: Renderer): Task[Unit] =
    for {
      target <- ZIO.fromOption(states.get(to)).orElseFail(new Throwable(s"Not found state '$to'"))
      _      <- target.enter(user, renderer)
      _      <- heroRepo.updateState(user.userId, to)
    } yield ()

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
      // Выручка с аукциона, не влезшая в банковскую ячейку, догоняет героя в
      // городе: в лабиринте такие деньги наполовину сгорели бы при смерти.
      _ <- ZIO.when(StateType.cityStates.contains(hero.state))(
             payouts.deliver(user, hero, renderer).ignore)
      // Посылки ложатся в банковскую ячейку откуда угодно: она не при герое.
      _ <- parcels.deliver(user, hero, renderer).ignore
      hero <- heroRepo.getHero(user.userId).map(_.getOrElse(hero))
      _ <-
        if (StateHandler.isHomeCommand(action))
          goHome(user, renderer)
        else if (StateHandler.isRestartCommand(action))
          offerRestart(user, renderer)
        else if (StateHandler.isRestartConfirm(action))
          restart(user, renderer)
        else
          for {
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
    } yield ()

  /** `/restart`, первый шаг: только предупреждение с inline-кнопкой. Клавиатура
    * текущей сцены при этом остаётся на месте — передумавший игрок просто играет
    * дальше, никакой «отмены» ему не нужно. */
  private def offerRestart(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(
      StateHandler.RestartWarning,
      List(Choice(StateHandler.RestartConfirmId, "💀 Да, стереть всё и начать заново",
        color = ChoiceColor.Negative)),
      inline = true))

  /** `/restart`, второй шаг: герой и всё, что к нему привязано, удаляются одной
    * транзакцией. Запись пользователя остаётся, поэтому тут же прогоняем пустое
    * действие через обычный диспетчер — он не найдёт героя, заведёт нового и
    * покажет первый экран регистрации, как при самом первом входе. */
  private def restart(user: User, renderer: Renderer): Task[Unit] =
    for {
      _ <- heroDao.deleteHero(user.userId)
      _ <- api.sendMessage(user, StateHandler.RestartDone, List.empty, None)
      _ <- makeAction(user, UserAction("", None), renderer)
    } yield ()

  /** Глобальная команда `/home` (см. ARCHITECTURE.md §10) — аварийный выход в
    * город из ЛЮБОГО состояния, минуя обработчик текущей сцены (на случай если
    * игрок застрял из-за возможного бага). Работает поверх обычного диспетчера:
    * durable-статы, инвентарь и исход боя не трогаются — герой не умирает, не
    * получает и не теряет ничего. Сбрасывается только эфемерный слой (активный
    * бой, `scene_data`), чтобы не тащить за собой зависшую/битую сцену в город. */
  private def goHome(user: User, renderer: Renderer): Task[Unit] =
    for {
      _ <- heroDao.clearActiveBattle(user.userId)
      _ <- heroDao.writeSceneData(user.userId, Json.Null)
      target <- ZIO
        .fromOption(states.get(StateType.GlobalMap))
        .orElseFail(new Throwable("GlobalMap state is not registered in StatesMap"))
      _ <- target.enter(user, renderer)
      _ <- heroRepo.updateState(user.userId, StateType.GlobalMap)
    } yield ()

  /** Performs a state transition (enter + persist) and follows any
    * `autoAdvance` chain so effect nodes route onward without a player action.
    * The fuel guard stops a misconfigured cycle of auto-advancing states.
    *
    * `enter` идёт ДО записи состояния: он готовит данные экрана (напр.
    * `FoundItemState` генерирует находку и кладёт её в `scene_data`). Если бы
    * состояние писалось первым, падение `enter` оставляло бы героя в новом
    * состоянии без его данных — и следующее нажатие («Забрать») валилось бы на
    * чтении `scene_data`, пока игрок не пропишет /home.
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
      _ <- target.enter(user, renderer)
      _ <- heroRepo.updateState(user.userId, to)
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

  /** `/home` — глобальная команда, а не кнопка: срабатывает только на «голый»
    * текст без payload (иначе кнопка с совпадающей по случайности подписью
    * тоже считалась бы командой). Регистр и пробелы по краям не важны. */
  private def isHomeCommand(action: UserAction): Boolean =
    action.payload.isEmpty && action.text.trim.equalsIgnoreCase("/home")

  /** `/restart` — по тем же правилам, что и `/home`: голый текст, без payload. */
  private def isRestartCommand(action: UserAction): Boolean =
    action.payload.isEmpty && action.text.trim.equalsIgnoreCase("/restart")

  /** Подтверждение перезапуска — единственная кнопка, которую диспетчер ловит
    * поверх состояний. Её id больше нигде не используется, так что чужую
    * кнопку с ним не спутать. */
  private def isRestartConfirm(action: UserAction): Boolean =
    action.payload.exists(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption
        .flatMap(_.get("action")).contains(RestartConfirmId))

  val RestartConfirmId: String = "RestartConfirm"

  val RestartWarning: String =
    "⚠️ Это сотрёт героя без возможности вернуть: уровень, опыт, серебро, дублоны, " +
    "снаряжение, сумку, бочку, куб Азата, репутацию и всё, что вы узнали о мире. " +
    "Игра начнётся с самого начала.\n\nЕсли передумали — просто продолжайте играть."

  val RestartDone: String = "💀 Прошлое стёрто. Начинаем заново."

  /** Ответы на «Передать» из беседы: короткие и в личку, чтобы не шуметь в чате. */
  val TransferNotInCity: String =
    "Передавать вещи можно только из города: дойдите до Кинета и повторите."

  val TransferNoTarget: String =
    "Не понял, кому передавать. Ответьте на сообщение игрока (или перешлите его) и напишите «Передать …»."

  val TransferSomeone: String = "искатель"

  def transferNothing(query: String): String =
    s"В сумке нет ничего похожего на «$query»."

  val live: ZLayer[
    Api with StatesMap with HeroRepository with UserRepository with HeroDao with Payouts with Parcels with Transfers,
    Nothing,
    StateHandler
  ] =
    ZLayer.fromZIO(
      for {
        api       <- ZIO.service[Api]
        userRepo  <- ZIO.service[UserRepository]
        heroRepo  <- ZIO.service[HeroRepository]
        heroDao   <- ZIO.service[HeroDao]
        payouts   <- ZIO.service[Payouts]
        parcels   <- ZIO.service[Parcels]
        transfers <- ZIO.service[Transfers]
        statesMap <- ZIO.service[StatesMap]
        lock <- Ref.make(Map.empty[UserId, Semaphore]).map(new PlayerLock(_))
      } yield new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, statesMap.states, lock)
    )
}
