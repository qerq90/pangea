package pangea.service.state.states.arena

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.arena.ArenaDao
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Players, Renderer, SceneContent, Screen, Target}
import pangea.model.arena.{ArenaFight, ArenaRates, ArenaSide, ArenaStatus}
import pangea.model.hero.Hero
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{User, UserId}
import pangea.service.schedule.Scheduler
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.arena.ArenaState._
import pangea.service.state.{CharacterMenu, CityExit, InstantRest, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Арена в Центре города: бой игрока против игрока.
  *
  * Записаться можно двумя путями. «Ближайший бой» показывает тех, кто уже
  * ждёт, — жми и дерись. «По записи» заводит свою запись с четырёхзначным
  * кодом: код передают тому, с кем договорились, а он пишет его в ответ.
  *
  * Первым ходит тот, у кого ловкость с интеллектом больше; поровну — решает
  * монета. Отряд на песок не выходит, добычи и опыта здесь нет, а проигравший
  * уходит на своих ногах с единицей HP.
  */
case class ArenaState(
  heroDao:   HeroDao,
  arenaDao:  ArenaDao,
  content:   SceneContent,
  scheduler: Scheduler = Scheduler.none,
  // Имя бойца на песке: соперник видит его вместо клички моба.
  players:   Option[Players] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "ArenaNearest" -> Target.Run { (u, _, r) => showWaiting(u, r) },
      "ArenaByCode"  -> Target.Run { (u, _, r) => enlist(u, r) },
      "ArenaCancel"  -> Target.Run { (u, _, r) => cancel(u, r) },
      "ArenaRest"    -> Target.Run { (u, _, r) => rest(u, r) },
      "ArenaMenu"    -> Target.Run { (u, _, r) => enter(u, r).as(StateType.Arena) },
      // Соперник нашёлся, пока герой ждал: арена зовёт его на песок.
      "ArenaPoke"    -> Target.Run { (u, _, r) => start(u, r) },
      "LeaveArena"   -> Target.Goto(StateType.CityCenter),
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.Arena) },
      CityExit.route
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets + StateType.HeroStats + StateType.Battle + StateType.Arena + StateType.Rest

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    arenaDao.ofUser(user.userId).flatMap {
      case Some(fight) if fight.finished => finish(user, fight, renderer)
      case Some(fight) if fight.waiting  => showEnlisted(user, fight, renderer)
      case _                             => showMenu(user, renderer)
    }

  /** Бой кончился, пока герой не смотрел: рассказываем чем и убираем строку. */
  private def finish(user: User, fight: ArenaFight, renderer: Renderer): Task[Unit] =
    for {
      _ <- ZIO.when(fight.unseenFor(user.userId))(
             renderer.show(user, Screen(fight.lastLog.mkString("\n"), Nil)))
      _ <- renderer.show(user, Screen(
             content.text(if (fight.winner.contains(user.userId)) "arena.won" else "arena.lost"), Nil))
      _ <- arenaDao.delete(fight.id)
      _ <- showMenu(user, renderer)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("arena.menu"), List(
      content.choice("ArenaNearest", "arena.nearest").copy(color = ChoiceColor.Positive, row = Some(0)),
      content.choice("ArenaByCode",  "arena.byCode").copy(row = Some(0)),
      content.choice("ArenaRest", "arena.rest").copy(row = Some(1)),
      content.choice("OpenCharacter", "common.character").copy(row = Some(1)),
      content.choice("LeaveArena", "arena.leave").copy(color = ChoiceColor.Negative, row = Some(2)),
      CityExit.button(content, Some(2)))))

  // ── Ближайший бой: кто уже ждёт ───────────────────────────────────────────

  /** «Ближайший бой»: герой сам встаёт в общую очередь и видит остальных,
    * кто в ней стоит. Записи по коду сюда не попадают — их зовут кодом. */
  private def showWaiting(user: User, renderer: Renderer): Task[StateType] =
    for {
      _   <- enqueue(user)
      all <- arenaDao.waiting(ArenaRates.ListSize + 1)
      free = all.filter(f => f.waiting && !f.has(user.userId)).take(ArenaRates.ListSize)
      _ <- if (free.isEmpty)
             renderer.show(user, Screen(content.text("arena.noneWaiting"), List(backButton(0))))
           else {
             val buttons = free.zipWithIndex.map { case (f, i) =>
               Choice(s"$JoinPrefix${f.code}",
                 content.format("arena.foeLine", "name" -> f.a.name, "lvl" -> f.a.lvl.toString),
                 color = ChoiceColor.Positive, row = Some(i))
             }
             renderer.show(user, Screen(content.text("arena.waitingHeader"),
               buttons :+ backButton(free.size)))
           }
    } yield StateType.Arena

  /** Встать в общую очередь: записи нет — заводим открытую, запись по коду —
    * открываем её же, код при этом остаётся в силе. */
  private def enqueue(user: User): Task[Unit] =
    arenaDao.ofUser(user.userId).flatMap {
      case Some(f) if f.waiting && !f.open =>
        nowMs.flatMap(now => arenaDao.update(f.copy(open = true), now))
      case Some(_) => ZIO.unit
      case None =>
        for {
          hero <- getHero(user)
          now  <- nowMs
          _    <- register(user, hero, now, open = true)
        } yield ()
    }

  /** Отдых на арене — тот же костёр, что в лабиринте, только просыпается
    * герой здесь же (см. `RestState`). Есть мгновенный отдых от благословения
    * — тратится он, и никакого привала. */
  private def rest(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      used <- InstantRest.use(heroDao, scheduler, content, user, now, renderer)
      res  <- used match {
                case Some(_) => enter(user, renderer).as(StateType.Arena)
                case None    =>
                  heroDao.writeSceneData(user.userId,
                    Json.obj("wakeTo" -> (StateType.Arena: StateType).asJson)).as(StateType.Rest)
              }
    } yield res

  private def backButton(row: Int): Choice =
    content.choice("ArenaMenu", "arena.back").copy(color = ChoiceColor.Negative, row = Some(row))

  // ── По записи: свой код и чужой ───────────────────────────────────────────

  /** Записаться и показать свой код. Уже записан — просто напоминаем код. */
  private def enlist(user: User, renderer: Renderer): Task[StateType] =
    arenaDao.ofUser(user.userId).flatMap {
      case Some(fight) if fight.waiting => showEnlisted(user, fight, renderer).as(StateType.Arena)
      case Some(_)                      => renderer.show(user, Screen(content.text("arena.alreadyFighting"), Nil))
                                             .as(StateType.Arena)
      case None =>
        for {
          hero  <- getHero(user)
          now   <- nowMs
          fight <- register(user, hero, now)
          _     <- showEnlisted(user, fight, renderer)
        } yield StateType.Arena
    }

  /** Завести запись со свободным кодом: занятый код — не беда, катаем другой. */
  private def register(user: User, hero: Hero, nowMs: Long, open: Boolean = false, tries: Int = 8): Task[ArenaFight] =
    for {
      n    <- Random.nextIntBetween(0, 10000)
      code  = f"$n%04d"
      name <- nameOf(user)
      side  = ArenaSide(user.userId, hero.id, name, hero.lvl)
      made <- arenaDao.create(ArenaFight(0L, code, ArenaStatus.Waiting, side, None, open = open), nowMs)
      out  <- made match {
                case Some(fight)         => ZIO.succeed(fight)
                case None if tries > 1   => register(user, hero, nowMs, open, tries - 1)
                case None                => ZIO.fail(new Throwable("Arena: no free code"))
              }
    } yield out

  /** Экран записи. Кнопка одна: пока герой ждёт, ему и надо стоять здесь —
    * зов соперника приходит только тому, кто на арене. Уйти можно, сняв
    * запись; «Назад» отсюда вело на этот же экран и только мешало. */
  private def showEnlisted(user: User, fight: ArenaFight, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.format("arena.enlisted", "code" -> fight.code), List(
      content.choice("ArenaCancel", "arena.cancel").copy(color = ChoiceColor.Negative, row = Some(0)))))

  private def cancel(user: User, renderer: Renderer): Task[StateType] =
    arenaDao.ofUser(user.userId).flatMap {
      case Some(fight) if fight.waiting =>
        arenaDao.delete(fight.id) *>
          renderer.show(user, Screen(content.text("arena.cancelled"), Nil)) *>
          enter(user, renderer).as(StateType.Arena)
      case _ => enter(user, renderer).as(StateType.Arena)
    }

  /** Написанный код или кнопка из списка — оба ведут сюда. */
  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(JoinPrefix) => join(user, a.drop(JoinPrefix.length), renderer)
      case _ =>
        val typed = ua.text.trim
        if (typed.length == ArenaRates.CodeLength && typed.forall(_.isDigit)) join(user, typed, renderer)
        else enter(user, renderer).as(StateType.Arena)
    }

  // ── Сведение ──────────────────────────────────────────────────────────────

  /** Встать напротив того, кто ждёт с этим кодом. */
  private def join(user: User, code: String, renderer: Renderer): Task[StateType] =
    for {
      found <- arenaDao.byCode(code)
      res <- found match {
        case None => say(user, "arena.noSuchCode", renderer)
        case Some(fight) if !fight.waiting => say(user, "arena.codeBusy", renderer)
        case Some(fight) if fight.has(user.userId) => say(user, "arena.ownCode", renderer)
        case Some(fight) => startFight(user, fight, renderer)
      }
    } yield res

  private def startFight(user: User, fight: ArenaFight, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      hero    <- getHero(user)
      foeHero <- heroDao.getHeroById(fight.a.heroId).flatMap(ZIO.fromOption(_))
                   .orElseFail(new Throwable("Arena: foe hero is gone"))
      // Своя запись, если герой тоже ждал, уходит: драться можно только одну.
      mine    <- arenaDao.ofUser(user.userId)
      _       <- ZIO.foreachDiscard(mine.filter(_.waiting))(m => arenaDao.delete(m.id))
      coin    <- Random.nextBoolean
      name    <- nameOf(user)
      me       = ArenaSide(user.userId, hero.id, name, hero.lvl)
      first    = ArenaBattle.firstTurn(fight.a.userId -> foeHero, user.userId -> hero, now, coin)
      joined   = fight.copy(status = ArenaStatus.Fighting, b = Some(me))
                   .passTurn(first, now).copy(round = 1)
      _       <- arenaDao.update(joined, now)
      // Соперника зовём к экрану: он ждал на арене, а теперь уже дерётся.
      _       <- scheduler.schedule(fight.a.userId, now, TaskKind.ArenaPoke, StateType.Arena,
                   BattleState.ArenaPokeAction)
      _       <- scheduler.schedule(first, now + ArenaRates.TurnMs, TaskKind.ArenaTurn,
                   StateType.Battle, BattleState.ArenaTurnAction)
      _       <- renderer.show(user, Screen(content.format("arena.started",
                   "name" -> fight.a.name, "who" -> whoFirst(first, user.userId)), Nil))
      out     <- toBattle(user, hero, joined, now, renderer)
    } yield out

  /** Зов арены ждущему: соперник нашёлся, пора на песок. */
  private def start(user: User, renderer: Renderer): Task[StateType] =
    arenaDao.ofUser(user.userId).flatMap {
      // Бой кончился, пока герой был на арене, — зовут его за итогом.
      case Some(fight) if fight.finished => finish(user, fight, renderer).as(StateType.Arena)
      case Some(fight) if !fight.waiting =>
        for {
          now  <- nowMs
          hero <- getHero(user)
          foe   = fight.foeOf(user.userId)
          _    <- ZIO.foreachDiscard(foe)(f => renderer.show(user, Screen(content.format("arena.started",
                    "name" -> f.name, "who" -> whoFirst(fight.turn.getOrElse(user.userId), user.userId)), Nil)))
          out  <- toBattle(user, hero, fight, now, renderer)
        } yield out
      case _ => enter(user, renderer).as(StateType.Arena)
    }

  /** Собрать герою его зеркало боя и увести в бой. */
  private def toBattle(user: User, hero: Hero, fight: ArenaFight, nowMs: Long, renderer: Renderer): Task[StateType] =
    (fight.sideOf(user.userId), fight.foeOf(user.userId)) match {
      case (Some(me), Some(foe)) =>
        for {
          foeHero <- heroDao.getHeroById(foe.heroId).map(_.getOrElse(hero))
          battle   = ArenaBattle.assemble(fight, me, foe, hero, foeHero, nowMs)
          _       <- heroDao.writeActiveBattle(user.userId, battle.asJson)
          _       <- ZIO.when(!fight.isTurn(user.userId))(
                       renderer.show(user, Screen(content.format("arena.waitFoe", "name" -> foe.name), Nil)))
        } yield StateType.Battle
      case _ => enter(user, renderer).as(StateType.Arena)
    }

  private def whoFirst(first: UserId, me: UserId): String =
    content.text(if (first == me) "arena.youFirst" else "arena.foeFirst")

  private def say(user: User, key: String, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text(key), Nil)) *> enter(user, renderer).as(StateType.Arena)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  /** Как зовут бойца. Имени нет — обходимся общим словом: бой от этого не
    * страдает, а соперник всё равно видит уровень. */
  private def nameOf(user: User): Task[String] =
    players.fold(ZIO.succeed(content.text("arena.someone")))(
      _.getDisplayName(user).orElseSucceed(content.text("arena.someone")))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object ArenaState {
  /** Кнопка «встать напротив»: за префиксом — код записи. */
  val JoinPrefix: String = "ArenaJoin_"
}
