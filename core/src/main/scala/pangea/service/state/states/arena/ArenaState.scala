package pangea.service.state.states.arena

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.arena.ArenaDao
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Players, Renderer, SceneContent, Screen, Target}
import pangea.model.arena.{ArenaFight, ArenaMode, ArenaRates, ArenaSide, ArenaStatus}
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
  * монета. Режим выбирается первым (см. ArenaMode): один на один отряд остаётся
  * за оградой, в групповом бою выходит с героем. Добычи и опыта здесь нет, а
  * проигравший уходит на своих ногах с единицей HP.
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
      "ArenaSolo"    -> Target.Run { (u, _, r) => showMode(u, ArenaMode.Solo, r) },
      "ArenaGroup"   -> Target.Run { (u, _, r) => showMode(u, ArenaMode.Group, r) },
      "ArenaNearest" -> Target.Run { (u, ua, r) => showWaiting(u, modeOf(ua), r) },
      "ArenaByCode"  -> Target.Run { (u, ua, r) => enlist(u, modeOf(ua), r) },
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

  /** Бой кончился, пока герой не смотрел: рассказываем чем. Кто итог уже
    * читал (обычно тот, чей ход всё и решил), второй раз его не увидит, а
    * строка уходит, когда её прочитали обе стороны. */
  private def finish(user: User, fight: ArenaFight, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      fresh  = fight.seenBy(user.userId)
      _     <- ZIO.when(fight.unseenFor(user.userId))(
                 renderer.show(user, Screen(fight.lastLog.mkString("\n"), Nil)) *>
                   renderer.show(user, Screen(
                     content.text(if (fight.winner.contains(user.userId)) "arena.won" else "arena.lost"), Nil)))
      _     <- if (fresh.readByAll) arenaDao.delete(fight.id) else arenaDao.update(fresh, now)
      _     <- showMenu(user, renderer)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("arena.menu"), List(
      content.choice("ArenaSolo",  "arena.solo").copy(color = ChoiceColor.Positive, row = Some(0)),
      content.choice("ArenaGroup", "arena.group").copy(color = ChoiceColor.Positive, row = Some(0)),
      content.choice("ArenaRest", "arena.rest").copy(row = Some(1)),
      content.choice("OpenCharacter", "common.character").copy(row = Some(1)),
      content.choice("LeaveArena", "arena.leave").copy(color = ChoiceColor.Negative, row = Some(2)),
      CityExit.button(content, Some(2)))))

  /** Экран режима: что в нём за бой и как найти соперника. Режим едет в
    * кнопках: по нему и очередь, и запись. */
  private def showMode(user: User, mode: ArenaMode, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text(s"arena.mode.${mode.entryName}"), List(
      content.choice("ArenaNearest", "arena.nearest")
        .copy(color = ChoiceColor.Positive, row = Some(0), data = Map("mode" -> mode.entryName)),
      content.choice("ArenaByCode", "arena.byCode").copy(row = Some(0), data = Map("mode" -> mode.entryName)),
      backButton(1)))).as(StateType.Arena)

  /** Режим из кнопки. Старые кнопки без режима — один на один, как и было. */
  private def modeOf(ua: UserAction): ArenaMode =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("mode")))
      .flatMap(ArenaMode.withNameOption).getOrElse(ArenaMode.Solo)

  // ── Ближайший бой: кто уже ждёт ───────────────────────────────────────────

  /** «Ближайший бой»: герой сам встаёт в общую очередь и видит остальных,
    * кто в ней стоит. Записи по коду сюда не попадают — их зовут кодом. */
  private def showWaiting(user: User, mode: ArenaMode, renderer: Renderer): Task[StateType] =
    for {
      _   <- enqueue(user, mode)
      all <- arenaDao.waiting(ArenaRates.ListSize + 1, mode)
      free = all.filter(f => f.waiting && !f.has(user.userId)).take(ArenaRates.ListSize)
      // В групповом бою видно, сколько бойцов выйдет с каждым: отряд считаем по
      // герою сейчас — с ним он и выйдет.
      sizes <- ZIO.foreach(free)(f =>
                 if (mode != ArenaMode.Group) ZIO.succeed(0)
                 else heroDao.getHeroById(f.a.heroId).map(_.fold(0)(ArenaBattle.squadSize)))
      _ <- if (free.isEmpty)
             renderer.show(user, Screen(content.text("arena.noneWaiting"), List(backButton(0))))
           else {
             val buttons = free.zip(sizes).zipWithIndex.map { case ((f, size), i) =>
               val line =
                 if (mode == ArenaMode.Group)
                   content.format("arena.foeLineGroup", "name" -> f.a.name, "lvl" -> f.a.lvl.toString, "n" -> size.toString)
                 else content.format("arena.foeLine", "name" -> f.a.name, "lvl" -> f.a.lvl.toString)
               Choice(s"$JoinPrefix${f.code}", line, color = ChoiceColor.Positive, row = Some(i))
             }
             renderer.show(user, Screen(content.text("arena.waitingHeader"),
               buttons :+ backButton(free.size)))
           }
    } yield StateType.Arena

  /** Встать в общую очередь режима: записи нет — заводим открытую, запись по
    * коду — открываем её же, код при этом остаётся в силе. Ждал в другом режиме
    * — запись переходит в этот. */
  private def enqueue(user: User, mode: ArenaMode): Task[Unit] =
    arenaDao.ofUser(user.userId).flatMap {
      case Some(f) if f.waiting && (!f.open || f.mode != mode) =>
        nowMs.flatMap(now => arenaDao.update(f.copy(open = true, mode = mode), now))
      case Some(_) => ZIO.unit
      case None =>
        for {
          hero <- getHero(user)
          now  <- nowMs
          _    <- register(user, hero, now, mode, open = true)
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

  /** Записаться и показать свой код. Уже записан — просто напоминаем код; в
    * другом режиме — запись переходит в этот, код прежний. */
  private def enlist(user: User, mode: ArenaMode, renderer: Renderer): Task[StateType] =
    arenaDao.ofUser(user.userId).flatMap {
      case Some(fight) if fight.waiting && fight.mode != mode =>
        for {
          now  <- nowMs
          moved = fight.copy(mode = mode)
          _    <- arenaDao.update(moved, now)
          _    <- showEnlisted(user, moved, renderer)
        } yield StateType.Arena
      case Some(fight) if fight.waiting => showEnlisted(user, fight, renderer).as(StateType.Arena)
      case Some(_)                      => renderer.show(user, Screen(content.text("arena.alreadyFighting"), Nil))
                                             .as(StateType.Arena)
      case None =>
        for {
          hero  <- getHero(user)
          now   <- nowMs
          fight <- register(user, hero, now, mode)
          _     <- showEnlisted(user, fight, renderer)
        } yield StateType.Arena
    }

  /** Завести запись со свободным кодом: занятый код — не беда, катаем другой. */
  private def register(user: User, hero: Hero, nowMs: Long, mode: ArenaMode,
                       open: Boolean = false, tries: Int = 8): Task[ArenaFight] =
    for {
      n    <- Random.nextIntBetween(0, 10000)
      code  = f"$n%04d"
      name <- nameOf(user)
      side  = ArenaSide(user.userId, hero.id, name, hero.lvl)
      made <- arenaDao.create(ArenaFight(0L, code, ArenaStatus.Waiting, side, None, open = open, mode = mode), nowMs)
      out  <- made match {
                case Some(fight)         => ZIO.succeed(fight)
                case None if tries > 1   => register(user, hero, nowMs, mode, open, tries - 1)
                case None                => ZIO.fail(new Throwable("Arena: no free code"))
              }
    } yield out

  /** Экран записи. Кнопка одна: пока герой ждёт, ему и надо стоять здесь —
    * зов соперника приходит только тому, кто на арене. Уйти можно, сняв
    * запись; «Назад» отсюда вело на этот же экран и только мешало. */
  private def showEnlisted(user: User, fight: ArenaFight, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.format("arena.enlisted", "code" -> fight.code,
      "mode" -> content.text(s"arena.modeName.${fight.mode.entryName}")), List(
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
      // В групповом бою каждый выходит со своим отрядом; крыса из-под ног
      // (набор «Крыса», порог 12) — в любом режиме, вместе с героем.
      enter    = (side: ArenaSide, h: Hero) => ArenaBattle.withStartingRat(
                   if (fight.mode == ArenaMode.Group) ArenaBattle.withSquad(side, h) else side, h)
      joined   = fight.copy(status = ArenaStatus.Fighting, a = enter(fight.a, foeHero), b = Some(enter(me, hero)))
                   .passTurn(first, now).copy(round = 1)
      _       <- arenaDao.update(joined, now)
      // Соперника зовём к экрану: он ждал на арене, а теперь уже дерётся.
      _       <- scheduler.schedule(fight.a.userId, now, TaskKind.ArenaPoke, StateType.Arena,
                   BattleState.ArenaPokeAction)
      _       <- scheduler.schedule(first, now + ArenaRates.TurnMs, TaskKind.ArenaTurn,
                   StateType.Battle, BattleState.ArenaTurnAction)
      _       <- renderer.show(user, Screen(content.format(startedKey(fight),
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
          _    <- ZIO.foreachDiscard(foe)(f => renderer.show(user, Screen(content.format(startedKey(fight),
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

  /** С чего начинается бой: в групповом — со строя за спиной у каждого. */
  private def startedKey(fight: ArenaFight): String =
    if (fight.mode == ArenaMode.Group) "arena.startedGroup" else "arena.started"

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
