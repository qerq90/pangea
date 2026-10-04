package pangea.service.state.states.guild

import io.circe.jawn
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.cave.SewerRates
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.quest._
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.states.guild.QuestBoardState._
import pangea.service.state.states.road.{QuestRoadState, RoadProgress}
import pangea.service.state.{CityExit, ItemMenu, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Доска заданий Гильдии Искателей.
  *
  * Гильдия держит по доске на каждые двадцать пять уровней ([[BoardTier]]).
  * Видны все, берут только со своей: к чужой не пускают, и у каждой свой отказ.
  *
  * Доска переписывается в понедельник в полночь по Москве — целиком, вместе со
  * взятым и не сданным: неделя и есть срок. На экране видно, сколько до неё
  * осталось. Брать можно хоть все объявления разом.
  *
  * Что просят: трофей названной расы (сложность 1, плата как и была — опыт по
  * трофею и дублон) либо дело потяжелее (сложность 15) — найти караван и
  * перебить охрану или найти пещеру и выбить всех. Тяжёлые отмечаются сами,
  * там, где дело и делается; награду герой забирает, вернувшись к доске.
  */
case class QuestBoardState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  scheduler:     Scheduler,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "QuestBoard"    -> Target.Run { (u, _, r) => showTiers(u, r) },
      "BoardTier"     -> Target.Run { (u, ua, r) => openTier(u, ua, r) },
      "BoardMine"     -> Target.Run { (u, _, r) => showBoard(u, r) },
      "BackFromQuest" -> Target.Goto(StateType.Guild),
      CityExit.route
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.QuestRoad

  override def enter(user: User, renderer: Renderer): Task[Unit] = showTiers(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Разделы доски ──────────────────────────────────────────────────────────

  /** Стена с досками: своя открыта, остальные видны и закрыты. */
  private def showTiers(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      mine  = BoardTier.of(hero.lvl)
      lines = BoardTier.values.toList.map(t =>
                content.format(if (t == mine) "questBoard.tierMine" else "questBoard.tierOther",
                  "title" -> t.title))
      text  = content.format("questBoard.wall",
                "tiers" -> lines.mkString("\n"),
                "left"  -> left(now))
      keys  = BoardTier.values.toList.zipWithIndex.map { case (t, i) =>
                Choice(if (t == mine) "BoardMine" else "BoardTier",
                  content.format(if (t == mine) "questBoard.tierMineLabel" else "questBoard.tierOtherLabel",
                    "title" -> t.title),
                  data = Map("tier" -> t.key),
                  color = if (t == mine) ChoiceColor.Positive else ChoiceColor.Secondary,
                  row = Some(i / 2))
              }
      nav   = List(
                content.choice("BackFromQuest", "questBoard.back").copy(row = Some(ExitRow)),
                CityExit.button(content, Some(ExitRow)))
      _    <- renderer.show(user, Screen(text, keys ++ nav))
    } yield StateType.QuestBoard

  /** Чужая доска: к ней не пускают, и у каждой свой отказ. */
  private def openTier(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payload(ua, "tier").flatMap(BoardTier.byKey) match {
      case None       => showTiers(user, renderer)
      case Some(tier) =>
        getHero(user).flatMap { hero =>
          if (BoardTier.of(hero.lvl) == tier) showBoard(user, renderer)
          else renderer.show(user, Screen(content.text(s"questBoard.locked.${tier.key}"), Nil)) *>
                 showTiers(user, renderer)
        }
    }

  // ── Своя доска ─────────────────────────────────────────────────────────────

  private def showBoard(user: User, renderer: Renderer): Task[StateType] =
    for {
      now   <- nowMs
      hero  <- getHero(user)
      data  <- load(user, hero, now)
      inv   <- bag(hero)
      lines  = data.slots.zipWithIndex.map { case (s, i) => line(s, i, inv) }
      text   = content.format("questBoard.board",
                 "title" -> data.board.title, "quests" -> lines.mkString("\n\n"), "left" -> left(now))
      keys   = data.slots.zipWithIndex.flatMap { case (s, i) => button(s, i) } ++ List(
                 content.choice("QuestBoard", "questBoard.toWall").copy(row = Some(ExitRow)),
                 CityExit.button(content, Some(ExitRow)))
      _     <- renderer.show(user, Screen(text, keys))
    } yield StateType.QuestBoard

  /** Строка объявления: сложность, что просят и как дела. */
  private def line(slot: BoardSlot, idx: Int, inv: List[pangea.model.item.Item]): String = {
    val what = slot.kind match {
      case BoardKind.Trophy => content.format("questBoard.ask.trophy",
        "race" -> slot.race.flatMap(Race.withNameOption).map(_.genitivePlural).getOrElse(""))
      case BoardKind.CaravanRout => content.text("questBoard.ask.caravan")
      case BoardKind.CaveClear   => content.text("questBoard.ask.cave")
      case BoardKind.SewerRats   => content.text("questBoard.ask.sewer")
    }
    val state =
      if (!slot.taken)   content.text("questBoard.stateFree")
      else if (slot.ready) content.text("questBoard.stateReady")
      else slot.kind match {
        case BoardKind.Trophy if trophyFor(slot, inv).isDefined => content.text("questBoard.stateReady")
        case BoardKind.Trophy                                   => content.text("questBoard.stateNoTrophy")
        case k if k.away                                        => content.text("questBoard.stateAway")
        case _                                                  => content.text("questBoard.stateHunting")
      }
    content.format("questBoard.line",
      "n" -> (idx + 1).toString, "difficulty" -> Difficulty.render(slot.difficulty),
      "what" -> what, "state" -> state)
  }

  /** Кнопка объявления: взять, выдвинуться снова, сдать — или ничего, пока
    * герой в пути. */
  private def button(slot: BoardSlot, idx: Int): Option[Choice] = {
    val row = Some(idx / 2)
    def btn(prefix: String, key: String): Option[Choice] =
      Some(Choice(s"$prefix$idx", ItemMenu.truncate(content.format(key, "n" -> (idx + 1).toString)),
        color = ChoiceColor.Positive, row = row))
    if (!slot.taken) btn(TakePrefix, "questBoard.takeLabel")
    else if (slot.ready || slot.kind == BoardKind.Trophy) btn(HandPrefix, "questBoard.handLabel")
    // С выездного можно уйти, не доделав, — тогда к нему возвращаются той же дорогой.
    else if (slot.kind.away) btn(GoPrefix, "questBoard.goLabel")
    else None
  }

  // ── Взять и сдать ──────────────────────────────────────────────────────────

  private def take(user: User, idx: Int, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      data <- load(user, hero, now)
      out <- data.slot(idx) match {
        case Some(s) if !s.taken =>
          save(user, data.updated(idx)(_.copy(taken = true))) *>
            renderer.show(user, Screen(content.text("questBoard.taken"), Nil)) *>
            // Выездное не ждёт в лабиринте: с ним уходят прямо от доски.
            (if (s.kind.away) depart(user, s, renderer) else showBoard(user, renderer))
        case _ => showBoard(user, renderer)
      }
    } yield out

  /** Снова в путь по уже взятому выездному: герой с него ушёл, не доделав, и
    * возвращается той же дорогой. */
  private def goAgain(user: User, idx: Int, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      data <- load(user, hero, now)
      out <- data.slot(idx).filter(s => s.taken && !s.done && s.kind.away) match {
        case Some(slot) => depart(user, slot, renderer)
        case None       => showBoard(user, renderer)
      }
    } yield out

  /** Дорога к месту: герой уходит из гильдии и добирается туда сам
    * (см. [[pangea.service.state.states.road.QuestRoadState]]). */
  private def depart(user: User, slot: BoardSlot, renderer: Renderer): Task[StateType] =
    for {
      now <- nowMs
      _   <- heroDao.writeSceneData(user.userId, RoadProgress(now, slot.kind, slot.lvl).asJson)
      _   <- scheduler.schedule(user.userId, now + SewerRates.RoadMs,
               TaskKind.QuestRoad, StateType.QuestRoad, QuestRoadState.DoneAction)
      _   <- renderer.show(user, Screen(content.text("questBoard.depart"), Nil, hideKeyboard = true))
    } yield StateType.QuestRoad

  /** Сдача: трофей уходит с рук, тяжёлое просто оплачивается. */
  private def hand(user: User, idx: Int, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      data <- load(user, hero, now)
      inv  <- bag(hero)
      _ <- data.slot(idx).filter(_.taken) match {
        case None       => ZIO.unit
        case Some(slot) => slot.kind match {
          case BoardKind.Trophy => trophyFor(slot, inv) match {
            case None => renderer.show(user, Screen(content.text("questBoard.noTrophy"), Nil))
            case Some(trophy) =>
              val exp = BoardRates.trophyExp(trophy.lvl, BoardTrophy.coef(trophy))
              inventoryRepo.removeItem(trophy.id, hero.id).mapError(asThrowable) *>
                pay(user, hero, exp, BoardRates.TrophyDoubloons) *>
                save(user, data.copy(slots = data.slots.patch(idx, Nil, 1))) *>
                renderer.show(user, Screen(content.format("questBoard.paidTrophy",
                  "item" -> trophy.displayTitle, "exp" -> exp.toString,
                  "doubloons" -> BoardRates.TrophyDoubloons.toString), Nil))
          }
          case _ if !slot.done => renderer.show(user, Screen(content.text("questBoard.notYet"), Nil))
          case _ =>
            val exp = BoardRates.exp(hero.lvl)
            pay(user, hero, exp, BoardRates.Doubloons) *>
              save(user, data.copy(slots = data.slots.patch(idx, Nil, 1))) *>
              renderer.show(user, Screen(content.format("questBoard.paid",
                "exp" -> exp.toString, "doubloons" -> BoardRates.Doubloons.toString), Nil))
        }
      }
      out <- showBoard(user, renderer)
    } yield out

  private def pay(user: User, hero: Hero, exp: Long, doubloons: Long): Task[Unit] = {
    val up = hero.gainExp(exp)
    heroDao.updateExpAndLevel(user.userId, up.exp, up.lvl, up.upgradePoints) *>
      heroDao.updateDoubloons(user.userId, hero.doubloons + doubloons)
  }

  // ── Доска в базе ───────────────────────────────────────────────────────────

  /** Доска на эту неделю: не та неделя или герой перерос раздел — пишем новую. */
  private def load(user: User, hero: Hero, now: Long): Task[BoardData] =
    heroDao.readQuestData(user.userId).map(_.flatMap(_.as[BoardData].toOption).getOrElse(BoardData.empty)).flatMap {
      // Пустая доска — не повод вывесить новые: всё сдал — жди понедельника.
      case d if d.fresh(now, hero.lvl) => ZIO.succeed(d)
      case _                           => regenerate(user, hero, now)
    }

  private def regenerate(user: User, hero: Hero, now: Long): Task[BoardData] =
    for {
      seed <- Random.nextLong
      slots = QuestBoardState.roll(Rng(seed))
      data  = BoardData(BoardRates.weekOf(now), BoardTier.of(hero.lvl).key, slots)
      _    <- save(user, data)
    } yield data

  private def save(user: User, data: BoardData): Task[Unit] =
    heroDao.writeQuestData(user.userId, data.asJson)

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def trophyFor(slot: BoardSlot, inv: List[pangea.model.item.Item]): Option[pangea.model.item.Item] =
    slot.race.flatMap(r => BoardTrophy.bestFor(inv, r))

  private def bag(hero: Hero): Task[List[pangea.model.item.Item]] =
    inventoryRepo.get(hero.id).mapError(asThrowable).map(_.items.data.filter(_.id != 0L))

  private def left(now: Long): String = {
    val mins = (BoardRates.untilNextWeek(now) / 60000L).max(1L)
    val days = mins / (60L * 24L)
    val hrs  = (mins % (60L * 24L)) / 60L
    if (days > 0L) s"$days дн $hrs ч" else if (hrs > 0L) s"$hrs ч ${mins % 60L} мин" else s"$mins мин"
  }

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(TakePrefix) =>
        a.drop(TakePrefix.length).toIntOption.fold(showBoard(user, renderer))(take(user, _, renderer))
      case Some(a) if a.startsWith(HandPrefix) =>
        a.drop(HandPrefix.length).toIntOption.fold(showBoard(user, renderer))(hand(user, _, renderer))
      case Some(a) if a.startsWith(GoPrefix) =>
        a.drop(GoPrefix.length).toIntOption.fold(showBoard(user, renderer))(goAgain(user, _, renderer))
      case _ => showTiers(user, renderer)
    }

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object QuestBoardState {

  val TakePrefix: String = "BoardTake_"
  val HandPrefix: String = "BoardHand_"

  /** «Выдвинуться снова»: выездное задание уже взято, но герой с него ушёл. */
  val GoPrefix: String = "BoardGo_"

  /** Ряд с выходом: восемь объявлений занимают четыре ряда по два. */
  val ExitRow: Int = BoardRates.Slots / 2

  /** Свежая доска: по [[BoardRates.Layout]], трофейным — своя раса, выездным —
    * свой уровень. Расы в пределах доски не повторяются, пока их хватает. */
  def roll(rng: Rng): List[BoardSlot] = {
    val kinds = BoardRates.Layout.flatMap { case (k, n) => List.fill(n)(k) }
    kinds.foldLeft((List.empty[BoardSlot], List.empty[String], rng)) { case ((acc, used, r), kind) =>
      if (kind.rolledLvl) {
        val (lvl, r1) = r.between(SewerRates.MinLvl, SewerRates.MaxLvl + 1L)
        (acc :+ BoardSlot(kind, lvl = lvl), used, r1)
      } else if (!kind.needsRace) (acc :+ BoardSlot(kind), used, r)
      else {
        val pool        = Race.mortals.toList.map(_.entryName).filterNot(used.contains)
        val choices     = if (pool.isEmpty) Race.mortals.toList.map(_.entryName) else pool
        val (race, r1)  = r.pick(choices)
        (acc :+ BoardSlot(kind, Some(race)), used :+ race, r1)
      }
    } match { case (slots, _, _) => slots }
  }
}
