package pangea.model.arena

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.battle.{BattleEffects, HeroBattleState, SkillSlotState}
import pangea.model.hero.HeroId
import pangea.model.user.UserId

/** Что с боем на арене прямо сейчас. */
sealed trait ArenaStatus extends EnumEntry
object ArenaStatus extends Enum[ArenaStatus] {
  val values: IndexedSeq[ArenaStatus] = findValues

  /** Герой записался и ждёт соперника — по коду или из списка. */
  case object Waiting extends ArenaStatus

  /** Оба на песке, бой идёт. */
  case object Fighting extends ArenaStatus

  /** Всё кончено. Строка живёт, пока проигравшая сторона не прочитает итог:
    * иначе она осталась бы с экраном боя и без объяснений. */
  case object Finished extends ArenaStatus

  implicit val encoder: Encoder[ArenaStatus] = (s: ArenaStatus) => s.entryName.asJson
  implicit val decoder: Decoder[ArenaStatus] = (c: HCursor) => c.as[String].map(ArenaStatus.withName)
}

/** Одна сторона боя. HP, брони и энергии здесь нет: они живут в самом герое,
  * туда их пишет боевой движок, и обе стороны видят одни и те же числа.
  *
  * Здесь — только то, чего у героя вне боя не бывает: что он успел наложить и
  * поймать, какие умения на перезарядке и какие бафы на нём висят. Набор
  * эффектов у каждой стороны свой: в её зеркале боя она герой, а соперник —
  * «моб», поэтому яд, наложенный ею, живёт у неё в `monster`-полях и тикает в
  * её же ходы.
  *
  * @param name  имя героя на песке: соперник видит его вместо клички моба
  * @param ready сторона уже сходила хотя бы раз — по этому видно, начался ли бой
  */
final case class ArenaSide(
  userId:  UserId,
  heroId:  HeroId,
  name:    String,
  lvl:     Long,
  effects: BattleEffects        = BattleEffects.empty,
  slots:   List[SkillSlotState] = Nil,
  buffs:   HeroBattleState      = HeroBattleState.empty,
  ready:   Boolean              = false,
  // Номер хода, чей лог сторона уже прочитала: по нему видно, что показать.
  seen:    Int                  = 0
)

object ArenaSide {
  implicit val encoder: Encoder[ArenaSide] = (s: ArenaSide) =>
    Json.obj(
      "userId"  -> s.userId.value.asJson,
      "heroId"  -> s.heroId.value.asJson,
      "name"    -> s.name.asJson,
      "lvl"     -> s.lvl.asJson,
      "effects" -> s.effects.asJson,
      "slots"   -> s.slots.asJson,
      "buffs"   -> s.buffs.asJson,
      "ready"   -> s.ready.asJson,
      "seen"    -> s.seen.asJson)

  implicit val decoder: Decoder[ArenaSide] = (c: HCursor) =>
    for {
      userId  <- c.get[Long]("userId")
      heroId  <- c.get[Long]("heroId")
      name    <- c.getOrElse[String]("name")("Боец")
      lvl     <- c.getOrElse[Long]("lvl")(1L)
      effects <- c.getOrElse[BattleEffects]("effects")(BattleEffects.empty)
      slots   <- c.getOrElse[List[SkillSlotState]]("slots")(Nil)
      buffs   <- c.getOrElse[HeroBattleState]("buffs")(HeroBattleState.empty)
      ready   <- c.getOrElse[Boolean]("ready")(false)
      seen    <- c.getOrElse[Int]("seen")(0)
    } yield ArenaSide(UserId(userId), HeroId(heroId), name, lvl, effects, slots, buffs, ready, seen)
}

/** Бой на арене: двое, чей сейчас ход и до какой минуты он думает.
  *
  * Пока соперника нет, строка живёт с одним бойцом и своим кодом — его герой
  * передаёт тому, с кем договорился. Записавшийся виден и в списке ждущих:
  * код нужен лишь для того, чтобы выбрать своего среди чужих.
  */
final case class ArenaFight(
  id:       Long,
  code:     String,
  status:   ArenaStatus,
  a:        ArenaSide,
  b:        Option[ArenaSide],
  turn:     Option[UserId] = None,
  deadline: Long           = 0L,
  round:    Int            = 1,
  // Открытая запись видна в «ближайшем бою»; запись по коду — только тому,
  // кому код передали.
  open:     Boolean        = false,
  // Чем кончился последний ход и какой он по счёту: соперник читает это,
  // когда его зовут к экрану.
  lastLog:  List[String]   = Nil,
  turnNo:   Int            = 0,
  winner:   Option[UserId] = None
) {
  def sides: List[ArenaSide] = a :: b.toList

  def has(userId: UserId): Boolean = sides.exists(_.userId == userId)

  def sideOf(userId: UserId): Option[ArenaSide] = sides.find(_.userId == userId)

  /** Кто напротив: у ждущего в одиночестве напротив пока никого. */
  def foeOf(userId: UserId): Option[ArenaSide] = sides.find(_.userId != userId)

  def isTurn(userId: UserId): Boolean = turn.contains(userId)

  def waiting: Boolean = status == ArenaStatus.Waiting

  def finished: Boolean = status == ArenaStatus.Finished

  /** Показывать ли этот лог стороне: свой ход она и так видела. */
  def unseenFor(userId: UserId): Boolean =
    lastLog.nonEmpty && sideOf(userId).exists(_.seen < turnNo)

  /** Отметить, что сторона прочитала последний лог. */
  def seenBy(userId: UserId): ArenaFight =
    sideOf(userId).fold(this)(s => withSide(s.copy(seen = turnNo)))

  /** Записать ход: что случилось и кто теперь ходит. */
  def withLog(lines: List[String]): ArenaFight =
    copy(lastLog = lines.filter(_.nonEmpty), turnNo = turnNo + 1)

  def withSide(side: ArenaSide): ArenaFight =
    if (a.userId == side.userId) copy(a = side)
    else if (b.exists(_.userId == side.userId)) copy(b = Some(side))
    else this

  /** Ход переходит другому: срок думать — свой у каждого хода. */
  def passTurn(to: UserId, nowMs: Long): ArenaFight =
    copy(turn = Some(to), deadline = nowMs + ArenaRates.TurnMs,
         round = if (a.userId == to) round + 1 else round)
}

object ArenaFight {
  implicit val encoder: Encoder[ArenaFight] = (f: ArenaFight) =>
    Json.obj(
      "id"       -> f.id.asJson,
      "code"     -> f.code.asJson,
      "status"   -> f.status.asJson,
      "a"        -> f.a.asJson,
      "b"        -> f.b.asJson,
      "turn"     -> f.turn.map(_.value).asJson,
      "deadline" -> f.deadline.asJson,
      "round"    -> f.round.asJson,
      "open"     -> f.open.asJson,
      "lastLog"  -> f.lastLog.asJson,
      "turnNo"   -> f.turnNo.asJson,
      "winner"   -> f.winner.map(_.value).asJson)

  implicit val decoder: Decoder[ArenaFight] = (c: HCursor) =>
    for {
      id       <- c.getOrElse[Long]("id")(0L)
      code     <- c.getOrElse[String]("code")("")
      status   <- c.getOrElse[ArenaStatus]("status")(ArenaStatus.Waiting)
      a        <- c.get[ArenaSide]("a")
      b        <- c.getOrElse[Option[ArenaSide]]("b")(None)
      turn     <- c.getOrElse[Option[Long]]("turn")(None)
      deadline <- c.getOrElse[Long]("deadline")(0L)
      round    <- c.getOrElse[Int]("round")(1)
      open     <- c.getOrElse[Boolean]("open")(false)
      lastLog  <- c.getOrElse[List[String]]("lastLog")(Nil)
      turnNo   <- c.getOrElse[Int]("turnNo")(0)
      winner   <- c.getOrElse[Option[Long]]("winner")(None)
    } yield ArenaFight(id, code, status, a, b, turn.map(UserId(_)), deadline, round,
                       open, lastLog, turnNo, winner.map(UserId(_)))
}

/** Числа арены. Отдельно от модели: их читают и состояние, и бой, и тесты. */
object ArenaRates {

  /** Сколько герой думает над ходом. Просрочил — бьёт обычной атакой, и ход
    * уходит сопернику. */
  val TurnMs: Long = 60L * 1000L

  /** Длина кода записи. Четыре знака: короче путаются, длиннее не диктуются. */
  val CodeLength: Int = 4

  /** Сколько ждущих показывать в списке: по одному в ряд, плюс ряды под свои
    * кнопки — больше клавиатура ВК не примет. */
  val ListSize: Int = 6
}
