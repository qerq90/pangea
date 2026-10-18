package pangea.model.arena

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.battle.{BattleAlly, BattleEffects, HeroBattleState, SkillSlotState}
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

/** Режим боя на арене. Один на один отряд остаётся за оградой (на песок
  * выходит разве что крыса из-под ног), в групповом бою каждый выходит со своим
  * отрядом. Соперников сводят только в одном режиме. */
sealed trait ArenaMode extends EnumEntry
object ArenaMode extends Enum[ArenaMode] {
  val values: IndexedSeq[ArenaMode] = findValues

  case object Solo  extends ArenaMode
  case object Group extends ArenaMode

  implicit val encoder: Encoder[ArenaMode] = (m: ArenaMode) => m.entryName.asJson
  implicit val decoder: Decoder[ArenaMode] = (c: HCursor) =>
    c.as[String].map(s => ArenaMode.withNameOption(s).getOrElse(Solo))
}

/** Союзник стороны на песке. `id` — его номер на весь бой: по нему его узнают
  * в чужом зеркале боя, где он — «моб» (см. `MonsterSlot.arenaUnit`). Сам
  * союзник — как в лабиринте: место, раны, энергия. */
final case class ArenaAlly(id: Int, ally: BattleAlly)

object ArenaAlly {
  implicit val encoder: Encoder[ArenaAlly] = (a: ArenaAlly) =>
    Json.obj("id" -> a.id.asJson, "ally" -> a.ally.asJson)
  implicit val decoder: Decoder[ArenaAlly] = (c: HCursor) =>
    for {
      id   <- c.get[Int]("id")
      ally <- c.get[BattleAlly]("ally")
    } yield ArenaAlly(id, ally)
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
  seen:    Int                  = 0,
  // Сколько своих ходов сторона не сможет применить умение: её сбили с мысли
  // (Боевой клич). Считается на том, кого сбили, — в его зеркале боя этого
  // эффекта не видно, он висит в чужом.
  blocked: Int                  = 0,
  // Сторона подняла щит своим ходом (Блок): он режет удары и умения соперника
  // в его ходе. Читается в чужом зеркале — там эта сторона «моб».
  shieldUp: Boolean             = false,
  // Сторона взяла парирующий кинжал наизготовку (Контрудар): промах соперника в
  // его ходе — и она ответит. Тоже читается в чужом зеркале.
  counterUp: Boolean            = false,
  // Место бойца в своём ряду. В начале боя оба стоят друг напротив друга, на
  // первом; переставляют их «Переместиться», «Отскок» и чужой «Отбрасывающий
  // выстрел». В своём зеркале боец — герой на этом месте, в чужом — «моб».
  pos:     Int                  = 1,
  // Сколько своих ходов сторона не сможет двинуться: её пригвоздили
  // («Пригвоздить», «Заградительный залп»). Считается на том, кого
  // пригвоздили, как и `blocked`.
  rooted:  Int                  = 0,
  // Союзники стороны на песке: крысы из-под ног, в групповом бою — отряд.
  // Сторона падает, только когда пали все: и герой, и они.
  allies:  List[ArenaAlly]      = Nil,
  // Номер для следующего, кто выйдет на песок за эту сторону.
  nextAlly: Int                 = 1,
  // Что сторона наложила на союзников соперника — яд, кровь, огонь, путы, — по
  // их номерам. Живёт здесь и тикает в её ходы, как наложенное на самого
  // соперника живёт в `effects`.
  marks:   Map[Int, BattleEffects] = Map.empty
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
      "seen"    -> s.seen.asJson,
      "blocked" -> s.blocked.asJson,
      "shieldUp" -> s.shieldUp.asJson,
      "counterUp" -> s.counterUp.asJson,
      "pos"     -> s.pos.asJson,
      "rooted"  -> s.rooted.asJson,
      "allies"  -> s.allies.asJson,
      "nextAlly" -> s.nextAlly.asJson,
      "marks"   -> s.marks.asJson)

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
      blocked <- c.getOrElse[Int]("blocked")(0)
      shield  <- c.getOrElse[Boolean]("shieldUp")(false)
      counter <- c.getOrElse[Boolean]("counterUp")(false)
      pos     <- c.getOrElse[Int]("pos")(1)
      rooted  <- c.getOrElse[Int]("rooted")(0)
      allies  <- c.getOrElse[List[ArenaAlly]]("allies")(Nil)
      next    <- c.getOrElse[Int]("nextAlly")(1)
      marks   <- c.getOrElse[Map[Int, BattleEffects]]("marks")(Map.empty)
    } yield ArenaSide(UserId(userId), HeroId(heroId), name, lvl, effects, slots, buffs, ready, seen, blocked, shield,
      counter, pos, rooted, allies, next, marks)
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
  winner:   Option[UserId] = None,
  // Один на один или группа на группу (см. ArenaMode).
  mode:     ArenaMode      = ArenaMode.Solo
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

  /** Итог прочитали обе стороны — строку можно убирать. */
  def readByAll: Boolean = sides.forall(_.seen >= turnNo)

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
      "winner"   -> f.winner.map(_.value).asJson,
      "mode"     -> f.mode.asJson)

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
      mode     <- c.getOrElse[ArenaMode]("mode")(ArenaMode.Solo)
    } yield ArenaFight(id, code, status, a, b, turn.map(UserId(_)), deadline, round,
                       open, lastLog, turnNo, winner.map(UserId(_)), mode)
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
