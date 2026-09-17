package pangea.model.squad

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}

/** Союзник в отряде: кто, на какой позиции, что с ним сейчас и до какого
 *  момента нанят (`hiredUntil`, epoch ms). Потолки считаются от уровня героя
 *  ([[AllyKind.stats]]), здесь — только текущее. */
final case class Ally(kind: AllyKind, position: Int, hp: Long, armor: Long, energy: Long, hiredUntil: Long = 0L) {
  def name: String = kind.name

  /** Найм истёк — отработал свой день. */
  def expired(nowMs: Long): Boolean = hiredUntil <= nowMs

  /** Полностью здоров на этом уровне героя. */
  def restored(lvl: Long): Ally = {
    val s = kind.stats(lvl)
    copy(hp = s.hp, armor = s.armor, energy = s.energy)
  }

  /** Текущее не выше потолков (герой мог понизиться — не бывает, но и выше не держим). */
  def clamped(lvl: Long): Ally = {
    val s = kind.stats(lvl)
    copy(hp = hp.min(s.hp).max(0L), armor = armor.min(s.armor).max(0L), energy = energy.min(s.energy).max(0L))
  }
}

object Ally {
  implicit val encoder: Encoder[Ally] = deriveEncoder
  implicit val decoder: Decoder[Ally] = (c: HCursor) =>
    for {
      kind     <- c.get[AllyKind]("kind")
      position <- c.get[Int]("position")
      hp       <- c.getOrElse[Long]("hp")(0L)
      armor    <- c.getOrElse[Long]("armor")(0L)
      energy   <- c.getOrElse[Long]("energy")(0L)
      until    <- c.getOrElse[Long]("hiredUntil")(0L)
    } yield Ally(kind, position, hp, armor, energy, until)
}

/** Отряд героя: позиции 1..4 делят герой (`heroPos`) и союзники. В бою позиция
 *  — это место в строю напротив врагов: союзник на позиции N стоит против
 *  врага на месте N и достаёт соседние места. `away` — кто ушёл по свитку и
 *  когда вернётся в таверну (ключ — вид); `offDuty` — кто отработал свой найм и
 *  когда снова сядет за стол. Хранится в `heroes.squad_data`. */
final case class Squad(
  heroPos: Int              = 1,
  allies:  List[Ally]       = Nil,
  away:    Map[String, Long] = Map.empty,
  offDuty: Map[String, Long] = Map.empty
) {
  def isEmpty: Boolean  = allies.isEmpty
  def nonEmpty: Boolean = allies.nonEmpty

  def has(kind: AllyKind): Boolean = allies.exists(_.kind == kind)

  def allyAt(pos: Int): Option[Ally] = allies.find(_.position == pos)

  /** В отлучке — по свитку или после отработанного найма. */
  def isAway(kind: AllyKind, nowMs: Long): Boolean =
    away.get(kind.entryName).exists(_ > nowMs) || offDuty.get(kind.entryName).exists(_ > nowMs)

  /** Кто вернулся из отлучки по свитку к этому моменту — им нужна реплика. */
  def returned(nowMs: Long): List[AllyKind] =
    away.collect { case (k, until) if until <= nowMs => AllyKind.withNameOption(k) }.flatten.toList

  /** Стереть записи об отлучке (реплика показана / срок вышел). */
  def welcomeBack(kind: AllyKind): Squad = copy(away = away - kind.entryName, offDuty = offDuty - kind.entryName)

  /** Первая свободная позиция, если есть. */
  def freePosition: Option[Int] =
    (1 to AllyRates.Positions).find(p => p != heroPos && allyAt(p).isEmpty)

  /** Нанять на [[AllyRates.HireMs]]: на первую свободную позицию, полностью здоровым. */
  def hire(kind: AllyKind, lvl: Long, nowMs: Long): Squad =
    if (has(kind)) this
    else freePosition.fold(this)(p =>
      copy(allies = allies :+ Ally(kind, p, 0L, 0L, 0L, hiredUntil = nowMs + AllyRates.HireMs).restored(lvl)))

  /** Кто отработал свой найм к этому моменту: они уходят из отряда и сядут за
    * стол снова через [[AllyRates.OffDutyMs]]. Возвращает отряд и ушедших. */
  def expire(nowMs: Long): (Squad, List[AllyKind]) = {
    val gone = allies.filter(_.expired(nowMs)).map(_.kind)
    if (gone.isEmpty) (this, Nil)
    else (copy(allies = allies.filterNot(_.expired(nowMs)),
               offDuty = offDuty ++ gone.map(k => k.entryName -> (nowMs + AllyRates.OffDutyMs))), gone)
  }

  def dismiss(kind: AllyKind): Squad = copy(allies = allies.filterNot(_.kind == kind))

  /** Союзник ушёл по свитку: из отряда — вон, вернётся через сутки. */
  def sentAway(kind: AllyKind, nowMs: Long): Squad =
    dismiss(kind).copy(away = away.updated(kind.entryName, nowMs + AllyRates.AwayMs))

  /** Переставить союзника на позицию `pos`: занята другим — меняются местами,
    * занята героем — герой встаёт на его прежнюю. */
  def move(kind: AllyKind, pos: Int): Squad =
    allies.find(_.kind == kind) match {
      case None => this
      case Some(a) if pos < 1 || pos > AllyRates.Positions || pos == a.position => this
      case Some(a) =>
        val from = a.position
        if (pos == heroPos)
          copy(heroPos = from, allies = allies.map(x => if (x.kind == kind) x.copy(position = pos) else x))
        else
          copy(allies = allies.map { x =>
            if (x.kind == kind) x.copy(position = pos)
            else if (x.position == pos) x.copy(position = from)
            else x
          })
    }

  /** Герой встаёт на позицию `pos`: союзник оттуда — на его прежнюю. */
  def moveHero(pos: Int): Squad =
    if (pos < 1 || pos > AllyRates.Positions || pos == heroPos) this
    else copy(heroPos = pos, allies = allies.map(x => if (x.position == pos) x.copy(position = heroPos) else x))

  def update(kind: AllyKind)(f: Ally => Ally): Squad =
    copy(allies = allies.map(a => if (a.kind == kind) f(a) else a))

  /** Все живы и здоровы — после отдыха. */
  def restored(lvl: Long): Squad = copy(allies = allies.map(_.restored(lvl)))

  /** По позициям. */
  def inOrder: List[Ally] = allies.sortBy(_.position)
}

object Squad {
  val empty: Squad = Squad()

  implicit val encoder: Encoder[Squad] = deriveEncoder
  implicit val decoder: Decoder[Squad] = (c: HCursor) =>
    for {
      heroPos <- c.getOrElse[Int]("heroPos")(1)
      allies  <- c.getOrElse[List[Ally]]("allies")(Nil)
      away    <- c.getOrElse[Map[String, Long]]("away")(Map.empty)
      offDuty <- c.getOrElse[Map[String, Long]]("offDuty")(Map.empty)
    } yield Squad(heroPos, allies, away, offDuty)

  implicit val meta: Meta[Squad] = new Meta(pgDecoderGet, pgEncoderPut)
}
