package pangea.model.squad

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.stats.FightStats

/** Поднятый с алтаря тёмных сил: он не растёт с героем и не нанимается, а
 *  живёт тем, что было в трофее, — поэтому имя, уровень и потолки статов лежат
 *  прямо на нём. Раса у всех такая нежить одна (см. [[AllyKind.Undead]]), а
 *  имя остаётся от того, кем он был при жизни. */
final case class UndeadForm(name: String, lvl: Long, stats: FightStats)

object UndeadForm {
  implicit val encoder: Encoder[UndeadForm] = (u: UndeadForm) =>
    Json.obj("name" -> u.name.asJson, "lvl" -> u.lvl.asJson, "stats" -> u.stats.asJson)

  implicit val decoder: Decoder[UndeadForm] = (c: HCursor) =>
    for {
      name  <- c.get[String]("name")
      lvl   <- c.getOrElse[Long]("lvl")(1L)
      stats <- c.get[FightStats]("stats")
    } yield UndeadForm(name, lvl, stats)
}

/** Союзник в отряде: кто, на какой позиции, что с ним сейчас и до какого
 *  момента он при герое (`hiredUntil`, epoch ms). Потолки считаются от уровня
 *  героя ([[AllyKind.stats]]), здесь — только текущее. У поднятого с алтаря
 *  вместо этого своя форма ([[UndeadForm]]): его статы от уровня героя не
 *  зависят, а срок — не найм, а то, насколько хватит тёмной силы
 *  ([[AllyRates.UndeadMs]]). */
final case class Ally(
  kind:       AllyKind,
  position:   Int,
  hp:         Long,
  armor:      Long,
  energy:     Long,
  hiredUntil: Long               = 0L,
  undead:     Option[UndeadForm] = None
) {
  def name: String = undead.map(_.name).getOrElse(kind.name)

  /** Потолки статов: у наёмника — по уровню героя, у поднятого — свои. */
  def statsAt(lvl: Long): FightStats = undead.map(_.stats).getOrElse(kind.stats(lvl))

  /** Уровень, по которому он дерётся. */
  def lvlAt(heroLvl: Long): Long = undead.map(_.lvl).getOrElse(kind.effectiveLvl(heroLvl))

  /** Найм истёк — отработал свой день. Поднятый не уходит, а рассыпается:
    * у него свой срок, см. [[crumbled]]. */
  def expired(nowMs: Long): Boolean = undead.isEmpty && hiredUntil <= nowMs

  /** Тёмная сила в костях кончилась. Поднятый без срока — из тех, кого
    * подняли до того, как срок вообще завели: такому его ставит
    * [[Squad.settleUndead]], а не рассыпает на месте. */
  def crumbled(nowMs: Long): Boolean = undead.isDefined && hiredUntil > 0L && hiredUntil <= nowMs

  /** Уходит из отряда прямо сейчас — по любой из двух причин. */
  def leaving(nowMs: Long): Boolean = expired(nowMs) || crumbled(nowMs)

  /** Полностью здоров на этом уровне героя. */
  def restored(lvl: Long): Ally = {
    val s = statsAt(lvl)
    copy(hp = s.hp, armor = s.armor, energy = s.energy)
  }

  /** Текущее не выше потолков (герой мог понизиться — не бывает, но и выше не держим). */
  def clamped(lvl: Long): Ally = {
    val s = statsAt(lvl)
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
      undead   <- c.getOrElse[Option[UndeadForm]]("undead")(None)
    } yield Ally(kind, position, hp, armor, energy, until, undead)
}

/** Отряд героя: места в строю (см. [[AllyRates.Positions]]) делят герой
 *  (`heroPos`) и союзники — до десяти. В бою позиция
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

  /** Кто покидает отряд к этому моменту: наёмник отработал свой найм и сядет
    * за стол снова через [[AllyRates.OffDutyMs]], а поднятый рассыпался — его
    * ждать неоткуда. Возвращает отряд и ушедших целиком: наверху по ним
    * решают, что сказать игроку. */
  def expire(nowMs: Long): (Squad, List[Ally]) = {
    val gone = allies.filter(_.leaving(nowMs))
    if (gone.isEmpty) (this, Nil)
    else {
      val hired = gone.filter(_.undead.isEmpty).map(_.kind)
      (copy(allies = allies.filterNot(_.leaving(nowMs)),
            offDuty = offDuty ++ hired.map(k => k.entryName -> (nowMs + AllyRates.OffDutyMs))).compact, gone)
    }
  }

  /** Поднятые до того, как у них завёлся срок, получают его с этой минуты:
    * иначе они рассыпались бы все разом при первом же заходе героя. */
  def settleUndead(nowMs: Long): Squad =
    if (!allies.exists(a => a.undead.isDefined && a.hiredUntil <= 0L)) this
    else copy(allies = allies.map { a =>
      if (a.undead.isDefined && a.hiredUntil <= 0L) a.copy(hiredUntil = nowMs + AllyRates.UndeadMs) else a
    })

  def dismiss(kind: AllyKind): Squad = copy(allies = allies.filterNot(_.kind == kind)).compact

  /** Убрать союзника с этой позиции. Позиция — единственный надёжный ключ:
    * поднятых с алтаря в отряде может быть несколько, и вид их не различает. */
  def dismissAt(pos: Int): Squad = dismissAll(Set(pos))

  /** Убрать сразу всех с этих позиций: по одному нельзя — [[compact]] сдвигает
    * оставшихся, и вторая позиция указала бы уже не на того. */
  def dismissAll(positions: Set[Int]): Squad =
    if (positions.isEmpty) this
    else copy(allies = allies.filterNot(a => positions.contains(a.position))).compact

  def updateAt(pos: Int)(f: Ally => Ally): Squad =
    copy(allies = allies.map(a => if (a.position == pos) f(a) else a))

  /** Поднятый встаёт на свободное место — и держится [[AllyRates.UndeadMs]] с
    * этой минуты. Мест нет — отряд как был; заменой заведует [[replaceAt]]. */
  def raise(form: UndeadForm, lvl: Long, nowMs: Long): Squad =
    freePosition.fold(this)(p => copy(allies = allies :+ risen(form, p, lvl, nowMs)))

  /** Поднятый занимает место того, кто на нём стоял. */
  def replaceAt(pos: Int, form: UndeadForm, lvl: Long, nowMs: Long): Squad =
    copy(allies = allies.filterNot(_.position == pos) :+ risen(form, pos, lvl, nowMs)).compact

  private def risen(form: UndeadForm, pos: Int, lvl: Long, nowMs: Long): Ally =
    Ally(AllyKind.Undead, pos, 0L, 0L, 0L,
      hiredUntil = nowMs + AllyRates.UndeadMs, undead = Some(form)).restored(lvl)

  /** Все места заняты — новому нужно потеснить кого-то из своих. */
  def full: Boolean = freePosition.isEmpty

  /** Союзник ушёл по свитку: из отряда — вон, вернётся через сутки. */
  def sentAway(kind: AllyKind, nowMs: Long): Squad =
    dismiss(kind).copy(away = away.updated(kind.entryName, nowMs + AllyRates.AwayMs))

  /** Пустые позиции схлопываются: герой и союзники в прежнем порядке встают на
    * 1, 2, … — герой один всегда на 1, а не на месте, оставшемся от ушедших. */
  def compact: Squad = {
    val order = (None +: allies.map(Some(_))).sortBy {
      case None    => heroPos
      case Some(a) => a.position
    }
    val placed = order.zipWithIndex.map { case (who, i) => who -> (i + 1) }
    copy(
      heroPos = placed.collectFirst { case (None, p) => p }.getOrElse(1),
      allies  = placed.collect { case (Some(a), p) => a.copy(position = p) })
  }

  /** Переставить союзника с позиции `from` на позицию `pos`: занята другим —
    * меняются местами, занята героем — герой встаёт на его прежнюю. */
  def moveAt(from: Int, pos: Int): Squad =
    allies.find(_.position == from) match {
      case None => this
      case Some(_) if pos < 1 || pos > AllyRates.Positions || pos == from => this
      case Some(_) =>
        if (pos == heroPos)
          copy(heroPos = from, allies = allies.map(x => if (x.position == from) x.copy(position = pos) else x))
        else
          copy(allies = allies.map { x =>
            if (x.position == from) x.copy(position = pos)
            else if (x.position == pos) x.copy(position = from)
            else x
          })
    }

  /** То же по виду наёмника — им пользуются экраны, где союзник один такой. */
  def move(kind: AllyKind, pos: Int): Squad =
    allies.find(_.kind == kind).fold(this)(a => moveAt(a.position, pos))

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
