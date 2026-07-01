package pangea.model.stats

import doobie._
import doobie.postgres.circe.jsonb.implicits._
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

/** Процентная прибавка к характеристикам (в процентах: 15 == +15%).
 *  Чтобы разрешить баффать новую характеристику — добавь сюда поле, примени его
 *  в соответствующем `effective*`-расчёте [[pangea.model.hero.Hero]] и обнови
 *  существующие конструкторы `ParamsBuff` (их немного). */
case class ParamsBuff(str: Int, vit: Int, agi: Int, int: Int)

object ParamsBuff {
  val zero: ParamsBuff = ParamsBuff(0, 0, 0, 0)

  implicit val encoder: Encoder[ParamsBuff] = deriveEncoder
  implicit val decoder: Decoder[ParamsBuff] = deriveDecoder
}

/** Один активный баф: `name` — идентичность источника (повтор бафа с тем же именем
 *  не стакается, а вытесняет прежний), `buff` — сама прибавка, `until` — момент
 *  времени (ms), до которого баф действует. */
case class StatBoost(name: String, buff: ParamsBuff, until: Long)

object StatBoost {
  implicit val encoder: Encoder[StatBoost] = deriveEncoder
  implicit val decoder: Decoder[StatBoost] = deriveDecoder
}

/** Набор временных бафов героя — любого происхождения, не только зелья Густаво.
 *  Хранится в `heroes.stat_boosts`. Активные бафы суммируются по каждой характеристике. */
case class StatBoosts(boosts: List[StatBoost]) {

  def active(nowMs: Long): List[StatBoost] = boosts.filter(_.until > nowMs)

  def hasActive(name: String, nowMs: Long): Boolean =
    boosts.exists(b => b.name == name && b.until > nowMs)

  def remainingMs(name: String, nowMs: Long): Option[Long] =
    boosts.collect { case b if b.name == name && b.until > nowMs => b.until - nowMs }.maxOption

  private def sumPct(nowMs: Long)(sel: ParamsBuff => Int): Int =
    active(nowMs).map(b => sel(b.buff)).sum

  def strFactor(nowMs: Long): Double = 1.0 + sumPct(nowMs)(_.str) / 100.0
  def vitFactor(nowMs: Long): Double = 1.0 + sumPct(nowMs)(_.vit) / 100.0
  def agiFactor(nowMs: Long): Double = 1.0 + sumPct(nowMs)(_.agi) / 100.0
  def intFactor(nowMs: Long): Double = 1.0 + sumPct(nowMs)(_.int) / 100.0

  /** Добавляет баф, попутно выкидывая протухшие и прежний баф с тем же именем. */
  def add(boost: StatBoost, nowMs: Long): StatBoosts =
    StatBoosts(boosts.filter(b => b.until > nowMs && b.name != boost.name) :+ boost)
}

object StatBoosts {
  val none: StatBoosts = StatBoosts(Nil)

  implicit val encoder: Encoder[StatBoosts] = deriveEncoder
  implicit val decoder: Decoder[StatBoosts] = deriveDecoder

  implicit val meta: Meta[StatBoosts] = new Meta(pgDecoderGet, pgEncoderPut)
}
