package pangea.model.battle

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Тикающий яд на цели: в конце раунда снимает `pct`% макс.HP, затем слабеет на
 *  `DecayPerRound` п.п. до исчезновения. Повторное попадание стакает (+`OnHit`),
 *  лечение цели ослабляет на `HealCut`. Поведение — здесь, а не в BattleState. */
case class Poison(pct: Int) {
  def damageOn(maxHp: Long): Long = (maxHp.toDouble * pct / 100.0).toLong.max(0L)
  def decayed: Option[Poison]        = Poison.of(pct - Poison.DecayPerRound)
  def stacked: Poison                = Poison(pct + Poison.OnHit)
  def weakenedByHeal: Option[Poison] = Poison.of(pct - Poison.HealCut)
}

object Poison {
  val OnHit: Int         = 8
  val DecayPerRound: Int = 2
  val HealCut: Int       = 4

  /** Яд силы `pct`, либо None если сила исчерпана (≤0). */
  def of(pct: Int): Option[Poison] = if (pct <= 0) None else Some(Poison(pct))

  /** Свежее отравление от попадания. */
  def onHit: Poison = Poison(OnHit)

  implicit val encoder: Encoder[Poison] = deriveEncoder
  implicit val decoder: Decoder[Poison] = deriveDecoder
}

/** Тикающая регенерация героя: в конце раунда лечит `pct`% макс.HP, затем слабеет
 *  на `DecayPerRound` п.п. до исчезновения. */
case class Regen(pct: Int) {
  def healOn(maxHp: Long): Long = (maxHp.toDouble * pct / 100.0).toLong.max(0L)
  def decayed: Option[Regen]    = Regen.of(pct - Regen.DecayPerRound)
}

object Regen {
  val OnDrink: Int       = 12
  val DecayPerRound: Int = 2

  def of(pct: Int): Option[Regen] = if (pct <= 0) None else Some(Regen(pct))

  /** Свежая регенерация от выпитого зелья. */
  def onDrink: Regen = Regen(OnDrink)

  implicit val encoder: Encoder[Regen] = deriveEncoder
  implicit val decoder: Decoder[Regen] = deriveDecoder
}

/** Тикающие статус-эффекты боя — вынесены из [[SoloPveBattle]], чтобы та не
 *  превращалась в god object. Яд накладывается на моба, реген — на героя;
 *  пересечься на одной сущности не могут (см. правило «яд/реген взаимовычитаются»
 *  в ТЗ — при выбранной раскладке цель у них разная, поэтому не срабатывает).
 *  `heroPoisonousAttacks` — баф «ядовитые атаки» от зелья яда: висит на герое (потенциально
 *  до конца боя), пока герой хотя бы раз не ПОПАДЁТ обычной атакой; на попадании баф
 *  снимается, а моб травится только если удар прошёл в HP (см. `BattleState`). */
case class BattleEffects(
  monsterPoison:        Option[Poison] = None,
  heroRegen:            Option[Regen]  = None,
  heroPoisonousAttacks: Boolean        = false
)

object BattleEffects {
  val empty: BattleEffects = BattleEffects()

  implicit val encoder: Encoder[BattleEffects] = deriveEncoder
  implicit val decoder: Decoder[BattleEffects] = deriveDecoder
}
