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

/** Кровотечение на цели: в конце раунда снимает `pct`% макс.HP. В ОТЛИЧИЕ от яда
 *  НЕ затухает со временем; повторное наложение стакает силу; любое лечение цели
 *  снимает его ПОЛНОСТЬЮ (см. [[BattleEffects]] и MonsterSkill.HealingFlask). */
case class Bleed(pct: Int) {
  def damageOn(maxHp: Long): Long = (maxHp.toDouble * pct / 100.0).toLong.max(0L)
  def stackedWith(add: Int): Bleed = Bleed(pct + add)
}

object Bleed {
  def of(pct: Int): Option[Bleed] = if (pct <= 0) None else Some(Bleed(pct))

  implicit val encoder: Encoder[Bleed] = deriveEncoder
  implicit val decoder: Decoder[Bleed] = deriveDecoder
}

/** Горение (стихия Огня) на цели: в конце раунда снимает `pct`% макс.HP ИГНОРИРУЯ
 *  броню, затем усиливается — до 10% по +2 п.п./раунд, далее по +1 п.п./раунд.
 *  Повторный поджог усиливает на +2 п.п. Любое лечение снимает горение до 0, при
 *  этом ослабляя само лечение на (50 + pct)% (см. MonsterSkill.HealingFlask). */
case class Burn(pct: Int) {
  def damageOn(maxHp: Long): Long = (maxHp.toDouble * pct / 100.0).toLong.max(0L)

  /** Рост в конце раунда: до порога — по `GrowthBelow`, после — по `GrowthAbove`. */
  def grown: Burn =
    Burn(pct + (if (pct < Burn.GrowthThreshold) Burn.GrowthBelow else Burn.GrowthAbove))

  /** Повторный поджог уже горящей цели: +`Reignite` п.п. */
  def reignited: Burn = Burn(pct + Burn.Reignite)

  /** На сколько % ослабляется лечение цели, пока она горит: 50 + текущий pct. */
  def healWeakenPct: Int = Burn.HealWeakenBase + pct
}

object Burn {
  val Initial: Int         = 2  // стартовый % при поджоге
  val GrowthThreshold: Int = 10
  val GrowthBelow: Int     = 2
  val GrowthAbove: Int     = 1
  val Reignite: Int        = 2
  val HealWeakenBase: Int  = 50

  /** Свежий поджог. */
  def onIgnite: Burn = Burn(Initial)

  /** Горение силы `pct`, либо None, если пламя выгорело (≤0). Через него лечение
   *  тратит горение: `HealWeakenBase` процентных пунктов уходит на ослабление
   *  самого лечения, а гореть остаётся излишек. */
  def of(pct: Int): Option[Burn] = if (pct <= 0) None else Some(Burn(pct))

  implicit val encoder: Encoder[Burn] = deriveEncoder
  implicit val decoder: Decoder[Burn] = deriveDecoder
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

/** Временный %-дебаф защиты цели на `turnsLeft` ходов (комбо Молния+Холод). */
case class TimedDefenceDebuff(pct: Int, turnsLeft: Int) {
  def ticked: Option[TimedDefenceDebuff] =
    if (turnsLeft <= 1) None else Some(copy(turnsLeft = turnsLeft - 1))
}

object TimedDefenceDebuff {
  implicit val encoder: Encoder[TimedDefenceDebuff] = deriveEncoder
  implicit val decoder: Decoder[TimedDefenceDebuff] = deriveDecoder
}

/** Тикающие статус-эффекты боя — вынесены из [[SoloPveBattle]], чтобы та не
 *  превращалась в god object. Все накладываются на моба (кроме регена/ядовитых
 *  атак героя). `monsterColdDefenceCut` — суммарное снижение итогового %-снижения
 *  урона цели от проков Холода (в п.п., не затухает). `monsterDefenceDebuff` —
 *  временный %-дебаф защиты (комбо Молния+Холод). `airBoostTurns` — сколько ходов
 *  ещё действует бонус уклонения/точности от прока Воздуха. */
case class BattleEffects(
  monsterPoison:        Option[Poison]             = None,
  monsterBleed:         Option[Bleed]              = None,
  monsterBurn:          Option[Burn]               = None,
  monsterColdDefenceCut: Int                       = 0,
  monsterDefenceDebuff: Option[TimedDefenceDebuff] = None,
  airBoostTurns:        Int                        = 0,
  heroRegen:            Option[Regen]              = None,
  // Горение НА ГЕРОЕ — первый негативный эффект в его сторону: раньше яд,
  // кровь и огонь существовали только в сторону моба. Поджигают огненный
  // элементаль и его шипы.
  heroBurn:             Option[Burn]               = None,
  heroPoisonousAttacks: Boolean                    = false,
  // Флаги «один раз за бой» набора «Охотник» (пороги 10 и 12).
  cancelSpent:          Boolean                    = false,
  doubleSpent:          Boolean                    = false,
  // Сколько ходов элементаль ещё скован холодом: шипы молчат, его атаки не
  // поджигают, точность срезана (см. MiniBoss.FireElemental.ChilledTurns).
  chilledTurns:         Int                        = 0,
  // ── Дебафы каменного элементаля на ГЕРОЕ ──────────────────────────────────
  // Залп валунов срезает защиту и уклонение, вязкая земля — точность и уклонение.
  heroStunnedTurns:     Int                        = 0,
  heroGroundedTurns:    Int                        = 0,
  // Яд НА ГЕРОЕ — им травит Гнилой Джо своим смрадом.
  heroPoison:           Option[Poison]             = None,
  // Моб бьёт слабее: подожжённый камень, поднявшийся Джо. Проценты у каждого
  // свои, поэтому храним и срок, и силу.
  monsterWeakenedTurns: Int                        = 0,
  monsterWeakenedPct:   Long                       = 0L,
  // Насколько поджоги уже срезали ПОТОЛОК брони моба. Текущая броня при этом не
  // трогается: она может остаться выше потолка, но выше него уже не чинится.
  monsterMaxArmorCut:   Long                       = 0L
) {

  /** Скован ли сейчас элементаль холодом. */
  def chilled: Boolean = chilledTurns > 0

  /** Сбита ли герою защита с уклонением залпом валунов. */
  def heroStunned: Boolean = heroStunnedTurns > 0

  /** Держит ли героя вязкая земля (точность и уклонение). */
  def heroGrounded: Boolean = heroGroundedTurns > 0

  /** Ослаблен ли урон моба горением. */
  def monsterWeakened: Boolean = monsterWeakenedTurns > 0
}

object BattleEffects {
  val empty: BattleEffects = BattleEffects()

  implicit val encoder: Encoder[BattleEffects] = deriveEncoder
  implicit val decoder: Decoder[BattleEffects] = deriveDecoder
}
