package pangea.model.battle

import enumeratum._
import pangea.model.monster.Race

/** Природа существа на поле боя: аура смотрит именно на неё, а не на то, кто за
  * кого воюет. Мёртвое кормится тьмой, живое от неё гниёт, а камню и пламени
  * всё равно — в них нет ни крови, ни смерти. */
sealed trait Nature extends EnumEntry

object Nature extends Enum[Nature] {
  val values: IndexedSeq[Nature] = findValues

  case object Undead  extends Nature
  case object Living   extends Nature
  case object Neither extends Nature

  /** Природа по расе. Герой-некромант на пороге 10 числится нежитью отдельно —
    * у него своя раса (см. `HeroSets.heroIsUndead`). */
  def of(race: Race): Nature = race match {
    case Race.Undead                      => Undead
    case Race.Elemental | Race.Construct  => Neither
    case _                                => Living
  }

  def of(race: String): Nature = Race.withNameOption(race).fold(Neither: Nature)(of)
}

/** Числа аур — отдельно от компаньона [[Aura]], как [[pangea.model.item.SetRates]]:
  * вариант enum не должен читать значения из своего же компаньона, пока тот
  * собирает `values`. */
object AuraRates {

  /** Доля макс. HP, которую миазмы снимают или возвращают сами по себе. */
  val MiasmaPct: Long = 2L

  /** Сколько текущей энергии уходит на миазмы за раунд, в %. Это же число
    * прибавляется к их силе: сколько вложил, столько и тянет. */
  val MiasmaEnergyCostPct: Long = 10L

  /** Ниже этого запаса энергии держать миазмы нечем — они оседают. */
  val MiasmaMinEnergy: Long = 10L

  /** Сколько раундов вылеченный не чувствует ауры. */
  val MiasmaCalmRounds: Int = 3
}

/** Аура: эффект, который держит ВСЁ ПОЛЕ БОЯ и достаётся каждому, кто на нём
  * стоит, — не цели, а полю. Своих от чужих аура не отличает: ей важна лишь
  * природа существа ([[Nature]]).
  *
  * Держит её герой, и держит силой: за раунд уходит доля энергии, и она же идёт
  * в силу ауры. Кончилась энергия — аура оседает.
  *
  * Сейчас аура одна — миазмы тьмы «Некроманта» (порог 10); механика сделана
  * общей, чтобы следующая встала рядом. */
sealed abstract class Aura(
  val key: String,
  /** Доля макс. HP, которую аура даёт или отнимает сама по себе. */
  val pct: Long,
  /** Какую часть текущей энергии герой тратит на неё за раунд, в %. */
  val energyCostPct: Long,
  /** Ниже этого запаса энергии аура не держится. */
  val minEnergy: Long,
  /** На сколько раундов лечение выводит из-под ауры. */
  val calmRounds: Int
) extends EnumEntry {

  /** Сколько аура даст или возьмёт у того, у кого столько максимума HP: своя
    * доля плюс всё, что герой в неё вложил. */
  def power(maxHp: Long, spent: Long): Long = (maxHp * pct / 100L).max(0L) + spent

  /** Сколько энергии уйдёт на ауру при таком запасе — не меньше единицы. */
  def cost(energy: Long): Long = (energy * energyCostPct / 100L).max(1L)

  /** Чем аура обернётся для такой природы: +1 — лечит, −1 — вредит, 0 — мимо. */
  def sign(nature: Nature): Int
}

object Aura extends Enum[Aura] {
  val values: IndexedSeq[Aura] = findValues

  /** Миазмы тьмы: мёртвое крепнет, живое гниёт, камню и пламени всё равно. */
  case object Miasma extends Aura("miasma", AuraRates.MiasmaPct, AuraRates.MiasmaEnergyCostPct,
    AuraRates.MiasmaMinEnergy, AuraRates.MiasmaCalmRounds) {
    def sign(nature: Nature): Int = nature match {
      case Nature.Undead  =>  1
      case Nature.Living  => -1
      case Nature.Neither =>  0
    }
  }
}
