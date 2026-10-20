package pangea.model.hero

import enumeratum._

/** Зелья Фета: выпиваются сразу при покупке. Ключ — секция в `scenes.yaml`
  * (`fet.potions.<key>`). */
sealed abstract class FetPotion(val key: String) extends EnumEntry

object FetPotion extends Enum[FetPotion] {
  val values: IndexedSeq[FetPotion] = findValues

  /** Останавливает опыт: он не идёт ни из боёв, ни из заданий. */
  case object ExpStop extends FetPotion("expStop")

  /** Снова запускает опыт. */
  case object ExpStart extends FetPotion("expStart")

  /** Сбрасывает вложенные очки характеристик в свободные (см. Hero.respec). */
  case object Respec extends FetPotion("respec")

  /** Будет ли от зелья толк: остановить идущий опыт, запустить стоящий,
    * сбросить, если есть что. */
  def useful(potion: FetPotion, hero: Hero): Boolean = potion match {
    case ExpStop  => !hero.expFrozen
    case ExpStart => hero.expFrozen
    case Respec   => Hero.spentPoints(hero) > 0L
  }
}
