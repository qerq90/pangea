package pangea.model.item

/** Показ одинаковых предметов одной строкой: «Надколотый аметист (3 шт)».
  *
  * Группировка ТОЛЬКО экранная. Каждый камень, каждая горсть пыли и каждый трофей
  * остаются отдельными предметами и занимают своё место — в сумке, в бочке, в
  * кубе. Три аметиста это три занятых слота, сколько бы кнопок их ни показывало.
  *
  * Складываются только те вещи, у которых нет собственной судьбы: камень (вид и
  * грейд), материал (вид), трофей (вид, раса и уровень). Экипировку не
  * складываем никогда — у двух одинаковых с виду мечей разные статы, гнёзда и
  * камни в них. */
object ItemStack {

  /** Ключ «это одно и то же». None — вещь уникальна и показывается сама по себе. */
  def key(item: Item): Option[String] = item.details match {
    case ItemDetails.Gem(g)             => Some(s"gem:${g.kind.entryName}:${g.grade}")
    case ItemDetails.Material(k)        => Some(s"mat:${k.entryName}")
    case ItemDetails.Trophy(race, kind) => Some(s"trophy:$race:${kind.entryName}:${item.lvl}")
    case _                              => None
  }

  /** Складывается ли эта вещь с себе подобными. */
  def stackable(item: Item): Boolean = key(item).isDefined

  /** Список для экрана: представитель группы и сколько таких же лежит рядом.
    * Порядок исходного списка сохраняется — группа встаёт на место первого
    * своего предмета, а действия идут по его id. */
  def grouped(items: List[Item]): List[(Item, Int)] = {
    val counts = items.flatMap(key).groupBy(identity).map { case (k, xs) => k -> xs.size }
    val seen   = scala.collection.mutable.Set.empty[String]
    items.flatMap { item =>
      key(item) match {
        case None                       => Some(item -> 1)
        case Some(k) if seen.contains(k) => None
        case Some(k)                    => seen += k; Some(item -> counts.getOrElse(k, 1))
      }
    }
  }

  /** Сколько таких же предметов лежит в списке (включая сам предмет). */
  def countOf(items: List[Item], item: Item): Int =
    key(item).fold(1)(k => items.count(i => key(i).contains(k)))

  /** Приписка к названию: «(3 шт)» для группы, пусто для одиночки. */
  def countSuffix(count: Int): String = if (count > 1) s" ($count шт)" else ""
}
