package pangea.generator.item

import pangea.model.item.{Item, ItemType, MapZone, Rarity}

/**
 * Генератор карт клада и их половинок. У карты нет уровня: она несёт только зону
 * ([[MapZone]]), а зона сама задаёт диапазон уровней добычи (Кинэт 1–25 и т.д.).
 * Уровень нужен лишь в момент дропа — чтобы выбрать зону ([[MapZone.forLevel]]).
 * Карты легендарной (Orange) редкости: их не считают «хламом» у торговца.
 * Возвращает шаблон с id = -1; id присваивает персист (см. `ItemRepository`).
 */
object TreasureMapGenerator {

  /** Карта или половинка: зона выбирается по уровню дропа, сам уровень не хранится. */
  def create(dropLevel: Long, half: Boolean): Item =
    build(MapZone.forLevel(dropLevel), half)

  /** Целая карта конкретной зоны — сборка из двух половинок этой же зоны. */
  def full(zone: MapZone): Item =
    build(zone, half = false)

  private def build(zone: MapZone, half: Boolean): Item =
    Item(
      id            = -1L,
      name          = if (half) zone.halfName else zone.mapName,
      lvl           = 0L, // у карт уровня нет — диапазон добычи задаёт зона
      rarity        = Rarity.Orange,
      itemType      = if (half) ItemType.TreasureMapHalf else ItemType.TreasureMap,
      attack        = 0,
      accuracy      = 0,
      concentration = 0,
      armor         = 0,
      defence       = 0,
      evasion       = 0,
      mapZone       = Some(zone)
    )
}
