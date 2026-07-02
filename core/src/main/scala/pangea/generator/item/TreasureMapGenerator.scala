package pangea.generator.item

import pangea.model.item.{Item, ItemType, MapZone, Rarity}

/**
 * Генератор карт клада и их половинок. Зона (а значит имя и описание) выбирается
 * детерминированно по уровню героя ([[MapZone.forLevel]]) — RNG не нужен. Карты
 * легендарной (Orange) редкости: их никогда не считают «хламом» у торговца.
 * Возвращает шаблон с id = -1; id присваивает персист (см. `ItemRepository`).
 */
object TreasureMapGenerator {

  def create(heroLevel: Long, half: Boolean): Item = {
    val zone = MapZone.forLevel(heroLevel)
    Item(
      id            = -1L,
      name          = if (half) zone.halfName else zone.mapName,
      lvl           = heroLevel.max(1L).min(150L),
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
}
