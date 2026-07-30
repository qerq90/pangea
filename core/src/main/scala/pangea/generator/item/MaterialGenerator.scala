package pangea.generator.item

import pangea.model.item.{Item, ItemDetails, ItemType, MaterialKind, Rarity}

/** Чистая фабрика предметов-материалов (мифрил и т.п.). Материалы не имеют уровня
 *  (формальный 1) и статов — только идентичность вида. */
object MaterialGenerator {

  def item(kind: MaterialKind): Item =
    Item(
      id = -1L,
      name = kind.displayName,
      lvl = 1L,
      rarity = Rarity.Gray,
      itemType = ItemType.Material,
      attack = 0,
      accuracy = 0,
      energy = 0,
      armor = 0,
      defence = 0,
      evasion = 0,
      details = ItemDetails.Material(kind)
    )

  /** Мифрил (для рецептов пересборки). Источник добычи — отдельный тикет. */
  def mithril: Item = item(MaterialKind.Mithril)
}
