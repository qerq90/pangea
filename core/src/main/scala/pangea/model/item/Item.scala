package pangea.model.item

import doobie.Meta

case class Item(
  id: Long,
  itemType: ItemType,
  attack: Long,
  accuracy: Long,
  armor: Long,
  defence: Long,
  evasion: Long
)
