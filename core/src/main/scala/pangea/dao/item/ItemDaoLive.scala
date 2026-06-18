package pangea.dao.item

import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.item.Item
import zio.Task
import zio.interop.catz._

class ItemDaoLive(xa: Transactor[Task]) extends ItemDao {

  override def insert(heroId: Long, item: Item): Task[Long] =
    sql"""INSERT INTO items (hero_id, name, lvl, rarity, item_type, attack, accuracy, concentration, armor, defence, evasion)
          VALUES ($heroId, ${item.name}, ${item.lvl}, ${item.rarity.entryName}, ${item.itemType.entryName},
                  ${item.attack}, ${item.accuracy}, ${item.concentration}, ${item.armor}, ${item.defence}, ${item.evasion})
       """.update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(xa)
}
