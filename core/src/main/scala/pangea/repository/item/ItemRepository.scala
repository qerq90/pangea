package pangea.repository.item

import pangea.model.item.{Item, ItemType}
import pangea.model.user.User
import zio.Task

trait ItemRepository {
  def createItem(user: User, types: List[ItemType]): Task[Item]
  def getItem(itemId: Long): Task[Item]
}
