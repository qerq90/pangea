package pangea.repository.item

import pangea.dao.item.ItemDao
import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.hero.HeroId
import pangea.model.item.{Item, Rarity}
import zio.Task

class ItemRepositoryLive(itemDao: ItemDao) extends ItemRepository {

  def generate(heroId: HeroId, lvl: Long, rarity: Rarity, rng: Rng): Task[Item] = {
    val (item, _) = ItemGenerator.createItem(lvl, rarity, rng)
    itemDao.insert(heroId.value, item).map(id => item.copy(id = id))
  }

  def persist(heroId: HeroId, item: Item): Task[Item] =
    itemDao.insert(heroId.value, item).map(id => item.copy(id = id))
}
