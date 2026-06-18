package pangea.repository.item

import pangea.dao.item.ItemDao
import pangea.domain.Rng
import pangea.model.hero.HeroId
import pangea.model.item.{Item, Rarity}
import zio.{Task, ZLayer}

trait ItemRepository {
  def generate(heroId: HeroId, lvl: Long, rarity: Rarity, rng: Rng): Task[Item]
  def persist(heroId: HeroId, item: Item): Task[Item]
}

object ItemRepository {
  val live: ZLayer[ItemDao, Nothing, ItemRepository] =
    ZLayer.fromFunction(new ItemRepositoryLive(_))
}
