package pangea.test

import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.hero.HeroId
import pangea.model.item.{Item, Rarity}
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

class TestItemRepository extends ItemRepository {
  private var nextId = 1L

  def generate(heroId: HeroId, lvl: Long, rarity: Rarity, rng: Rng): Task[Item] = {
    val (item, _) = ItemGenerator.createItem(lvl, rarity, rng)
    ZIO.succeed(assignId(item))
  }

  def persist(heroId: HeroId, item: Item): Task[Item] =
    ZIO.succeed(assignId(item))

  private def assignId(item: Item): Item = {
    val id = nextId
    nextId += 1
    item.copy(id = id)
  }
}

object TestItemRepository {
  def make: TestItemRepository = new TestItemRepository
}
