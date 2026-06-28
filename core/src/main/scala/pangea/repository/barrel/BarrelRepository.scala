package pangea.repository.barrel

import pangea.dao.barrel.BarrelDao
import pangea.model.barrel.Barrel
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZLayer}

trait BarrelRepository {
  def get(heroId: HeroId): IO[BarrelRepoError, Barrel]
  def deposit(heroId: HeroId, item: Item): IO[BarrelRepoError, Unit]
  def withdraw(heroId: HeroId, itemId: Long): IO[BarrelRepoError, Item]
  def depositGold(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit]
  def withdrawGold(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit]
}

object BarrelRepository {
  val live: ZLayer[BarrelDao, Nothing, BarrelRepository] =
    ZLayer.fromFunction(new BarrelRepositoryLive(_))
}
