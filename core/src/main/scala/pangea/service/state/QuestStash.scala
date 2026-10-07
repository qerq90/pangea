package pangea.service.state

import pangea.model.artifact.ArtifactKind
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Где задания ищут вещи: сумка, Ларец Азата и Живая сумка. В Миниатюрный шкаф
  * не смотрят ни при каких обстоятельствах — что убрано туда, убрано нарочно.
  *
  * Без ларца и сумки задание можно было и вовсе не выполнить: оба ловят добычу
  * сами, минуя сумку ([[ArtifactKind.autoCollect]]), так что склянки гномьего
  * заказа улетали в Живую сумку — и герой, у которого она есть, не мог собрать в
  * сумке ни одной. */
object QuestStash {

  /** Вещь и то, где она лежит: `None` — в сумке героя. */
  final case class Found(item: Item, where: Option[ArtifactKind])

  /** Хранилища, куда задания заглядывают, в порядке, в котором из них берут:
    * сперва сумка, потом ларец и живая сумка. Шкафа тут нет и не будет. */
  val Searched: List[ArtifactKind] = List(ArtifactKind.Casket, ArtifactKind.LivingBag)

  /** Всё, что задание может зачесть. Пустые места сумки (`id == 0`) сюда не идут. */
  def findAll(
      inventoryRepo: InventoryRepository,
      artifactRepo:  ArtifactRepository,
      hero:          Hero
  ): Task[List[Found]] =
    for {
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      arts <- artifactRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      bag   = inv.items.data.filter(_.id != 0L).map(Found(_, None))
      kept  = Searched.flatMap(k => arts.of(k).items.data.map(Found(_, Some(k))))
    } yield bag ++ kept

  /** Вещи, которые задание зачтёт по своему признаку. */
  def find(
      inventoryRepo: InventoryRepository,
      artifactRepo:  ArtifactRepository,
      hero:          Hero
  )(p: Item => Boolean): Task[List[Found]] =
    findAll(inventoryRepo, artifactRepo, hero).map(_.filter(f => p(f.item)))

  /** Забрать сданное — каждую вещь оттуда, где она лежала. */
  def take(
      inventoryRepo: InventoryRepository,
      artifactRepo:  ArtifactRepository,
      hero:          Hero,
      taken:         List[Found]
  ): Task[Unit] = {
    val (fromBag, fromArtifacts) = taken.partition(_.where.isEmpty)
    val bagIds = fromBag.map(_.item.id).toSet
    ZIO.when(bagIds.nonEmpty)(
      inventoryRepo.removeItems(bagIds, hero.id).mapError(e => new Throwable(e.toString))) *>
      ZIO.foreachDiscard(fromArtifacts)(f =>
        artifactRepo.take(hero.id, f.where.get, f.item.id).mapError(e => new Throwable(e.toString)))
  }
}
