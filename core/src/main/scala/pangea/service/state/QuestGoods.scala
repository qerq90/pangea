package pangea.service.state

import pangea.model.artifact.ArtifactKind
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Вещи героя, которые можно отдать по поручению или показать горожанину: сумка
  * и артефакты, что сами ловят добычу, — Ларец Азата (камни) и Живая сумка
  * (травы и отвары). Туда вещь попадает без спроса героя, и горожанин видит её
  * так же, как в сумке. Шкаф не в счёт: что в нём лежит, хозяин убрал сам. */
object QuestGoods {

  /** Вещь и где она лежит: в сумке (`in` пусто) или в артефакте. */
  final case class Good(item: Item, in: Option[ArtifactKind])

  /** Всё, что есть при герое: сперва сумка, потом артефакты — так отданное
    * первым делом освобождает сумку. Артефакты не прочитались — хватит и сумки. */
  def all(inventory: InventoryRepository, artifacts: Option[ArtifactRepository], heroId: HeroId): Task[List[Good]] =
    for {
      inv  <- inventory.get(heroId).mapError(e => new Throwable(e.toString))
      arts <- ZIO.foreach(artifacts)(_.get(heroId).option).map(_.flatten)
    } yield inv.items.data.filter(_.id != 0L).map(Good(_, None)) ++
      arts.toList.flatMap(all => ArtifactKind.values.filter(_.autoCollect).flatMap(kind =>
        all.of(kind).items.data.map(Good(_, Some(kind)))))

  /** Забрать отданное: из сумки — разом, из артефактов — по одной. */
  def remove(inventory: InventoryRepository, artifacts: Option[ArtifactRepository], heroId: HeroId,
             goods: List[Good]): Task[Unit] = {
    val fromBag = goods.collect { case Good(item, None) => item.id }.toSet
    val fromArt = goods.collect { case Good(item, Some(kind)) => kind -> item.id }
    ZIO.when(fromBag.nonEmpty)(inventory.removeItems(fromBag, heroId).mapError(e => new Throwable(e.toString))) *>
      ZIO.foreachDiscard(artifacts)(repo => ZIO.foreachDiscard(fromArt) { case (kind, id) =>
        repo.take(heroId, kind, id).mapError(e => new Throwable(e.toString))
      })
  }
}
