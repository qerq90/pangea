package pangea.repository.artifact

import pangea.dao.artifact.ArtifactDao
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZIO, ZLayer}

sealed trait ArtifactRepoError
object ArtifactRepoError {
  /** Артефакт ещё не куплен у Фета. */
  case object NotOwned      extends ArtifactRepoError
  case object NoRoom        extends ArtifactRepoError
  case object WrongKind     extends ArtifactRepoError
  case object NoSuchItem    extends ArtifactRepoError
  /** Улучшать больше некуда: собран полностью. */
  case object FullyUpgraded extends ArtifactRepoError
  case object NoCharges     extends ArtifactRepoError
  case object Failed        extends ArtifactRepoError
}

/** Ларец Азата и Живая сумка. Деньги репозиторий не трогает: их считает
 *  состояние (дублоны за сборку, серебро за зарядку). */
trait ArtifactRepository {
  def get(heroId: HeroId): IO[ArtifactRepoError, HeroArtifacts]

  /** Купить артефакт или поднять его ступень (плату берёт вызывающий). */
  def upgrade(heroId: HeroId, kind: ArtifactKind): IO[ArtifactRepoError, Artifact]

  /** Положить вещь внутрь — если артефакт её принимает и место есть. */
  def put(heroId: HeroId, kind: ArtifactKind, item: Item): IO[ArtifactRepoError, Artifact]

  /** Достать вещь обратно. */
  def take(heroId: HeroId, kind: ArtifactKind, itemId: Long): IO[ArtifactRepoError, Item]

  /** Заменить содержимое и списать заряды — итог «магии Азата». */
  def applyMagic(heroId: HeroId, kind: ArtifactKind, items: List[Item], chargesUsed: Int): IO[ArtifactRepoError, Artifact]

  /** Зарядить до полного (серебро списывает вызывающий). */
  def recharge(heroId: HeroId, kind: ArtifactKind): IO[ArtifactRepoError, Artifact]
}

final class ArtifactRepositoryLive(dao: ArtifactDao) extends ArtifactRepository {

  def get(heroId: HeroId): IO[ArtifactRepoError, HeroArtifacts] =
    dao.getOrCreate(heroId).orElseFail(ArtifactRepoError.Failed)

  def upgrade(heroId: HeroId, kind: ArtifactKind): IO[ArtifactRepoError, Artifact] =
    for {
      all <- get(heroId)
      cur  = all.of(kind)
      _   <- ZIO.when(!cur.canUpgrade)(ZIO.fail(ArtifactRepoError.FullyUpgraded))
      // Первая ступень приходит заряженной: Фет отдаёт артефакт готовым к делу.
      next = cur.copy(tier = cur.tier + 1, charges = if (cur.owned) cur.charges else HeroArtifacts.MaxCharges)
      _   <- save(all.updated(kind, next))
    } yield next

  def put(heroId: HeroId, kind: ArtifactKind, item: Item): IO[ArtifactRepoError, Artifact] =
    for {
      all <- get(heroId)
      cur  = all.of(kind)
      _   <- ZIO.when(!cur.owned)(ZIO.fail(ArtifactRepoError.NotOwned))
      _   <- ZIO.when(!kind.accepts(item))(ZIO.fail(ArtifactRepoError.WrongKind))
      _   <- ZIO.when(cur.freeSlots <= 0)(ZIO.fail(ArtifactRepoError.NoRoom))
      next = cur.add(item)
      _   <- save(all.updated(kind, next))
    } yield next

  def take(heroId: HeroId, kind: ArtifactKind, itemId: Long): IO[ArtifactRepoError, Item] =
    for {
      all  <- get(heroId)
      cur   = all.of(kind)
      item <- ZIO.fromOption(cur.items.data.find(_.id == itemId)).orElseFail(ArtifactRepoError.NoSuchItem)
      _    <- save(all.updated(kind, cur.withItems(cur.items.data.filterNot(_.id == itemId))))
    } yield item

  def applyMagic(heroId: HeroId, kind: ArtifactKind, items: List[Item], chargesUsed: Int): IO[ArtifactRepoError, Artifact] =
    for {
      all <- get(heroId)
      cur  = all.of(kind)
      next = cur.withItems(items).copy(charges = (cur.charges - chargesUsed).max(0))
      _   <- save(all.updated(kind, next))
    } yield next

  def recharge(heroId: HeroId, kind: ArtifactKind): IO[ArtifactRepoError, Artifact] =
    for {
      all <- get(heroId)
      cur  = all.of(kind)
      _   <- ZIO.when(!cur.owned)(ZIO.fail(ArtifactRepoError.NotOwned))
      next = cur.copy(charges = HeroArtifacts.MaxCharges)
      _   <- save(all.updated(kind, next))
    } yield next

  private def save(all: HeroArtifacts): IO[ArtifactRepoError, Unit] =
    dao.update(all).orElseFail(ArtifactRepoError.Failed)
}

object ArtifactRepository {
  val live: ZLayer[ArtifactDao, Nothing, ArtifactRepository] =
    ZLayer.fromFunction(new ArtifactRepositoryLive(_))
}
