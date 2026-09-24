package pangea.test

import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.repository.artifact.{ArtifactRepoError, ArtifactRepository}
import zio.{IO, ZIO}

/** Ларец и сумка в памяти: те же правила, что в проде. */
class TestArtifactRepository(private var all: HeroArtifacts) extends ArtifactRepository {

  def get(heroId: HeroId): IO[ArtifactRepoError, HeroArtifacts] = ZIO.succeed(all)

  def upgrade(heroId: HeroId, kind: ArtifactKind): IO[ArtifactRepoError, Artifact] = {
    val cur = all.of(kind)
    if (!cur.canUpgrade) ZIO.fail(ArtifactRepoError.FullyUpgraded)
    else {
      val next = cur.copy(tier = cur.tier + 1,
        charges = if (cur.owned || !kind.hasMagic) cur.charges else HeroArtifacts.MaxCharges)
      all = all.updated(kind, next)
      ZIO.succeed(next)
    }
  }

  def put(heroId: HeroId, kind: ArtifactKind, item: Item): IO[ArtifactRepoError, Artifact] = {
    val cur = all.of(kind)
    if (!cur.owned)            ZIO.fail(ArtifactRepoError.NotOwned)
    else if (!kind.accepts(item)) ZIO.fail(ArtifactRepoError.WrongKind)
    else if (cur.freeSlots <= 0)  ZIO.fail(ArtifactRepoError.NoRoom)
    else {
      val next = cur.add(item)
      all = all.updated(kind, next)
      ZIO.succeed(next)
    }
  }

  def take(heroId: HeroId, kind: ArtifactKind, itemId: Long): IO[ArtifactRepoError, Item] = {
    val cur = all.of(kind)
    cur.items.data.find(_.id == itemId) match {
      case None => ZIO.fail(ArtifactRepoError.NoSuchItem)
      case Some(item) =>
        all = all.updated(kind, cur.withItems(cur.items.data.filterNot(_.id == itemId)))
        ZIO.succeed(item)
    }
  }

  def applyMagic(heroId: HeroId, kind: ArtifactKind, items: List[Item], chargesUsed: Int): IO[ArtifactRepoError, Artifact] = {
    val cur  = all.of(kind)
    val next = cur.withItems(items).copy(charges = (cur.charges - chargesUsed).max(0))
    all = all.updated(kind, next)
    ZIO.succeed(next)
  }

  def recharge(heroId: HeroId, kind: ArtifactKind): IO[ArtifactRepoError, Artifact] = {
    val cur = all.of(kind)
    if (!cur.owned) ZIO.fail(ArtifactRepoError.NotOwned)
    else {
      val next = cur.copy(charges = HeroArtifacts.MaxCharges)
      all = all.updated(kind, next)
      ZIO.succeed(next)
    }
  }

  def snapshot: HeroArtifacts = all
}

object TestArtifactRepository {
  private val heroId = HeroId(1L)

  def empty: TestArtifactRepository = new TestArtifactRepository(HeroArtifacts.empty(heroId))

  /** Готовые артефакты: ступень, заряды и содержимое. */
  def of(
    casket:   Artifact = Artifact.empty(ArtifactKind.Casket),
    bag:      Artifact = Artifact.empty(ArtifactKind.LivingBag),
    wardrobe: Artifact = Artifact.empty(ArtifactKind.Wardrobe)
  ): TestArtifactRepository = new TestArtifactRepository(HeroArtifacts(heroId, casket, bag, wardrobe))

  def artifact(
    kind:    ArtifactKind,
    tier:    Int,
    charges: Int        = HeroArtifacts.MaxCharges,
    items:   List[Item] = Nil
  ): Artifact = Artifact(kind, tier, charges, pangea.model.inventory.Inventory.Items(items))
}
