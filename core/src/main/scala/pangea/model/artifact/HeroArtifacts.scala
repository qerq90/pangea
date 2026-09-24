package pangea.model.artifact

import enumeratum.{Enum, EnumEntry}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType

/** Сборные артефакты из Лавки Фета: Ларец Азата глотает драгоценные камни,
 *  Живая сумка — травы и отвары. Оба берут вещь прямо с добычи, держат её у
 *  себя (место в сумке при этом не тратится) и умеют одну «магию Азата» за
 *  заряд: ларец плавит три одинаковых камня в один категорией выше, сумка
 *  варит отвар из трав.
 *
 *  Различий между ними ровно два: что артефакт принимает и что делает магия, —
 *  поэтому вид описан здесь, а всё остальное у них общее. */
sealed abstract class ArtifactKind(
  val label:     String,
  val state:     StateType,
  /** Ключ в `scenes.yaml`: `artifact.casket.*` / `artifact.bag.*`. */
  val key:       String
) extends EnumEntry {
  /** Берёт ли артефакт эту вещь себе. */
  def accepts(item: Item): Boolean
}

object ArtifactKind extends Enum[ArtifactKind] {

  case object Casket extends ArtifactKind("Ларец Азата", StateType.Casket, "casket") {
    // Любой камень-усилитель, включая черепа и надколотые. Пыль невесома и
    // места нигде не занимает — её ларец не трогает.
    def accepts(item: Item): Boolean = item.gem.isDefined
  }

  case object LivingBag extends ArtifactKind("Живая сумка", StateType.LivingBag, "bag") {
    def accepts(item: Item): Boolean =
      item.itemType == ItemType.Brew || item.material.exists(_.isHerb)
  }

  val values: IndexedSeq[ArtifactKind] = findValues

  /** Кто первым возьмёт вещь себе. Виды не пересекаются, так что порядок важен
    * только для определённости. */
  def forItem(item: Item): Option[ArtifactKind] = values.find(_.accepts(item))
}

/** Один артефакт героя: ступень сборки, заряды и то, что внутри. */
final case class Artifact(tier: Int, charges: Int, items: Items) {
  def owned: Boolean = tier > 0

  /** Мест внутри: [[HeroArtifacts.SlotsPerTier]] за каждую ступень. */
  def capacity: Int = tier * HeroArtifacts.SlotsPerTier

  def occupied: Int = items.data.size

  def freeSlots: Int = (capacity - occupied).max(0)

  def hasRoom: Boolean = owned && freeSlots > 0

  def canUpgrade: Boolean = tier < HeroArtifacts.MaxTier

  def withItems(data: List[Item]): Artifact = copy(items = Items(data))

  def add(item: Item): Artifact = copy(items = Items(items.data :+ item))
}

object Artifact {
  val empty: Artifact = Artifact(0, 0, Items(Nil))
}

/** Артефакты героя одной записью (таблица `hero_artifacts`). */
final case class HeroArtifacts(heroId: HeroId, casket: Artifact, bag: Artifact) {

  def of(kind: ArtifactKind): Artifact = kind match {
    case ArtifactKind.Casket    => casket
    case ArtifactKind.LivingBag => bag
  }

  def updated(kind: ArtifactKind, artifact: Artifact): HeroArtifacts = kind match {
    case ArtifactKind.Casket    => copy(casket = artifact)
    case ArtifactKind.LivingBag => copy(bag = artifact)
  }

  /** Какой артефакт готов принять эту вещь прямо сейчас. */
  def keeperFor(item: Item): Option[ArtifactKind] =
    ArtifactKind.forItem(item).filter(k => of(k).hasRoom)
}

object HeroArtifacts {
  /** Сколько мест даёт одна ступень сборки: 15, 30, 45, 60. */
  val SlotsPerTier: Int = 15

  /** Покупка и три улучшения. */
  val MaxTier: Int = 4

  /** Цена покупки и каждого улучшения — в дублонах. */
  val StepPriceDoubloons: Long = 100L

  /** Максимум зарядов и цена полной зарядки у Жреца. */
  val MaxCharges: Int = 25
  val RechargeSilver: Long = 5000L

  def empty(heroId: HeroId): HeroArtifacts = HeroArtifacts(heroId, Artifact.empty, Artifact.empty)
}
