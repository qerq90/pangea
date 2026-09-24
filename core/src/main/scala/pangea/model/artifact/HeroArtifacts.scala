package pangea.model.artifact

import enumeratum.{Enum, EnumEntry}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType

/** Сборные артефакты из Лавки Фета. Ларец Азата глотает драгоценные камни,
 *  Живая сумка — травы и отвары, Миниатюрный шкаф носит что угодно, лишь бы
 *  оно занимало место. Общее у всех: вещи внутри не тратят слотов сумки, за
 *  дублоны артефакт собирается дальше и становится вместительнее.
 *
 *  Различаются они тремя вещами — что берут, сколько мест даёт ступень и есть
 *  ли у них «магия Азата» на зарядах, — поэтому вид описан здесь, а всё
 *  остальное у них общее. */
sealed abstract class ArtifactKind(
  val label:        String,
  val state:        StateType,
  /** Ключ в `scenes.yaml`: `artifact.casket.*`, `artifact.bag.*`, `artifact.wardrobe.*`. */
  val key:          String,
  /** Сколько мест добавляет каждая ступень сборки. */
  val slotsPerTier: Int,
  /** Есть ли у артефакта магия на зарядах (кнопка и зарядка у Жреца). */
  val hasMagic:     Boolean,
  /** Ловит ли вещь прямо с добычи, минуя сумку. */
  val autoCollect:  Boolean,
  /** Уцелеет ли содержимое при смерти. Ларец и сумка — нет: их добро уходит с
    * тем же броском, что и вещи из сумки (см. `DeathState.dropItems`). */
  val safeFromDeath: Boolean
) extends EnumEntry {
  /** Берёт ли артефакт эту вещь себе. */
  def accepts(item: Item): Boolean

  def capacityAt(tier: Int): Int = tier * slotsPerTier
}

object ArtifactKind extends Enum[ArtifactKind] {

  case object Casket extends ArtifactKind("Ларец Азата", StateType.Casket, "casket",
    slotsPerTier = 15, hasMagic = true, autoCollect = true, safeFromDeath = false) {
    // Любой камень-усилитель, включая черепа и надколотые. Пыль невесома и
    // места нигде не занимает — её ларец не трогает.
    def accepts(item: Item): Boolean = item.gem.isDefined
  }

  case object LivingBag extends ArtifactKind("Живая сумка", StateType.LivingBag, "bag",
    slotsPerTier = 15, hasMagic = true, autoCollect = true, safeFromDeath = false) {
    def accepts(item: Item): Boolean =
      item.itemType == ItemType.Brew || item.material.exists(_.isHerb)
  }

  /** Шкаф-брелок: места в нём мало, зато они как слоты сумки, и единственный из
    * трёх он спасает добро от смерти — брелок с тела не снимают. Сам он с
    * добычи ничего не ловит: что положить, хозяин решает сам. */
  case object Wardrobe extends ArtifactKind("Миниатюрный шкаф", StateType.Wardrobe, "wardrobe",
    slotsPerTier = 3, hasMagic = false, autoCollect = false, safeFromDeath = true) {
    def accepts(item: Item): Boolean = !item.weightless && !item.isQuestItem
  }

  val values: IndexedSeq[ArtifactKind] = findValues

  /** Кто ловит эту вещь прямо с добычи. Ларец и сумка не пересекаются, шкаф в
    * счёт не идёт — он наполняется только руками. */
  def forItem(item: Item): Option[ArtifactKind] = values.find(k => k.autoCollect && k.accepts(item))
}

/** Один артефакт героя: ступень сборки, заряды и то, что внутри. */
final case class Artifact(kind: ArtifactKind, tier: Int, charges: Int, items: Items) {
  def owned: Boolean = tier > 0

  /** Мест внутри: [[ArtifactKind.slotsPerTier]] за каждую ступень. */
  def capacity: Int = kind.capacityAt(tier)

  def occupied: Int = items.data.size

  def freeSlots: Int = (capacity - occupied).max(0)

  def hasRoom: Boolean = owned && freeSlots > 0

  def canUpgrade: Boolean = tier < HeroArtifacts.MaxTier

  def withItems(data: List[Item]): Artifact = copy(items = Items(data))

  def add(item: Item): Artifact = copy(items = Items(items.data :+ item))
}

object Artifact {
  def empty(kind: ArtifactKind): Artifact = Artifact(kind, 0, 0, Items(Nil))
}

/** Артефакты героя одной записью (таблица `hero_artifacts`). */
final case class HeroArtifacts(heroId: HeroId, casket: Artifact, bag: Artifact, wardrobe: Artifact) {

  def of(kind: ArtifactKind): Artifact = kind match {
    case ArtifactKind.Casket    => casket
    case ArtifactKind.LivingBag => bag
    case ArtifactKind.Wardrobe  => wardrobe
  }

  def updated(kind: ArtifactKind, artifact: Artifact): HeroArtifacts = kind match {
    case ArtifactKind.Casket    => copy(casket = artifact)
    case ArtifactKind.LivingBag => copy(bag = artifact)
    case ArtifactKind.Wardrobe  => copy(wardrobe = artifact)
  }

  /** Какой артефакт готов принять эту вещь прямо с добычи. */
  def keeperFor(item: Item): Option[ArtifactKind] =
    ArtifactKind.forItem(item).filter(k => of(k).hasRoom)
}

object HeroArtifacts {
  /** Покупка и три улучшения. */
  val MaxTier: Int = 4

  /** Цена покупки и каждого улучшения — в дублонах. */
  val StepPriceDoubloons: Long = 100L

  /** Максимум зарядов и цена полной зарядки у Жреца (у кого есть магия). */
  val MaxCharges: Int = 25
  val RechargeSilver: Long = 5000L

  def empty(heroId: HeroId): HeroArtifacts =
    HeroArtifacts(heroId,
      Artifact.empty(ArtifactKind.Casket),
      Artifact.empty(ArtifactKind.LivingBag),
      Artifact.empty(ArtifactKind.Wardrobe))
}
