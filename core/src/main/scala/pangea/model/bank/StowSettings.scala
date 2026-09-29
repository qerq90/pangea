package pangea.model.bank

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.item.{Item, ItemType}
import pangea.model.rune.RuneStoneSize

/** Что именно уходит в ячейку по кнопке «Положить всё». Устроено как настройка
  * продажи хлама у Ришелье: список переключателей, каждый сам за себя.
  *
  * Три последних — не про вещи в сумке, а про другие хранилища: с ними «всё»
  * выгребается и из ларца, из шкафа и из живой сумки, чтобы добро не лежало
  * по трём карманам сразу. */
sealed abstract class StowGroup(val id: String, val key: String) extends EnumEntry

object StowGroup extends Enum[StowGroup] {
  val values: IndexedSeq[StowGroup] = findValues

  case object Herbs      extends StowGroup("herbs",      "bank.vault.stow.herbs")
  case object Gems       extends StowGroup("gems",       "bank.vault.stow.gems")
  case object Gear       extends StowGroup("gear",       "bank.vault.stow.gear")
  case object Trophies   extends StowGroup("trophies",   "bank.vault.stow.trophies")
  case object Brews      extends StowGroup("brews",      "bank.vault.stow.brews")
  case object Dust       extends StowGroup("dust",       "bank.vault.stow.dust")
  case object SmallRunes extends StowGroup("smallRunes", "bank.vault.stow.smallRunes")
  case object BigRunes   extends StowGroup("bigRunes",   "bank.vault.stow.bigRunes")
  case object Materials  extends StowGroup("materials",  "bank.vault.stow.materials")
  case object Maps       extends StowGroup("maps",       "bank.vault.stow.maps")
  case object Silver     extends StowGroup("silver",     "bank.vault.stow.silver")
  case object Casket     extends StowGroup("casket",     "bank.vault.stow.casket")
  case object Wardrobe   extends StowGroup("wardrobe",   "bank.vault.stow.wardrobe")
  case object LivingBag  extends StowGroup("livingBag",  "bank.vault.stow.livingBag")

  /** Переключатели вещей — в том порядке, в каком они стоят на экране. Серебро
    * и чужие хранилища идут следом отдельными строками. */
  val itemGroups: List[StowGroup] =
    List(Herbs, Gems, Gear, Trophies, Brews, Dust, SmallRunes, BigRunes, Materials, Maps)

  val storageGroups: List[StowGroup] = List(Casket, Wardrobe, LivingBag)

  def withId(id: String): Option[StowGroup] = values.find(_.id == id)
}

/** Настройка «Положить всё». Живёт в `heroes.vault_stow`, поэтому декодер
  * рукописный: новый переключатель не должен стирать уже настроенное. */
final case class StowSettings(
  herbs:      Boolean = true,
  gems:       Boolean = true,
  gear:       Boolean = true,
  trophies:   Boolean = true,
  brews:      Boolean = true,
  dust:       Boolean = true,
  smallRunes: Boolean = true,
  bigRunes:   Boolean = true,
  materials:  Boolean = true,
  maps:       Boolean = true,
  silver:     Boolean = false,
  casket:     Boolean = false,
  wardrobe:   Boolean = false,
  livingBag:  Boolean = false
) {

  def on(g: StowGroup): Boolean = g match {
    case StowGroup.Herbs      => herbs
    case StowGroup.Gems       => gems
    case StowGroup.Gear       => gear
    case StowGroup.Trophies   => trophies
    case StowGroup.Brews      => brews
    case StowGroup.Dust       => dust
    case StowGroup.SmallRunes => smallRunes
    case StowGroup.BigRunes   => bigRunes
    case StowGroup.Materials  => materials
    case StowGroup.Maps       => maps
    case StowGroup.Silver     => silver
    case StowGroup.Casket     => casket
    case StowGroup.Wardrobe   => wardrobe
    case StowGroup.LivingBag  => livingBag
  }

  def toggle(g: StowGroup): StowSettings = g match {
    case StowGroup.Herbs      => copy(herbs = !herbs)
    case StowGroup.Gems       => copy(gems = !gems)
    case StowGroup.Gear       => copy(gear = !gear)
    case StowGroup.Trophies   => copy(trophies = !trophies)
    case StowGroup.Brews      => copy(brews = !brews)
    case StowGroup.Dust       => copy(dust = !dust)
    case StowGroup.SmallRunes => copy(smallRunes = !smallRunes)
    case StowGroup.BigRunes   => copy(bigRunes = !bigRunes)
    case StowGroup.Materials  => copy(materials = !materials)
    case StowGroup.Maps       => copy(maps = !maps)
    case StowGroup.Silver     => copy(silver = !silver)
    case StowGroup.Casket     => copy(casket = !casket)
    case StowGroup.Wardrobe   => copy(wardrobe = !wardrobe)
    case StowGroup.LivingBag  => copy(livingBag = !livingBag)
  }

  /** К какой группе относится вещь. Порядок проверок важен: пыль и руны —
    * тоже материалы и камни на вид, но у них свои переключатели. */
  def groupOf(item: Item): Option[StowGroup] =
    if (item.isQuestItem) None
    else if (item.isDust) Some(StowGroup.Dust)
    else if (item.isSmallRune) Some(StowGroup.SmallRunes)
    else if (item.runeStone.exists(_.size == RuneStoneSize.Big)) Some(StowGroup.BigRunes)
    else if (item.brew.isDefined) Some(StowGroup.Brews)
    else if (item.gem.isDefined) Some(StowGroup.Gems)
    else if (item.material.exists(_.isHerb)) Some(StowGroup.Herbs)
    else if (item.material.isDefined) Some(StowGroup.Materials)
    // Карта и её половинка — под одним переключателем: половинка тоже ждёт
    // своего часа, и хранить их врозь незачем.
    else if (item.isTreasureMap) Some(StowGroup.Maps)
    else if (item.itemType == ItemType.Trophy) Some(StowGroup.Trophies)
    else if (ItemType.equippable.contains(item.itemType)) Some(StowGroup.Gear)
    else None // прочее кладут руками

  /** Идёт ли эта вещь в ячейку при нынешней настройке. */
  def takes(item: Item): Boolean = groupOf(item).exists(on)
}

object StowSettings {
  val default: StowSettings = StowSettings()

  implicit val encoder: Encoder[StowSettings] = (s: StowSettings) =>
    Json.obj(StowGroup.values.toList.map(g => g.id -> s.on(g).asJson): _*)

  implicit val decoder: Decoder[StowSettings] = (c: HCursor) => {
    def flag(g: StowGroup): Boolean = c.get[Boolean](g.id).getOrElse(default.on(g))
    Right(StowSettings(
      herbs      = flag(StowGroup.Herbs),
      gems       = flag(StowGroup.Gems),
      gear       = flag(StowGroup.Gear),
      trophies   = flag(StowGroup.Trophies),
      brews      = flag(StowGroup.Brews),
      dust       = flag(StowGroup.Dust),
      smallRunes = flag(StowGroup.SmallRunes),
      bigRunes   = flag(StowGroup.BigRunes),
      materials  = flag(StowGroup.Materials),
      maps       = flag(StowGroup.Maps),
      silver     = flag(StowGroup.Silver),
      casket     = flag(StowGroup.Casket),
      wardrobe   = flag(StowGroup.Wardrobe),
      livingBag  = flag(StowGroup.LivingBag)))
  }
}
