package pangea.model.hero

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.generic.semiauto.deriveEncoder
import pangea.model.item.{Gem, Item, ItemSet, ItemType, ShieldRates}

case class Equipment(
  helmet: Item,
  shoulderPads: Item,
  chestPlate: Item,
  bracelets: Item,
  gloves: Item,
  pants: Item,
  boots: Item,
  amulet: Item,
  firstRing: Item,
  secondRing: Item,
  belt: Item,
  flask: Item,
  // Правая рука: одноручное оружие или щит — или двуручное, и тогда занята и левая.
  weapon: Item,
  additionalWeapon: Item,
  // Левая рука: второе одноручное или щит. При двуручном в правой она пуста (см. leftBusy).
  leftHand: Item = Item.NoItem
) {
  def allArmor: Long =
    helmet.armor + shoulderPads.armor + chestPlate.armor + bracelets.armor +
    gloves.armor + pants.armor + boots.armor + amulet.armor +
    firstRing.armor + secondRing.armor + belt.armor + flask.armor +
    weapon.armor + additionalWeapon.armor + leftHand.armor

  // Суммарная прибавка к HP от экипировки (сейчас её даёт только нагрудник)
  // Суммарная прибавка к максимуму Энергии от экипировки (слагаемое «с экипировки»
  // в Hero.maxEnergy).
  def allEnergy: Long =
    helmet.energy + shoulderPads.energy + chestPlate.energy + bracelets.energy +
    gloves.energy + pants.energy + boots.energy + amulet.energy +
    firstRing.energy + secondRing.energy + belt.energy + flask.energy +
    weapon.energy + additionalWeapon.energy + leftHand.energy

  def allHp: Long =
    helmet.hp + shoulderPads.hp + chestPlate.hp + bracelets.hp +
    gloves.hp + pants.hp + boots.hp + amulet.hp +
    firstRing.hp + secondRing.hp + belt.hp + flask.hp +
    weapon.hp + additionalWeapon.hp + leftHand.hp

  /** Все надетые предметы (включая пустые слоты) — единый список для агрегаций,
   *  которым не важен конкретный слот (пассивки, будущие суммы статов). */
  def allItems: List[Item] = List(
    helmet, shoulderPads, chestPlate, bracelets, gloves, pants, boots, amulet,
    firstRing, secondRing, belt, flask, weapon, leftHand, additionalWeapon
  )

  /** Ставит предмет в его же слот вместо прежнего: вещь та же самая, но
    * изменилась — например из её гнезда выломали камень. Кольца и оружие в
    * руках различаются по id; надеванием это НЕ является — статы пересчитывает
    * вызывающий. */
  def replacing(item: pangea.model.item.Item): Equipment = item.itemType match {
    case pangea.model.item.ItemType.Helmet           => copy(helmet = item)
    case pangea.model.item.ItemType.ShoulderPads     => copy(shoulderPads = item)
    case pangea.model.item.ItemType.ChestPlate       => copy(chestPlate = item)
    case pangea.model.item.ItemType.Bracelets        => copy(bracelets = item)
    case pangea.model.item.ItemType.Gloves           => copy(gloves = item)
    case pangea.model.item.ItemType.Pants            => copy(pants = item)
    case pangea.model.item.ItemType.Leggings         => copy(pants = item)
    case pangea.model.item.ItemType.Boots            => copy(boots = item)
    case pangea.model.item.ItemType.Amulet           => copy(amulet = item)
    case pangea.model.item.ItemType.Ring             =>
      if (firstRing.id == item.id) copy(firstRing = item) else copy(secondRing = item)
    case pangea.model.item.ItemType.Belt             => copy(belt = item)
    case pangea.model.item.ItemType.Flask            => copy(flask = item)
    case pangea.model.item.ItemType.Weapon | pangea.model.item.ItemType.Shield =>
      if (leftHand.itemType != ItemType.NoItem && leftHand.id == item.id) copy(leftHand = item)
      else copy(weapon = item)
    case pangea.model.item.ItemType.AdditionalWeapon => copy(additionalWeapon = item)
    case _                                           => this
  }

  /** Набор пассивок с надетых предметов. Множество само схлопывает дубли —
   *  «работает только одна» (см. [[pangea.model.item.PassiveKind]]). */
  def passiveKinds: Set[pangea.model.item.PassiveKind] =
    allItems.flatMap(_.passive).toSet

  /** Слоты, которые считаются в наборы: всё, кроме фляги и доп. оружия, — и обе
   *  руки. Вещей одного набора так можно надеть тринадцать, но полным набор
   *  считается с двенадцати (см. [[pangea.model.item.ItemSet]]), и выше
   *  двенадцати счёт не растёт (см. [[setCounts]]). */
  def setSlots: List[Item] = List(
    helmet, shoulderPads, chestPlate, bracelets, gloves, pants, boots, amulet,
    firstRing, secondRing, belt, weapon, leftHand
  )

  /** Сколько надето предметов каждого набора (учитываются только сетовые слоты).
   *  Тринадцатая вещь набора лишняя: счёт упирается в полный набор. */
  def setCounts: Map[ItemSet, Int] =
    setSlots.flatMap(_.set).groupBy(identity).map { case (s, xs) =>
      s -> xs.size.min(ItemSet.Thresholds.max)
    }

  // ── Руки ───────────────────────────────────────────────────────────────────

  /** Обе руки: правая и левая. В руке — оружие или щит, в любом сочетании. */
  def hands: List[Item] = List(weapon, leftHand)

  /** Щиты в руках: они не оружие, и камни в них — «броневые». */
  def handShields: List[Item] = hands.filter(_.itemType == ItemType.Shield)

  /** Сколько урона снимает Блок: по лучшему щиту в руках (см.
    * ShieldRates.blockPct). Без щита Блока нет, но число — как у белого. */
  def blockPct: Long =
    handShields.map(s => ShieldRates.blockPct(s.rarity)).maxOption.getOrElse(ShieldRates.BaseBlockPct)

  /** Оружие в руках — пустые руки не в счёт. */
  def handWeapons: List[Item] = hands.filter(_.itemType == ItemType.Weapon)

  /** Обе руки пусты: герой бьёт голыми руками. */
  def unarmed: Boolean = handWeapons.isEmpty

  /** Оружие, о котором говорят реплики: в правой руке, а нет его — в левой. */
  def mainWeapon: Item = handWeapons.headOption.getOrElse(Item.NoItem)

  /** Левая рука занята двуручным из правой. */
  def leftBusy: Boolean = weapon.twoHanded

  /** Обе руки заняты одноручным или щитом: новое встаёт только вместо одного
    * из них, и руку спрашивают у игрока — как слот кольца. */
  def handsFull: Boolean =
    weapon.itemType != ItemType.NoItem && leftHand.itemType != ItemType.NoItem

  /** Камни в гнёздах оружия в руках: с ними ссорится пыль на клинке (см. WeaponDust). */
  def handGems: List[Gem] = handWeapons.flatMap(_.socketedGems)

  /** Камни в гнёздах оружия (в руках и в доп. слоте) — дают «оружейную» грань.
   *  Щит не оружие: его камни — в броневой грани. */
  def weaponGems: List[Gem] =
    handWeapons.flatMap(_.socketedGems) ++ additionalWeapon.socketedGems

  /** Камни в гнёздах остального снаряжения (всё, кроме оружия) — дают «броневую»
   *  грань. Стакаются (в отличие от пассивок), поэтому список, а не множество. */
  def armorGems: List[Gem] =
    (List(helmet, shoulderPads, chestPlate, bracelets, gloves, pants, boots, amulet,
      firstRing, secondRing, belt, flask) ++ handShields).flatMap(_.socketedGems)

  /** Какие предметы из снаряжения занимают слот данного типа. Для Ring, Weapon и
   *  Shield возвращает оба слота (колец два, рук две); для прочих типов — один. Пустые
   *  слоты включены — фильтрацию по [[Item.itemType]] делает вызывающий. */
  def equippedFor(itemType: ItemType): List[Item] = itemType match {
    case ItemType.Helmet           => List(helmet)
    case ItemType.ShoulderPads     => List(shoulderPads)
    case ItemType.ChestPlate       => List(chestPlate)
    case ItemType.Bracelets        => List(bracelets)
    case ItemType.Gloves           => List(gloves)
    case ItemType.Pants            => List(pants)
    case ItemType.Leggings         => List(pants)
    case ItemType.Boots            => List(boots)
    case ItemType.Amulet           => List(amulet)
    case ItemType.Ring             => List(firstRing, secondRing)
    case ItemType.Belt             => List(belt)
    case ItemType.Flask            => List(flask)
    case ItemType.Weapon           => List(weapon, leftHand)
    case ItemType.Shield           => List(weapon, leftHand)
    case ItemType.AdditionalWeapon => List(additionalWeapon)
    case ItemType.Trophy           => Nil
    case ItemType.TreasureMap      => Nil
    case ItemType.TreasureMapHalf  => Nil
    case ItemType.Gem              => Nil
    case ItemType.Material         => Nil
    case ItemType.QuestItem        => Nil
    case ItemType.Brew             => Nil
    case ItemType.Rose             => Nil
    case ItemType.RuneStone        => Nil
    case ItemType.NoItem           => Nil
  }
}

object Equipment {
  implicit val encoder: Encoder[Equipment] = deriveEncoder[Equipment]

  /** Декодер рукописный: в снаряжении, сохранённом до разделения рук, левой руки
    * нет — она пуста, а прежнее оружие остаётся в правой (`weapon`). */
  implicit val decoder: Decoder[Equipment] = (c: HCursor) =>
    for {
      helmet           <- c.get[Item]("helmet")
      shoulderPads     <- c.get[Item]("shoulderPads")
      chestPlate       <- c.get[Item]("chestPlate")
      bracelets        <- c.get[Item]("bracelets")
      gloves           <- c.get[Item]("gloves")
      pants            <- c.get[Item]("pants")
      boots            <- c.get[Item]("boots")
      amulet           <- c.get[Item]("amulet")
      firstRing        <- c.get[Item]("firstRing")
      secondRing       <- c.get[Item]("secondRing")
      belt             <- c.get[Item]("belt")
      flask            <- c.get[Item]("flask")
      weapon           <- c.get[Item]("weapon")
      additionalWeapon <- c.get[Item]("additionalWeapon")
      leftHand         <- c.getOrElse[Item]("leftHand")(Item.NoItem)
    } yield Equipment(helmet, shoulderPads, chestPlate, bracelets, gloves, pants, boots, amulet,
      firstRing, secondRing, belt, flask, weapon, additionalWeapon, leftHand)

  implicit val meta: Meta[Equipment] = new Meta(pgDecoderGet, pgEncoderPut)
}
