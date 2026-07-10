package pangea.model.item

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Item(
  id: Long,
  name: String,
  lvl: Long,
  rarity: Rarity,
  itemType: ItemType,
  attack: Long,
  accuracy: Long,
  energy: Long,
  armor: Long,
  defence: Long,
  evasion: Long,
  hp: Long = 0, // обязательная прибавка к максимуму HP (доспех)
  details: ItemDetails = ItemDetails.Plain // спец-данные типа (навык/заряды/трофей/карта)
) {
  def withId(id: Long): Item = copy(id = id)

  def withName(name: String): Item = copy(name = name)

  def withLvl(lvl: Long): Item = copy(lvl = lvl)

  def withRarity(rarity: Rarity): Item = copy(rarity = rarity)

  def withAttack(attack: Long): Item = copy(attack = attack)

  def withAccuracy(accuracy: Long): Item = copy(accuracy = accuracy)

  def withEnergy(energy: Long): Item =
    copy(energy = energy)

  def withArmor(armor: Long): Item = copy(armor = armor)

  def withDefence(defence: Long): Item = copy(defence = defence)

  def withEvasion(evasion: Long): Item = copy(evasion = evasion)

  def withHp(hp: Long): Item = copy(hp = hp)

  /** Активный навык предмета (оружие/нагрудник), если есть. */
  def activeSkill: Option[pangea.model.skill.Skill] = details match {
    case ItemDetails.Weapon(s) => Some(s)
    case ItemDetails.Armor(s)  => Some(s)
    case _                     => None
  }

  /** Карта клада или её половинка. */
  def isTreasureMap: Boolean =
    itemType == ItemType.TreasureMap || itemType == ItemType.TreasureMapHalf

  /** Заголовок для списков и экранов. У карт клада уровня нет (зона сама задаёт
   *  диапазон) — показываем только имя; у прочих предметов — «Имя Ур.N». */
  def displayTitle: String = if (isTreasureMap) name else s"$name Ур.$lvl"

  /** Текст-описание карты (для целой — описание зоны, для половинки — заглушка).
   *  None у любого предмета, не являющегося картой. */
  def mapDescription: Option[String] = details match {
    case ItemDetails.TreasureMap(zone) => Some(zone.descriptionFor(itemType))
    case _                             => None
  }

  /** Строки характеристик для отображения (инвентарь/снаряжение/дроп). Спец-строки
   *  (активный навык, зелье пояса) — ниже числовых статов; пустая строка
   *  возвращается, если у предмета нет ни статов, ни спец-данных. */
  def statsLines: List[String] = {
    // Смайлики статов совпадают с конвенцией экрана боя (см. scenes.yaml battle):
    // ⚔ атака, 🎯 точность, ⚡ энергия, 🧥 броня, 🛡 защита, 💨 уклонение, ❤ HP.
    val numeric = List(
      Option.when(attack > 0)(s"⚔ +$attack"),
      Option.when(accuracy > 0)(s"🎯 +$accuracy"),
      Option.when(energy > 0)(s"⚡ +$energy"),
      Option.when(armor > 0)(s"🧥 +$armor"),
      Option.when(defence > 0)(s"🛡 +$defence"),
      Option.when(evasion > 0)(s"💨 +$evasion"),
      Option.when(hp > 0)(s"❤ +$hp")
    ).flatten
    val extra = details match {
      case ItemDetails.Weapon(skill)     => List(s"""Активный навык: «${skill.label}»""")
      case ItemDetails.Armor(skill)      => List(s"""Активный навык: «${skill.label}»""")
      case ItemDetails.Belt(potion, _, m) => List(s"${potion.label} (вместимость $m)")
      case _                             => Nil
    }
    numeric ++ extra
  }

  /** Строка «надетого/сравниваемого» предмета — единый формат для всех экранов,
   *  где рядом с предметом показываем, что уже надето (дроп, надевание):
   *  «<prefix>: Имя Ур.N» и текстовые характеристики ниже (как в [[statsLines]]). */
  def equippedComparison(prefix: String): String = {
    val body = if (statsLines.isEmpty) "" else "\n" + statsLines.mkString("\n")
    s"$prefix: $name Ур.$lvl$body"
  }
}

object Item {
  /** Разделитель между сравниваемым предметом и тем, что уже надето в том же
    * слоте. Единый для всех экранов сравнения (дроп, находка, инвентарь). */
  val ComparisonSeparator: String = "➖➖➖➖➖"

  def NoItem: Item =
    Item(0, "Пусто", 0, Rarity.Gray, ItemType.NoItem, 0, 0, 0, 0, 0, 0, 0, ItemDetails.Plain)

  implicit val encoder: Encoder[Item] = deriveEncoder[Item]
  implicit val decoder: Decoder[Item] = deriveDecoder[Item]

  implicit val meta: Meta[Item] = new Meta(pgDecoderGet, pgEncoderPut)
}
