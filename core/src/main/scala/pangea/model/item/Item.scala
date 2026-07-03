package pangea.model.item

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.skill.Skill

case class Item(
  id: Long,
  name: String,
  lvl: Long,
  rarity: Rarity,
  itemType: ItemType,
  attack: Long,
  accuracy: Long,
  concentration: Long,
  armor: Long,
  defence: Long,
  evasion: Long,
  hp: Long = 0, // обязательная прибавка к максимуму HP (доспех)
  flaskEffect: Option[FlaskEffect] = None,
  charges:     Option[Int]         = None,
  maxCharges:  Option[Int]         = None,
  race:        Option[String]      = None, // раса моба для трофеев (entryName); None у обычных предметов
  trophyKind:  Option[TrophyKind]  = None, // вид трофея; None у обычных предметов
  activeSkill: Option[Skill]       = None, // активный навык: только на Weapon (один из weaponSkills) и ChestPlate (один из armorSkills)
  mapZone:     Option[MapZone]     = None  // зона карты клада; None у всех предметов, кроме карт/половинок
) {
  def withId(id: Long): Item = copy(id = id)

  def withRace(race: String): Item = copy(race = Some(race))

  def withName(name: String): Item = copy(name = name)

  def withLvl(lvl: Long): Item = copy(lvl = lvl)

  def withRarity(rarity: Rarity): Item = copy(rarity = rarity)

  def withAttack(attack: Long): Item = copy(attack = attack)

  def withAccuracy(accuracy: Long): Item = copy(accuracy = accuracy)

  def withConcentration(concentration: Long): Item =
    copy(concentration = concentration)

  def withArmor(armor: Long): Item = copy(armor = armor)

  def withDefence(defence: Long): Item = copy(defence = defence)

  def withEvasion(evasion: Long): Item = copy(evasion = evasion)

  def withHp(hp: Long): Item = copy(hp = hp)

  /** Карта клада или её половинка. */
  def isTreasureMap: Boolean =
    itemType == ItemType.TreasureMap || itemType == ItemType.TreasureMapHalf

  /** Заголовок для списков и экранов. У карт клада уровня нет (зона сама задаёт
   *  диапазон) — показываем только имя; у прочих предметов — «Имя Ур.N». */
  def displayTitle: String = if (isTreasureMap) name else s"$name Ур.$lvl"

  /** Текст-описание карты (для целой — описание зоны, для половинки — заглушка).
   *  None у любого предмета, не являющегося картой. */
  def mapDescription: Option[String] =
    mapZone.map(_.descriptionFor(itemType))

  /** Строки характеристик для отображения (инвентарь/снаряжение/дроп). Активный
   *  навык — последней строкой ниже всех статов; пустая строка возвращается, если
   *  у предмета нет ни статов, ни навыка. */
  def statsLines: List[String] = {
    val numeric = List(
      Option.when(attack > 0)(s"Атака: +$attack"),
      Option.when(accuracy > 0)(s"Точность: +$accuracy"),
      Option.when(concentration > 0)(s"Концентрация: +$concentration"),
      Option.when(armor > 0)(s"Броня: +$armor"),
      Option.when(defence > 0)(s"Защита: +$defence"),
      Option.when(evasion > 0)(s"Уклонение: +$evasion"),
      Option.when(hp > 0)(s"HP: +$hp")
    ).flatten
    numeric ++ activeSkill.map(s => s"""Активный навык: «${s.label}»""").toList
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
  def NoItem: Item =
    Item(0, "Пусто", 0, Rarity.Gray, ItemType.NoItem, 0, 0, 0, 0, 0, 0, 0, None, None, None, None, None, None, None)

  implicit val encoder: Encoder[Item] = deriveEncoder[Item]
  implicit val decoder: Decoder[Item] = deriveDecoder[Item]

  implicit val meta: Meta[Item] = new Meta(pgDecoderGet, pgEncoderPut)
}
