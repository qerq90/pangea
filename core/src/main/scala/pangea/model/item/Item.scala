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
  details: ItemDetails = ItemDetails.Plain, // спец-данные типа (навык/заряды/трофей/карта)
  // Гнёзда под камни-усилители: длина = число гнёзд, элемент = вставленный камень
  // (None — свободное гнездо). Пусто у предметов без гнёзд (редкость ниже синей и
  // всё ненадеваемое). Раскатывается при генерации, см. ItemGenerator.
  sockets: List[Option[Gem]] = Nil
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

  /** Пассивный навык предмета (кольца/амулеты/шлем/плечи и т.п.), если есть. */
  def passive: Option[PassiveKind] = details match {
    case ItemDetails.Passive(k) => Some(k)
    case _                      => None
  }

  /** Камень-усилитель, если этот предмет — камень (для инвентаря/вставки). */
  def gem: Option[Gem] = details match {
    case ItemDetails.Gem(g) => Some(g)
    case _                  => None
  }

  /** Материал-ингредиент, если этот предмет — материал. */
  def material: Option[MaterialKind] = details match {
    case ItemDetails.Material(k) => Some(k)
    case _                       => None
  }

  /** Вставленные в гнёзда камни (без пустых гнёзд). */
  def socketedGems: List[Gem] = sockets.flatten

  /** Число свободных гнёзд. */
  def freeSockets: Int = sockets.count(_.isEmpty)

  /** Есть ли хотя бы одно свободное гнездо. */
  def hasFreeSocket: Boolean = freeSockets > 0

  /** Вставить камень в первое свободное гнездо. Если свободных нет — предмет как есть. */
  def socketGem(g: Gem): Item =
    sockets.indexWhere(_.isEmpty) match {
      case -1  => this
      case idx => copy(sockets = sockets.updated(idx, Some(g)))
    }

  /** Карта клада или её половинка. */
  def isTreasureMap: Boolean =
    itemType == ItemType.TreasureMap || itemType == ItemType.TreasureMapHalf

  /** Заголовок для списков и экранов. У карт клада и камней-усилителей уровня нет —
   *  показываем только имя; у прочих предметов — «Имя Ур.N». */
  def displayTitle: String =
    if (isTreasureMap || itemType == ItemType.Gem || itemType == ItemType.Material) name
    else s"$name Ур.$lvl"

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
      case ItemDetails.Passive(kind)     => List(s"""Пассивный навык: «${kind.label}»""")
      case ItemDetails.Belt(potion, _, m) => List(s"${potion.label} (вместимость $m)")
      case ItemDetails.Gem(g)            => List(g.weaponEffectText, g.armorEffectText)
      case ItemDetails.Material(k)       => List(s"Материал: ${k.displayName}")
      case _                             => Nil
    }
    numeric ++ extra ++ socketLines
  }

  /** Строки о гнёздах: сводка «занято/всего» и по одному камню на строку.
   *  Пусто у предметов без гнёзд. */
  def socketLines: List[String] =
    if (sockets.isEmpty) Nil
    else {
      val filled = sockets.count(_.isDefined)
      val header = s"🔲 Гнёзда: $filled/${sockets.length}"
      header :: socketedGems.map(g => s"💎 ${g.displayName}")
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
