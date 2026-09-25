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
  sockets: List[Option[Gem]] = Nil,
  // Набор, к которому принадлежит предмет. Option, а не значение по умолчанию:
  // у уже сохранённых предметов этого поля в JSON нет, а производный декодер
  // circe без него не собрал бы объект. Имя набора стоит в названии предмета
  // вместо титула (см. ItemNameGenerator.setName).
  set: Option[ItemSet] = None
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
  /** Сюжетный предмет: вид, если это он. */
  def questItem: Option[QuestItemKind] = details match {
    case ItemDetails.Quest(kind) => Some(kind)
    case _                       => None
  }

  def isQuestItem: Boolean = itemType == ItemType.QuestItem

  /** Горсть пыли: места в сумке и в бочке не занимает (в кубе Азата — как все, слот). */
  def isDust: Boolean = material.exists(_.gem.isDefined)

  /** Вид пыли, если это она. */
  def dustKind: Option[MaterialKind] = material.filter(_.gem.isDefined)

  /** Малая руна: как и пыль, места не занимает и копится до предела. */
  def isSmallRune: Boolean = runeStone.exists(_.size == pangea.model.rune.RuneStoneSize.Small)

  /** Ключ «этого же добра» у вещей, которые не занимают места: по нему считается
    * предел [[Item.HoardLimit]]. None — обычная вещь, она занимает слот. */
  def hoardKey: Option[String] =
    dustKind.map(k => s"dust:${k.entryName}")
      .orElse(Option.when(isSmallRune)(s"rune:${runeStone.get.runeKey}"))

  /** Вещь, которая не занимает места ни в сумке, ни в бочке. */
  def weightless: Boolean = hoardKey.isDefined

  def brew: Option[BrewKind] = details match {
    case ItemDetails.Brew(k) => Some(k)
    case _                   => None
  }

  /** Рунный камень, если это он (см. [[pangea.model.rune.RuneStone]]). */
  def runeStone: Option[ItemDetails.RuneStone] = details match {
    case r: ItemDetails.RuneStone => Some(r)
    case _                        => None
  }

  /** Божественное оружие, если это оно (см. [[DivineKind]]); в доп. слоте может
    * лежать и обычная вещь — тогда пусто. */
  def divine: Option[ItemDetails.Divine] = details match {
    case r: ItemDetails.Divine => Some(r)
    case _                    => None
  }

  /** Раскрывшаяся роза, если это она (см. [[RoseKind]]). */
  def rose: Option[ItemDetails.Rose] = details match {
    case r: ItemDetails.Rose => Some(r)
    case _                   => None
  }

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

  /** Заголовок для списков и экранов — единый формат «<кружок редкости> [Ур.N]
   *  <имя>» (напр. «🔵 [Ур.12] Хороший Шлем Рыцаря»). Кружок редкости вшит в
   *  начало сгенерированного имени (см. [[pangea.generator.item.ItemNameGenerator]]),
   *  поэтому его вырезаем и ставим перед уровнем. У предметов без кружка в имени
   *  (трофеи, тестовые предметы) остаётся «[Ур.N] <имя>».
   *
   *  У карт клада, камней-усилителей и материалов уровня нет — только имя. */
  def displayTitle: String =
    if (isQuestItem) s"${Item.QuestMark} $name" // сюжетный предмет: звёздочка вместо редкости и уровня
    else if (isTreasureMap || itemType == ItemType.Gem || itemType == ItemType.Material ||
        itemType == ItemType.Flask || itemType == ItemType.Brew || itemType == ItemType.RuneStone ||
        rose.isDefined) name
    else {
      val prefix = s"${rarity.emoji} "
      if (name.startsWith(prefix)) s"${rarity.emoji} [Ур.$lvl] ${name.stripPrefix(prefix)}"
      else s"[Ур.$lvl] $name"
    }

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
    val setLine = set.map(s => s"Набор: «${s.label}»").toList
    val extra = details match {
      case ItemDetails.Weapon(skill)     => List(s"""Активный навык: «${skill.label}»""")
      case ItemDetails.Armor(skill)      => List(s"""Активный навык: «${skill.label}»""")
      case ItemDetails.Passive(kind)     => List(s"""Пассивный навык: «${kind.label}»""")
      case ItemDetails.Belt(potion, _, m) => List(s"${potion.label} (вместимость $m)")
      case ItemDetails.Gem(g)            => List(g.weaponEffectText, g.armorEffectText)
      // Фляга: что делает глоток и сколько его осталось. Уровня у неё нет.
      case ItemDetails.Flask(effect, c, m) => List(effect.describe, s"🧪 Заряды: $c/$m")
      // Название материала уже стоит заголовком предмета, повторять его строкой
      // «Материал: …» незачем — она остаётся только у тех, кому нечего сказать о
      // себе, чтобы экран не выглядел пустым.
      case ItemDetails.Material(k)       =>
        if (k.description.isEmpty) List(s"Материал: ${k.displayName}") else List(k.description)
      // Клык Белого волка — трофей с историей; у обычных трофеев описания нет.
      case ItemDetails.Trophy(_, k, _)   => if (k.description.isEmpty) Nil else List(k.description)
      // Отвар: только описание. Рецепты в карточках предметов не пишем — их
      // игрок узнаёт сам (или у нужного человека), это часть игры.
      case ItemDetails.Brew(k)           => List(k.description)
      // Божественное оружие: только описание — ни статов, ни числа ударов, которые она ещё держит.
      case ItemDetails.Divine(k, _, _)    => List(k.description)
      // Роза: описание и сколько раскрытий в ней осталось.
      case ItemDetails.Rose(k, c, m)      => List(k.description, s"🌹 Раскрытий: $c/$m")
      // Рунный камень: узор, ощущение от него и что даст сама руна.
      case ItemDetails.RuneStone(key, size) =>
        pangea.model.rune.Rune.byKey(key).map(pangea.model.rune.RuneStone.describe(_, size)).getOrElse(Nil)
      case _                             => Nil
    }
    numeric ++ setLine ++ extra ++ socketLines
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
   *  «<prefix>: <заголовок>» (см. [[displayTitle]]) и текстовые характеристики
   *  ниже (как в [[statsLines]]). */
  def equippedComparison(prefix: String): String = {
    val body = if (statsLines.isEmpty) "" else "\n" + statsLines.mkString("\n")
    s"$prefix: $displayTitle$body"
  }
}

object Item {
  /** Сколько одинакового невесомого добра (пыль, малые руны) влезает в одно
    * хранилище. Места оно не занимает (см. `Inventory.occupied`), поэтому предел
    * у него свой: без него сумка копила бы его без конца. У сумки и у бочки счёт
    * раздельный. */
  val HoardLimit: Int = 100

  /** Разделитель между сравниваемым предметом и тем, что уже надето в том же
    * слоте. Единый для всех экранов сравнения (дроп, находка, инвентарь). */
  val ComparisonSeparator: String = "➖➖➖➖➖"

  /** Значок сюжетного предмета перед именем — в списке сумки и на карточке. */
  val QuestMark: String = "★"

  def NoItem: Item =
    Item(0, "Пусто", 0, Rarity.Gray, ItemType.NoItem, 0, 0, 0, 0, 0, 0, 0, ItemDetails.Plain)

  implicit val encoder: Encoder[Item] = deriveEncoder[Item]
  implicit val decoder: Decoder[Item] = deriveDecoder[Item]

  implicit val meta: Meta[Item] = new Meta(pgDecoderGet, pgEncoderPut)
}
