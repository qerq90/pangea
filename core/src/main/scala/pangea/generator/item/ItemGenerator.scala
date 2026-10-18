package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.stats.Stat
import pangea.model.item.{Gem, Grip, GripRates, Item, ItemDetails, ItemType, PassiveKind, Rarity, ShieldRates}
import pangea.model.skill.Skill

import scala.annotation.tailrec

object ItemGenerator {
  private val id = -1

  private def getModifiedLvl(lvl: Long, rng: Rng): (Long, Rng) = {
    val (delta, next) = rng.between(0L, 7L)
    val scaled        = (lvl - delta + 1).max(1L).min(150L)
    (scaled, next)
  }

  private def modifyParameter(param: Double, rng: Rng): (Long, Rng) =
    modifySpread(param, rng, 10L)

  // param ± spread% (равномерный разброс)
  private def modifySpread(
      param: Double,
      rng: Rng,
      spread: Long
  ): (Long, Rng) = {
    val (pct, next) = rng.between(-spread, spread + 1L)
    ((param + param / 100.0 * pct).toLong, next)
  }

  // Обязательные прибавки: оружию (Weapon) — к атаке lvl×(6+R3) ±20%;
  // нагруднику (ChestPlate) — к HP персонажа lvl×(12+R3) ±10%; щиту — к защите
  // ровно 3×lvl (см. ShieldRates). R3 = rarity.factorR3.
  private def applyMandatory(item: Item, rng: Rng): (Item, Rng) = {
    val r3 = item.rarity.factorR3
    item.itemType match {
      case ItemType.Weapon =>
        val (bonus, next) = modifySpread(item.lvl * (6.0 + r3), rng, 20L)
        (item.withAttack(item.attack + bonus), next)
      case ItemType.ChestPlate =>
        val (bonus, next) = modifySpread(item.lvl * (12.0 + r3), rng, 10L)
        (item.withHp(item.hp + bonus), next)
      case ItemType.Shield =>
        (item.withDefence(item.defence + ShieldRates.DefencePerLvl * item.lvl), rng)
      case _ => (item, rng)
    }
  }

  @tailrec
  private def updateExtraParams(n: Long, item: Item, rng: Rng): (Item, Rng) =
    if (n <= 0) (item, rng)
    else {
      // Атака не ложится ни на что, кроме оружия: шлем с прибавкой к урону —
      // это прошлое. Прочим слотам достаётся всё остальное.
      val pool = if (item.itemType == ItemType.Weapon) Stat.values.toList
                 else Stat.values.toList.filterNot(_ == Stat.Attack)
      val (stat, rng1) = rng.pick(pool)
      val (modified, rng2) = stat match {
        case Stat.Attack =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 0.4 * item.lvl, rng1)
          (item.withAttack(item.attack + v), r)
        case Stat.Accuracy =>
          // Половина прежней ставки: в попадании точность весит вдвое больше
          // (см. BattleState.dodgeChance).
          val (v, r) = modifyParameter(item.rarity.factorR3 * 0.5 * item.lvl, rng1)
          (item.withAccuracy(item.accuracy + v), r)
        case Stat.Energy =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 0.3 * item.lvl, rng1)
          (item.withEnergy(item.energy + v), r)
        case Stat.Armor =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 1.5 * item.lvl, rng1)
          (item.withArmor(item.armor + v), r)
        case Stat.Defence =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 0.5 * item.lvl, rng1)
          (item.withDefence(item.defence + v), r)
        case Stat.Evasion =>
          val (v, r) = modifyParameter(item.rarity.factorR3 * item.lvl, rng1)
          (item.withEvasion(item.evasion + v), r)
      }
      updateExtraParams(n - 1, modified, rng2)
    }

  /** Редкость находки для события «найденный предмет». Одна таблица на все
    * этажи: 60% серый, 30% белый, 10% зелёный. Находка на полу — не награда за
    * глубину, глубина отыгрывается уровнем самого предмета (см. getModifiedLvl)
    * и дропом с мобов; синих и выше здесь не бывает. */
  def foundItemRarity(rng: Rng): (Rarity, Rng) = {
    val (roll, next) = rng.between(0L, 100L)
    val rarity =
      if (roll < FoundGrayPct) Rarity.Gray
      else if (roll < FoundGrayPct + FoundWhitePct) Rarity.White
      else Rarity.Green
    (rarity, next)
  }

  val FoundGrayPct: Long  = 60L
  val FoundWhitePct: Long = 30L

  def createItem(lvl: Long, rarity: Rarity, rng: Rng): (Item, Rng) = {
    val (itemLvl, rng1) = getModifiedLvl(lvl, rng)
    buildAtLevel(itemLvl, rarity, rng1)
  }

  /** Создаёт предмет ровно на заданный уровень, без разброса (для магазина). */
  def createItemAtLevel(lvl: Long, rarity: Rarity, rng: Rng): (Item, Rng) =
    buildAtLevel(lvl.max(1L).min(150L), rarity, rng)

  /** Создаёт предмет заданного СЛОТА на уровень (для пересборки в кубе Азата):
   *  характеристики роллятся заново, профиль (атака/защита) — случайный, поэтому
   *  результат может отличаться от исходного набора статов. Хват оружия тоже
   *  выпадает заново, если его не задали (`grip`): пересборка с прежним именем
   *  держит прежний хват, иначе «Двуручный» в имени соврал бы. */
  def createItemOfType(itemType: ItemType, lvl: Long, rarity: Rarity, rng: Rng,
                       grip: Option[Grip] = None): (Item, Rng) =
    buildAtLevel(lvl.max(1L).min(150L), rarity, rng, Some(itemType), grip)

  private def buildAtLevel(
      itemLvl: Long,
      rarity: Rarity,
      rng: Rng,
      forcedType: Option[ItemType] = None,
      forcedGrip: Option[Grip] = None
  ): (Item, Rng) = {
    val (numberOfExtraParams, rng2) = rarity.getNumOfExtraParams(rng)
    val (isAttack, rng3)            = rng2.nextBoolean

    // Тип: случайный из профиля (обычная генерация) либо принудительный (пересборка).
    val (itemType, rng3z) = forcedType match {
      case Some(t) => (t, rng3)
      case None    => if (isAttack) rng3.pick(ItemType.attackItems) else rng3.pick(ItemType.defenceItems)
    }

    val (item, rng4) =
      if (isAttack) {
        val (armor, rng3b)    = modifyParameter(rarity.factorR * itemLvl, rng3z)
        val (defence, rng3c) = modifyParameter(rarity.factorR1 * itemLvl, rng3b)
        (
          Item(
            id,
            "?",
            itemLvl,
            rarity,
            itemType,
            attack = 0,
            accuracy = 0,
            energy = 0,
            armor = armor,
            defence = defence,
            evasion = 0
          ),
          rng3c
        )
      } else {
        // Лёгкий профиль: меткость и уклонение. Атаки здесь нет — её даёт
        // только оружие (см. applyMandatory и Stat.Attack в updateExtraParams).
        // Меткости — половина ставки: в попадании она весит вдвое больше
        // (см. BattleState.dodgeChance).
        val (accuracy, rng3b) = modifyParameter(rarity.factorR * 0.5 * itemLvl, rng3z)
        val (evasion, rng3c)  = modifyParameter(rarity.factorR1 * itemLvl, rng3b)
        (
          Item(
            id,
            "?",
            itemLvl,
            rarity,
            itemType,
            attack = 0,
            accuracy = accuracy,
            energy = 0,
            armor = 0,
            defence = 0,
            evasion = evasion
          ),
          rng3c
        )
      }

    val (withExtras, rng5) = updateExtraParams(numberOfExtraParams, item, rng4)
    val (withMandatory, rng5b) = applyMandatory(withExtras, rng5)
    val (withGrip, rng5g)      = applyGrip(withMandatory, forcedGrip, rng5b)
    val (withDetails, rng5c)   = applyTypeDetails(withGrip, rng5g)
    val (withSockets, rng5d)   = applySockets(withDetails, rng5c)
    val (name, rng6) =
      ItemNameGenerator.generate(withSockets.itemType, rarity, rng5d, withSockets.grip)
    (withSockets.withName(name), rng6)
  }

  /** Двуручное — поровну меч, топор, копьё, лук или арбалет. У трёх первых хват
    * один, двуручный, поэтому в пуле он стоит трижды: лук или арбалет выходит
    * каждым пятым оружием. */
  private val TwoHandedPool: List[Grip] =
    List(Grip.TwoHanded, Grip.TwoHanded, Grip.TwoHanded, Grip.Bow, Grip.Crossbow)

  /** Одноручное — поровну меч, топор, кинжал, копьё или парирующий кинжал. У
    * четырёх первых хват один, поэтому в пуле он стоит четырежды: парирующий
    * кинжал выходит каждым десятым оружием. */
  private val OneHandedPool: List[Grip] =
    List(Grip.OneHanded, Grip.OneHanded, Grip.OneHanded, Grip.OneHanded, Grip.Parrying)

  /** Хват оружия: в одну руку или в обе — поровну, если его не задали; в обе —
    * см. [[TwoHandedPool]]. Одноручное несёт 60% каждого своего стата,
    * двуручное (и лук с арбалетом) — все сто (см. [[Grip]]); режется итог,
    * вместе с обязательной прибавкой к атаке. Прочим типам хвата нет. */
  private def applyGrip(item: Item, forced: Option[Grip], rng: Rng): (Item, Rng) =
    if (item.itemType != ItemType.Weapon) (item, rng)
    else {
      val (grip, next) = forced match {
        case Some(g) => (g, rng)
        case None =>
          val (two, r) = rng.nextBoolean
          if (two) r.pick(TwoHandedPool) else r.pick(OneHandedPool)
      }
      def cut(v: Long): Long = v * grip.statPct / 100L
      (item.copy(
        grip     = Some(grip),
        attack   = cut(item.attack),
        accuracy = cut(item.accuracy),
        energy   = cut(item.energy),
        armor    = cut(item.armor),
        defence  = cut(item.defence),
        evasion  = cut(item.evasion),
        hp       = cut(item.hp)), next)
    }

  /** Максимум гнёзд на предмете (в т.ч. на одноручном оружии) — 3; у
   *  двуручного — до четырёх (см. [[GripRates]]). */
  private val MaxSockets: Int = 3

  // Число гнёзд по редкости (пороги в %, from Rng.between [0,100)). У оружия и у
  // остального снаряжения таблицы РАЗНЫЕ: многогнёздность — привилегия оружия,
  // прочие слоты получают максимум одно гнездо. Серый/белый/зелёный — без гнёзд.
  //
  //                          ОРУЖИЕ                |     ОСТАЛЬНОЕ
  //   Синяя:       0 — 20, 1 — 30, 2 — 30, 3 — 20  |  0 — 60, 1 — 40
  //   Фиолетовая:  0 — 10, 1 — 30, 2 — 30, 3 — 30  |  0 — 10, 1 — 90
  //   Пурпурная и легендарная:      2 — 50, 3 — 50  |          1 — 100
  //
  // Двуручному оружию — на гнездо больше, чем по таблице, но не больше четырёх.
  // Серое, белое и зелёное гнёзд не носит вовсе, в какую руку его ни бери.
  private def socketCount(rarity: Rarity, isWeapon: Boolean, twoHanded: Boolean, rng: Rng): (Int, Rng) = {
    val (roll, next) = rng.between(0L, 100L)
    val raw =
      if (isWeapon) rarity match {
        case Rarity.Blue =>
          if (roll < 20) 0 else if (roll < 50) 1 else if (roll < 80) 2 else 3
        case Rarity.Purple => // фиолетовая
          if (roll < 10) 0 else if (roll < 40) 1 else if (roll < 70) 2 else 3
        case Rarity.Violet | Rarity.Orange => // пурпурная и легендарная
          if (roll < 50) 2 else 3
        case _ => 0
      }
      else rarity match {
        case Rarity.Blue                   => if (roll < 60) 0 else 1
        case Rarity.Purple                 => if (roll < 10) 0 else 1
        case Rarity.Violet | Rarity.Orange => 1
        case _                             => 0
      }
    val socketed = rarity match {
      case Rarity.Gray | Rarity.White | Rarity.Green => false
      case _                                         => true
    }
    if (isWeapon && twoHanded && socketed)
      ((raw + GripRates.TwoHandedExtraSockets).min(GripRates.TwoHandedMaxSockets), next)
    else (raw.min(MaxSockets), next)
  }

  // Раскатывает пустые гнёзда (None) на предмет по его редкости/типу. Щит и
  // парирующий кинжал — по таблице оружия, но не больше двух гнёзд.
  private def applySockets(item: Item, rng: Rng): (Item, Rng) = {
    val shield        = item.itemType == ItemType.Shield
    val (count, next) = socketCount(item.rarity, item.itemType == ItemType.Weapon || shield, item.twoHanded, rng)
    val capped        =
      if (shield) count.min(ShieldRates.MaxSockets)
      else if (item.grip.contains(Grip.Parrying)) count.min(GripRates.ParryingMaxSockets)
      else count
    (item.copy(sockets = List.fill(capped)(Option.empty[Gem])), next)
  }

  // Шанс (в %) получить пассивку по редкости предмета: серый/белый — 10, зелёный —
  // 40, синий — 80, фиол./пурпур/оранж — гарантированно.
  private def passiveChancePct(rarity: Rarity): Long = rarity match {
    case Rarity.Gray | Rarity.White => 10L
    case Rarity.Green               => 40L
    case Rarity.Blue                => 80L
    case _                          => 100L
  }

  // Носимым слотам, не несущим активного навыка/зелья (шлем/плечи/брасы/перчи/
  // сапоги/штаны/кольца/амулет), с шансом по редкости даём ровно одну пассивку из
  // их пула. Слоты без пула (оружие/нагрудник/пояс/фляга и т.п.) возвращаются как есть.
  private def applyPassive(item: Item, rng: Rng): (Item, Rng) = {
    val pool = PassiveKind.poolFor(item.itemType)
    if (pool.isEmpty) (item, rng)
    else {
      val (roll, rng1) = rng.between(0L, 100L)
      if (roll >= passiveChancePct(item.rarity)) (item, rng1)
      else {
        val (kind, rng2) = rng1.pick(pool)
        (item.copy(details = ItemDetails.Passive(kind)), rng2)
      }
    }
  }

  // Спец-данные типа при генерации:
  //  - оружию — активный навык (один из weaponSkills, у лука и арбалета — из
  //    rangedSkills) → ItemDetails.Weapon;
  //  - нагруднику — активный навык (один из armorSkills) → ItemDetails.Armor;
  //  - поясу — пустые ячейки под расходники, числом по редкости
  //    (ItemDetails.Belt): фляги, пыль, отвары и цветки игрок укладывает сам;
  //  - прочим носимым слотам — пассивка по шансу редкости (см. applyPassive).
  private def applyTypeDetails(item: Item, rng: Rng): (Item, Rng) =
    item.itemType match {
      // Парирующему кинжалу — его умение, Контрудар: не случайное и не руна.
      case ItemType.Weapon if item.grip.contains(Grip.Parrying) =>
        (item.copy(details = ItemDetails.HandSkill(Skill.Counterstrike)), rng)
      case ItemType.Weapon =>
        val (skill, next) = rng.pick(if (item.ranged) Skill.rangedSkills else Skill.weaponSkills)
        (item.copy(details = ItemDetails.Weapon(skill)), next)
      case ItemType.ChestPlate =>
        val (skill, next) = rng.pick(Skill.armorSkills)
        (item.copy(details = ItemDetails.Armor(skill)), next)
      // Щиту — его умение, Блок: не случайное и не руна (см. Skill.handSkills).
      case ItemType.Shield =>
        (item.copy(details = ItemDetails.HandSkill(Skill.Block)), rng)
      case ItemType.Belt =>
        // Пояс выходит пустым: что в него положить, решает игрок.
        (item.copy(details = ItemDetails.Belt()), rng)
      // Фляга в обычной генерации не участвует (см. FlaskGenerator), но по типу
      // её могут собрать заново — вид тогда случайный, заряды по редкости.
      case ItemType.Flask => FlaskGenerator.reroll(item, rng)
      case _ => applyPassive(item, rng)
    }
}
