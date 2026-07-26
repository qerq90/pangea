package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Пассивный навык предмета — постоянно действующий эффект, пока предмет надет.
 *  В отличие от активного [[pangea.model.skill.Skill]] (кнопка в бою), пассивка не
 *  тратит ход и не имеет стоимости: она встраивается в конкретную систему
 *  (шанс встречи, лут, уклонение, снижение урона, реген, вместимость сумки…).
 *
 *  Падает на предметы всех слотов КРОМЕ оружия, нагрудника, пояса и фляги — см.
 *  [[eligibleSlots]] у каждого варианта (генерация: [[pangea.generator.item.ItemGenerator]]).
 *  На один предмет — ровно одна пассивка; в бою/луте дубли не складываются
 *  («работает только одна», см. [[pangea.model.hero.HeroPassives]]).
 *
 *  Все числовые пороги/проценты каждой пассивки живут ЗДЕСЬ, на её варианте, а не
 *  магией в месте применения — точки применения только читают эти константы. */
sealed abstract class PassiveKind(
  val label: String,
  val description: String,
  val eligibleSlots: Set[ItemType]
) extends EnumEntry {

  /** Описание для инвентаря (у пассивок нет стоимости/кулдауна — просто текст). */
  def describe: String = description
}

object PassiveKind extends Enum[PassiveKind] {

  // Группы слотов, на которые падает та или иная пассивка. Объявлены ДО `findValues`:
  // иначе при инициализации case object-ов (внутри findValues) они были бы ещё null.
  private val Helmet: Set[ItemType]  = Set(ItemType.Helmet)
  private val Arms: Set[ItemType]    = Set(ItemType.ShoulderPads, ItemType.Bracelets, ItemType.Gloves)
  private val Legs: Set[ItemType]    = Set(ItemType.Boots, ItemType.Pants, ItemType.Leggings)
  private val Rings: Set[ItemType]   = Set(ItemType.Ring)
  private val Amulets: Set[ItemType] = Set(ItemType.Amulet)

  val values: IndexedSeq[PassiveKind] = findValues

  /** Пассивки, которые могут выпасть на предмет данного типа (пусто — тип пассивок
   *  не носит: оружие/нагрудник/пояс/фляга/трофеи/карты). */
  def poolFor(itemType: ItemType): List[PassiveKind] =
    values.filter(_.eligibleSlots.contains(itemType)).toList

  // ── Шлем ────────────────────────────────────────────────────────────────────
  case object Stealthy extends PassiveKind(
    "Скрытный",
    "Скрытный: на 20% реже встречаются боевые события в лабиринте.",
    Helmet
  ) { val EncounterReductionPct: Long = 20L }

  case object Hunter extends PassiveKind(
    "Охотника",
    "Охотника: на 20% чаще встречаются боевые события в лабиринте.",
    Helmet
  ) { val EncounterBonusPct: Long = 20L }

  // ── Плечи + Брасы + Перчатки ─────────────────────────────────────────────────
  case object Taxidermist extends PassiveKind(
    "Таксидермиста",
    "Таксидермиста: 10% шанс получить дополнительный трофей при победе над мобом.",
    Arms
  ) { val TrophyChancePct: Long = 10L }

  case object Toughness extends PassiveKind(
    "Крепкость",
    "Крепкость: при потере всей брони 25% шанс одноразово за бой восстановить 10% брони.",
    Arms
  ) {
    val TriggerPct: Long = 25L
    val RestorePct: Long = 10L
  }

  case object Spiky extends PassiveKind(
    "Шипастый",
    "Шипастый: возвращает 5% полученного от обычной атаки урона обратно врагу.",
    Arms
  ) { val ThornsPct: Long = 5L }

  case object QuickHands extends PassiveKind(
    "Быстрые руки",
    "Быстрые руки: 25% шанс применить флягу и/или пояс повторно в том же раунде.",
    Arms
  ) { val RepeatChancePct: Long = 25L }

  case object Blending extends PassiveKind(
    "Сливающиеся",
    "Сливающиеся: снижает шанс попадания врага на 10%.",
    Arms
  ) { val EnemyHitPenaltyPct: Long = 10L }

  case object Terrifying extends PassiveKind(
    "Ужасающие",
    "Ужасающие: снижает восстановление энергии противника на 10%.",
    Arms
  ) { val EnemyEnergyRegenPenaltyPct: Long = 10L }

  // ── Сапоги + Штаны ────────────────────────────────────────────────────────────
  case object QuickFeet extends PassiveKind(
    "Быстрые ноги",
    "Быстрые ноги: +5% к шансу уклонения.",
    Legs
  ) { val DodgeBonusPct: Long = 5L }

  case object Impenetrable extends PassiveKind(
    "Непробиваемый",
    "Непробиваемый: 20% шанс снизить полученный от обычной атаки урон на 50%.",
    Legs
  ) {
    val TriggerPct: Long   = 20L
    val ReductionPct: Long = 50L
  }

  case object Stash extends PassiveKind(
    "Тайник",
    "Тайник: +10 к слотам инвентаря.",
    Legs
  ) { val ExtraSlots: Long = 10L }

  case object Reinforced extends PassiveKind(
    "Укреплённые",
    "Укреплённые: +5% к защите.",
    Legs
  ) { val DefenceBonusPct: Long = 5L }

  case object Glittering extends PassiveKind(
    "Сверкающие",
    "Сверкающие: +25% к шансу успешно сбежать.",
    Legs
  ) { val FleeDodgeBonusPct: Long = 25L }

  // ── Кольца ────────────────────────────────────────────────────────────────────
  case object Jeweler extends PassiveKind(
    "Ювелира",
    "Ювелира: +10% шанс получить дополнительное золото после боя.",
    Rings
  ) { val GoldChancePct: Long = 10L }

  case object Marauder extends PassiveKind(
    "Мародёра",
    "Мародёра: раскопанная свежая могила существа даёт добычу, будто повержено Необычное существо этой расы.",
    Rings
  )

  case object Robber extends PassiveKind(
    "Разбойника",
    "Разбойника: +5% к итоговому урону.",
    Rings
  ) { val DamageBonusPct: Long = 5L }

  case object Healer extends PassiveKind(
    "Целителя",
    "Целителя: +10% к эффективности активного лечения в бою.",
    Rings
  ) { val HealBonusPct: Long = 10L }

  // ── Амулеты ───────────────────────────────────────────────────────────────────
  case object Focused extends PassiveKind(
    "Сосредоточенный",
    "Сосредоточенный: +10% к восстановлению энергии в бою.",
    Amulets
  ) { val EnergyRegenBonusPct: Long = 10L }

  case object Precise extends PassiveKind(
    "Точности",
    "Точности: +5% к точности.",
    Amulets
  ) { val AccuracyBonusPct: Long = 5L }

  case object Healing extends PassiveKind(
    "Целебный",
    "Целебный: восстанавливает 4% от максимального здоровья каждый раунд.",
    Amulets
  ) { val HpRegenPct: Long = 4L }

  case object SelfRepairing extends PassiveKind(
    "Самовосстанавливающийся",
    "Самовосстанавливающийся: восстанавливает 4% от максимальной брони каждый ход.",
    Amulets
  ) { val ArmorRegenPct: Long = 4L }

  implicit val encoder: Encoder[PassiveKind] = (k: PassiveKind) => k.entryName.asJson
  implicit val decoder: Decoder[PassiveKind] = (c: HCursor) =>
    c.as[String].map(PassiveKind.withName)
}
