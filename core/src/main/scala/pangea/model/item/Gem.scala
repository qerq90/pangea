package pangea.model.item

import enumeratum._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Вид камня-усилителя (череп/аметист/…). Как и [[PassiveKind]], все числовые
 *  ставки эффекта живут ЗДЕСЬ, на самом варианте — и текст описания, и боевая
 *  математика ([[pangea.model.hero.HeroGems]]) читают одни и те же константы.
 *
 *  Эффект камня зависит от того, во что он вставлен: в оружии — «оружейная» грань,
 *  в остальном снаряжении — «броневая». Стихийные грани (преобразование урона в
 *  холод/огонь/воздух/молнию и «эффективность магии») ОТЛОЖЕНЫ: системы стихий в
 *  бою пока нет, поэтому такие грани помечены как временно недействующие
 *  (см. [[GemKind.inertWeapon]]/[[inertArmor]]). */
sealed abstract class GemKind(val baseName: String) extends EnumEntry {

  /** Описание оружейной грани для данного грейда (инвентарь/дроп). */
  def weaponEffectText(grade: Int): String

  /** Описание броневой грани для данного грейда. */
  def armorEffectText(grade: Int): String
}

object GemKind extends Enum[GemKind] {

  val values: IndexedSeq[GemKind] = findValues

  // ── Череп ─────────────────────────────────────────────────────────────────
  case object Skull extends GemKind("Череп") {
    val WeaponVampPctPerGrade: Long          = 1L  // вампиризм: % нанесённого по HP урона в лечение
    val WeaponEnergyRegenPctPerGrade: Long   = 1L  // +% к восстановлению энергии в бою
    val ArmorHpRegenPerMillePerGrade: Long   = 5L  // 0.5% макс.HP реген/раунд (в промилле)
    def weaponEffectText(g: Int): String =
      s"В оружии: вампиризм ${WeaponVampPctPerGrade * g}% нанесённого урона в HP и +${WeaponEnergyRegenPctPerGrade * g}% к восстановлению энергии."
    def armorEffectText(g: Int): String =
      s"В снаряжении: +${ArmorHpRegenPerMillePerGrade * g / 10.0}% макс. HP реген каждый раунд."
  }

  // ── Аметист ───────────────────────────────────────────────────────────────
  case object Amethyst extends GemKind("Аметист") {
    val WeaponAccuracyPctPerGrade: Long = 5L
    val ArmorDefencePctPerGrade: Long   = 1L
    def weaponEffectText(g: Int): String = s"В оружии: +${WeaponAccuracyPctPerGrade * g}% к итоговой точности."
    def armorEffectText(g: Int): String  = s"В снаряжении: +${ArmorDefencePctPerGrade * g}% к итоговой защите."
  }

  // ── Сапфир (стихия холода; броневая грань «магия» — отложена) ────────────────
  case object Sapphire extends GemKind("Сапфир") {
    def weaponEffectText(g: Int): String = s"В оружии: стихия Холода, +${2 * g}% к стихийному урону. +10% урона по броне; 30% шанс -10% Защиты цели."
    def armorEffectText(g: Int): String  = s"В снаряжении: +${1 * g}% к эффективности магии. (пока не действует)"
  }

  // ── Изумруд ──────────────────────────────────────────────────────────────
  case object Emerald extends GemKind("Изумруд") {
    val WeaponPoisonTenthPctPerGrade: Long = 15L // 1.5% яда за грейд (в десятых долях %)
    val ArmorEvasionPctPerGrade: Long      = 1L
    def weaponEffectText(g: Int): String =
      s"В оружии: при уроне по HP накладывает ${WeaponPoisonTenthPctPerGrade * g / 10.0}% яда."
    def armorEffectText(g: Int): String = s"В снаряжении: +${ArmorEvasionPctPerGrade * g}% к итоговому уклонению."
  }

  // ── Рубин (стихия огня; броневая грань — макс. HP) ───────────────────────────
  case object Ruby extends GemKind("Рубин") {
    val ArmorMaxHpPctPerGrade: Long   = 1L
    val ArmorFlatHpBase: Long         = 10L
    val ArmorFlatHpPerExtraGrade: Long = 30L
    def flatHp(g: Int): Long = ArmorFlatHpBase + ArmorFlatHpPerExtraGrade * (g - 1)
    def weaponEffectText(g: Int): String = s"В оружии: стихия Огня, +${2 * g}% к стихийному урону. -20% по броне, +10% по HP; 30% шанс поджечь."
    def armorEffectText(g: Int): String  = s"В снаряжении: +${ArmorMaxHpPctPerGrade * g}% к макс. HP и +${flatHp(g)} к макс. HP."
  }

  // ── Бриллиант (стихия воздуха; броневая грань — энергия) ─────────────────────
  case object Diamond extends GemKind("Бриллиант") {
    val ArmorEnergyPctPerGrade: Long = 1L
    def weaponEffectText(g: Int): String = s"В оружии: стихия Воздуха, +${2 * g}% к стихийному урону. -10% урона, +5% уклонения/точности; 30% шанс усилить их."
    def armorEffectText(g: Int): String  = s"В снаряжении: +${ArmorEnergyPctPerGrade * g}% к Энергии."
  }

  // ── Топаз (стихия молнии; броневая грань — шанс экипировки) ───────────────────
  case object Topaz extends GemKind("Топаз") {
    val ArmorGearChancePctPerGrade: Long = 1L
    def weaponEffectText(g: Int): String = s"В оружии: стихия Молнии, +${2 * g}% к стихийному урону. -20% по броне; 20% урона по броне бьёт и по HP."
    def armorEffectText(g: Int): String  = s"В снаряжении: +${ArmorGearChancePctPerGrade * g}% к шансу получения экипировки."
  }

  implicit val encoder: Encoder[GemKind] = (k: GemKind) => k.entryName.asJson
  implicit val decoder: Decoder[GemKind] = (c: HCursor) => c.as[String].map(GemKind.withName)
}

/** Камень-усилитель конкретного грейда (1..5). Лежит в инвентаре как предмет
 *  ([[ItemType.Gem]] + [[ItemDetails.Gem]]) и вставляется в гнездо снаряжения. */
case class Gem(kind: GemKind, grade: Int) {

  /** Имя с учётом грейда: «Надколотый череп», «Череп», «Идеальный череп». */
  def displayName: String = Gem.gradeName(kind, grade)

  /** Строка эффекта для гнезда в оружии/снаряжении. */
  def weaponEffectText: String = kind.weaponEffectText(grade)
  def armorEffectText: String  = kind.armorEffectText(grade)
}

object Gem {

  val MinGrade: Int = 1
  val MaxGrade: Int = 5

  /** Приставки грейдов (индекс = grade-1). Грейд 3 — базовое имя без приставки. */
  private val gradePrefix: Vector[String] =
    Vector("Надколотый", "Поврежденный", "", "Безупречный", "Идеальный")

  /** Имя камня грейда: приставка + строчное базовое имя, кроме грейда 3 (базовое). */
  def gradeName(kind: GemKind, grade: Int): String = {
    val g = grade.max(MinGrade).min(MaxGrade)
    val prefix = gradePrefix(g - 1)
    if (prefix.isEmpty) kind.baseName else s"$prefix ${kind.baseName.toLowerCase}"
  }

  implicit val encoder: Encoder[Gem] = deriveEncoder[Gem]
  implicit val decoder: Decoder[Gem] = deriveDecoder[Gem]
}
