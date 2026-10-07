package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Набор («сет») снаряжения. Предмет из набора носит его имя вместо титула —
 *  третьим словом названия: «🔵 Прочный Шлем Каменного стража» (поэтому
 *  [[ItemSet.title]] стоит в родительном падеже, как обычные титулы).
 *
 *  Бонусы выдаются по порогам числа надетых предметов набора: 2/4/6/8/10/12.
 *  Пороги независимы между наборами — можно носить 12 предметов из шести разных
 *  наборов и получить шесть двойных бонусов. Считаются только 12 «сетовых»
 *  слотов (см. `Equipment.setItems`): фляга и доп. оружие в наборы не входят.
 *
 *  Числовые ставки живут ЗДЕСЬ, на варианте — как у [[PassiveKind]] и
 *  [[GemKind]]; точки применения читают готовый модификатор через
 *  `pangea.model.hero.HeroSets`. */
sealed abstract class ItemSet(val label: String, val title: String) extends EnumEntry {

  /** Бонусы набора по порогам, от 2 предметов и выше. */
  def bonuses: List[SetBonus]

  /** Бонус конкретного порога, если он у набора описан. */
  def bonusAt(pieces: Int): Option[SetBonus] = bonuses.find(_.pieces == pieces)
}

/** Один бонус набора: порог, текст для игрока и признак «механики ещё нет».
 *
 *  `active = false` — порог описан, показывается игроку, но в бою пока ничего не
 *  делает (как броневая грань Сапфира). Так помечены бонусы, под которые в игре
 *  нет системы: стихийный урон мобов и негативные эффекты на герое. */
final case class SetBonus(pieces: Int, text: String, active: Boolean = true)

/** Общие для всех наборов числа. Живут ОТДЕЛЬНО от компаньона [[ItemSet]]
 *  намеренно: если бы `case object`-ы читали их из своего же компаньона, его
 *  инициализация пошла бы изнутри их конструкторов — а компаньон в этот момент
 *  сам инициализирует их через `findValues`. Такой цикл на одном потоке кладёт в
 *  `values` null (см. регрессию в PassiveKindSpec), а на нескольких — намертво
 *  заклинивает инициализацию класса. */
object SetRates {

  /** Пороги, на которых набор выдаёт бонус. */
  val Thresholds: List[Int] = List(2, 4, 6, 8, 10, 12)

  /** Прибавка «двойного» порога у каждого набора — в % к своему стату. */
  val StatBonusPct: Long = 5L

  /** Общий для всех наборов бонус восьмого порога. */
  val HpFlatBonus: Long   = 300L
  val HpPctBonus: Long    = 10L
  val HpBonusText: String = s"+$HpFlatBonus к HP и +$HpPctBonus% к HP."
}

object ItemSet extends Enum[ItemSet] {

  val values: IndexedSeq[ItemSet] = findValues

  // Реэкспорт ставок для потребителей — сами варианты читают их из SetRates.
  val Thresholds: List[Int] = SetRates.Thresholds
  val HpFlatBonus: Long     = SetRates.HpFlatBonus
  val HpPctBonus: Long      = SetRates.HpPctBonus
  val StatBonusPct: Long    = SetRates.StatBonusPct

  // ── Каменный страж ──────────────────────────────────────────────────────────
  case object StoneGuard extends ItemSet("Каменный страж", "Каменного стража") {
    val DefencePct: Long          = SetRates.StatBonusPct
    val ElementalTakenCutPct: Long = 20L
    val ArmorDamageCutPct: Long    = 20L
    val IgniteResistPct: Long      = 30L
    val LowHpThresholdPct: Long    = 30L
    val ArmorRestorePct: Long      = 30L

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$DefencePct% к защите."),
      SetBonus(4,  s"Любой стихийный урон на $ElementalTakenCutPct% слабее по вам."),
      SetBonus(6,  s"Урон по вашей броне дополнительно снижен на $ArmorDamageCutPct%."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"Шанс поджечь Вас снижен на $IgniteResistPct%."),
      SetBonus(12, s"При получении урона, после которого у вас остаётся менее $LowHpThresholdPct% HP, " +
                   s"вы восстанавливаете $ArmorRestorePct% брони.")
    )
  }

  // ── Дикое пламя ─────────────────────────────────────────────────────────────
  case object WildFlame extends ItemSet("Дикое пламя", "Дикого пламени") {
    val AttackPct: Long        = SetRates.StatBonusPct
    val FireDamageBonusPct: Long = 10L
    val BurnGrowthMult: Long   = 2L
    val IgniteChanceBonusPct: Long = 30L

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$AttackPct% к атаке."),
      SetBonus(4,  s"Стихийный урон огнём по HP и броне увеличен на $FireDamageBonusPct%."),
      SetBonus(6,  s"Рост урона от огня за раунд увеличен в $BurnGrowthMult раза."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"Шанс поджечь врага увеличен на $IgniteChanceBonusPct%."),
      SetBonus(12, "Ваши активные умения всегда поджигают врага. Каждый процент огня дополнительно " +
                   "снижает защиту противника на столько же процентов.")
    )
  }

  // ── Упырь ───────────────────────────────────────────────────────────────────
  case object Ghoul extends ItemSet("Упырь", "Упыря") {
    val EvasionPct: Long      = SetRates.StatBonusPct
    val LifestealPct: Long    = 2L
    val BleedChancePct: Long  = 30L
    val BleedPct: Int         = 4
    val KillHpRestorePct: Long    = 10L
    val KillArmorRestorePct: Long = 10L

    /** Сколько чужой крови достанется одному вампиру (порог 10).
      *
      * `bleedOnOtherSide` — урон, который кровотечение нанесло за раунд
      * ЧУЖОЙ стороне; `vampires` — сколько вампиров на СВОЕЙ. Кровь
      * делится ровно между ними: двое — каждому половина.
      *
      * Счёт идёт по сторонам, а не по всем кровоточащим сразу: вампиры
      * одной стороны пьют кровь только соперников, и наоборот; кто именно
      * заставил кровоточить, значения не имеет.
      *
      * Сегодня вампир в бою всегда один — сам герой, и правило отдаёт ему
      * всю кровь. Написано оно под будущее, где вампиров на стороне может
      * оказаться несколько. */
    def bleedHealShare(bleedOnOtherSide: Long, vampires: Int): Long =
      if (vampires <= 0 || bleedOnOtherSide <= 0L) 0L else bleedOnOtherSide / vampires.toLong

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$EvasionPct% к уклонению."),
      SetBonus(4,  s"+$LifestealPct% вампиризма."),
      SetBonus(6,  s"$BleedChancePct% шанс, что ваша атака, нанёсшая урон по HP, вызовет кровотечение $BleedPct%."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, "Урон от кровотечения врага также восстанавливает ваше HP."),
      SetBonus(12, "Ваши активные умения, наносящие урон, всегда накладывают кровотечение. " +
                   s"Каждый убитый враг тут же, в бою, восстанавливает вам $KillHpRestorePct% HP и $KillArmorRestorePct% брони.")
    )
  }

  // ── Охотник ─────────────────────────────────────────────────────────────────
  case object Hunter extends ItemSet("Охотник", "Охотника") {
    val AccuracyPct: Long      = SetRates.StatBonusPct
    val EnergyPct: Long        = 10L
    val AgiRegenMult: Long     = 2L
    val RepeatChancePct: Long  = 25L
    val BlockSkillChancePct: Long = 10L
    val SkillCritBonusPct: Long = 10L

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$AccuracyPct% к точности."),
      SetBonus(4,  s"+$EnergyPct% к энергии. Ловкость восстанавливает в $AgiRegenMult раза больше энергии за раунд."),
      SetBonus(6,  s"$RepeatChancePct% шанс повторить атаку при промахе по врагу. Один раз за раунд."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"$BlockSkillChancePct% шанс заблокировать урон от атакующего умения противника."),
      SetBonus(12, s"+$SkillCritBonusPct% к шансу критического применения всех атакующих умений.")
    )
  }

  // ── Крыса ───────────────────────────────────────────────────────────────────
  case object Rat extends ItemSet("Крыса", "Крысы") {
    val ArmorPct: Long        = SetRates.StatBonusPct
    val PoisonChancePct: Long = 30L
    val PoisonPct: Int        = 3
    val SummonChancePct: Long = 5L
    val DotTakenCutPct: Long  = 50L

    /** На сколько п.п. за раунд слабеет кровотечение на герое (порог 10): до
      * набора оно не затухало вовсе — только лечение снимало его целиком. Шаг
      * тот же, что у яда (`Poison.DecayPerRound`), чтобы правило читалось одно на
      * оба эффекта; число здесь своё — вариант enum не ходит за ним в другой
      * пакет (см. комментарий к [[SetRates]] про циклы инициализации). */
    val BleedDecayPerRound: Int = 2

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$ArmorPct% к броне."),
      SetBonus(4,  s"$PoisonChancePct% шанс, что ваша атака, нанёсшая урон по HP, отравит врага на $PoisonPct%."),
      SetBonus(6,  s"$SummonChancePct% шанс за раунд, что из-под ваших ног выскочит крыса и встанет в строй. " +
                   "После боя она остаётся при вас на сутки."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"Яд и кровотечение на вас снимают вдвое меньше HP, а кровотечение вдобавок затухает " +
                   s"на $BleedDecayPerRound% в раунд, как яд."),
      SetBonus(12, "Из-под ваших ног выскакивают Чумные крысы. Если в начале боя при вас нет ни одной крысы, " +
                   "одна приходит сразу.")
    )
  }

  /** Набор по имени-титулу из названия предмета (третье слово). */
  def byTitle(title: String): Option[ItemSet] = values.find(_.title == title)

  implicit val encoder: Encoder[ItemSet] = (s: ItemSet) => s.entryName.asJson
  implicit val decoder: Decoder[ItemSet] = (c: HCursor) => c.as[String].map(ItemSet.withName)
}
