package pangea.model.monster

import enumeratum._
import pangea.model.battle.Element
import pangea.model.item.{ItemSet, MaterialKind}
import pangea.model.stats.FightStats

/** Минибосс — особый моб, который встречается редко и живёт по своим правилам:
 *  сила зависит от уровня ГЕРОЯ, а не от глубины лабиринта, статы фиксированы (а
 *  не роллятся), способности идут строго по кругу и оплачиваются энергией, а
 *  добыча своя.
 *
 *  Статы считаются от `BossLvL` (см. [[MiniBoss.bossLvl]]) — базовые числа живут
 *  ЗДЕСЬ, на варианте, как у [[pangea.model.item.PassiveKind]]. Уязвимости к
 *  стихиям оружия задаёт сам вариант через [[damageTakenMult]]. */
sealed abstract class MiniBoss(
  val label:    String,
  val genitive: String,
  val race:     Race
) extends EnumEntry {

  /** Боевые статы элементаля на данном уровне босса. */
  def stats(bossLvl: Long): FightStats

  /** Сколько энергии элементаль восстанавливает за раунд. */
  def energyRegen(bossLvl: Long): Long

  /** Опыт за победу. */
  def expReward(bossLvl: Long): Long

  /** Множитель урона, который элементаль получает от оружия стихии `e`. Своя
   *  стихия ему почти не вредит, противоположная — наоборот. 1.0 — обычный урон. */
  def damageTakenMult(e: Element): Double

  /** Можно ли навесить на элементаля этот эффект стихии. Огненный не горит. */
  def immuneToBurn: Boolean

  /** Сколько шагов в круге его способностей (последний — пропуск хода). */
  def abilities: Int

  /** Множитель урона от оружия БЕЗ стихий вообще. Каменного голая сталь почти не
   *  берёт; огненному всё равно. */
  def plainDamageTakenMult: Double

  /** Сколько раз он поднимается после обнуления HP и на сколько % своего
   *  максимума лечится каждый раз. Пустой список — умирает с первого раза. */
  def revives: List[Long] = Nil

  /** Насколько (в п.п.) горение срезает ЕГО точность. 0 — пламя точности не мешает. */
  def burnAccuracyCutPct: Long = 0L

  /** Шанс (в %), что его ОБЫЧНАЯ атака подожжёт героя, и на сколько % при этом
   *  разгорается пламя. 0 — не поджигает: огонь есть только у огненного, камень
   *  и гниль бьют без него. */
  def heroIgniteChancePct: Long = 0L
  def heroIgniteBurnPct: Int    = 0

  /** Как его обычная атака делится по герою: доли (по броне, по HP) от урона.
   *  None — обычный порядок «сперва броня, остаток в HP». Каменный бьёт иначе:
   *  90% урона уходит в броню и одновременно 30% — в HP. Что броня не покрыла,
   *  добирает HP: без брони удар целиком приходится на здоровье. */
  def heroHitSplit: Option[(Double, Double)]

  /** Ингредиент, который остаётся после него. */
  def ingredient: MaterialKind

  /** Набор, вещи которого он роняет и в который переводит куб через ингредиент. */
  def set: ItemSet

  /** Имя в бою и в логе: «Огненный Элементаль», «Гнилой Джо». Редкость в него не
   *  входит — минибосс не «легендарный моб», он именной. */
  def monsterName: String
}

object MiniBoss extends Enum[MiniBoss] {

  val values: IndexedSeq[MiniBoss] = findValues

  /** Уровень босса: `(уровень героя − 1) / 5`, округление вниз, минимум 1. */
  def bossLvl(heroLvl: Long): Long = ((heroLvl - 1L) / 5L).max(1L)

  // ── Огненный элементаль ────────────────────────────────────────────────────────────────
  case object FireElemental extends MiniBoss("Огненный", "Огненного", Race.Elemental) {

    val HpPerLvl: Long       = 1500L
    val ArmorPerLvl: Long    = 750L
    val AtkPerLvl: Long      = 250L
    val EnergyPerLvl: Long   = 150L
    val AccuracyPerLvl: Long = 250L
    val EvasionPerLvl: Long  = 100L
    val EnergyRegenPerLvl: Long = 7L
    val ExpPerLvl: Long      = 200L

    /** Доля урона, которую элементаль получает от огненного оружия — своя стихия
     *  ему почти не вредит. */
    val FireDamageTakenPct: Long = 20L

    /** Насколько сильнее бьёт по нему холод — противоположная стихия. */
    val ColdDamageTakenPct: Long = 150L

    /** Шипы: доля урона обычной атаки, возвращаемая атакующему, пока у элементаля
     *  цела броня. Вместе с шипами он поджигает героя. */
    val ThornsPct: Long   = 1L
    val ThornsBurnPct: Int = 1

    /** Шанс, что обычная атака элементаля подожжёт героя. */
    val IgniteChancePct: Long = 50L

    override val heroIgniteChancePct: Long = IgniteChancePct
    override val heroIgniteBurnPct: Int    = ThornsBurnPct

    // ── Способности (применяются по кругу) ──────────────────────────────────
    /** Огненный всплеск: доля атаки в урон и на сколько % поджигает героя. */
    val SplashDamageFactor: Double = 0.4
    val SplashBurnPct: Int         = 1
    val SplashCostPerLvl: Long     = 7L

    /** Сфера огня: сколько сфер собирается до смерча и сколько стоит каждая. */
    val OrbsToBurst: Int       = 3
    val OrbCostPerLvl: Long    = 14L
    /** Смерч добавляет к двойной атаке по столько % от макс. HP и брони героя. */
    val BurstHeroStatPct: Long = 5L
    /** Шанс травмы, если смерч дошёл до HP героя. */
    val BurstTraumaChancePct: Long = 20L

    /** Огненный щит: сколько % максимумов элементаль себе возвращает. */
    val ShieldArmorPct: Long  = 20L
    val ShieldHpPct: Long     = 5L
    val ShieldCostPerLvl: Long = 7L

    def stats(bossLvl: Long): FightStats = FightStats(
      atk      = AtkPerLvl * bossLvl,
      hp       = HpPerLvl * bossLvl,
      armor    = ArmorPerLvl * bossLvl,
      defence  = 0L,
      evasion  = EvasionPerLvl * bossLvl,
      accuracy = AccuracyPerLvl * bossLvl,
      energy   = EnergyPerLvl * bossLvl
    )

    def energyRegen(bossLvl: Long): Long = EnergyRegenPerLvl * bossLvl
    def expReward(bossLvl: Long): Long   = ExpPerLvl * bossLvl

    /** Огонь по огню почти не проходит, холод — наоборот. Остальные стихии бьют
     *  как обычно. Оружия сразу с огнём и холодом не бывает — эти камни гасят
     *  друг друга при вставке (см. `SocketingState`), так что множитель всегда
     *  один. */
    def damageTakenMult(e: Element): Double = e match {
      case Element.Fire => FireDamageTakenPct / 100.0
      case Element.Cold => ColdDamageTakenPct / 100.0
      case _            => 1.0
    }

    def monsterName: String = s"$label ${Race.Elemental}"

    /** Огненного нельзя поджечь — он и так пламя. */
    def immuneToBurn: Boolean = true

    /** Всплеск, сфера, щит и пропуск. */
    def abilities: Int = 4

    /** Голая сталь бьёт огненного как обычно. */
    def plainDamageTakenMult: Double = 1.0

    /** Бьёт как все: сперва броня, остаток в HP. */
    def heroHitSplit: Option[(Double, Double)] = None

    def ingredient: MaterialKind = MaterialKind.EverburningIron
    def set: ItemSet             = ItemSet.WildFlame

    // ── «Скован холодом» ─────────────────────────────────────────────────────
    /** Сколько ходов держится оцепенение от прока Холода и на сколько % оно
     *  срезает точность элементаля. Пока он скован, шипы не отвечают, его атаки
     *  не поджигают, а одна из собранных сфер гаснет. */
    val ChilledTurns: Int        = 5
    val ChilledAccuracyCutPct: Long = 5L
  }

  // ── Каменный элементаль ────────────────────────────────────────────────────────────────
  case object StoneElemental extends MiniBoss("Каменный", "Каменного", Race.Elemental) {

    val HpPerLvl: Long          = 1250L
    val ArmorPerLvl: Long       = 1500L
    val AtkPerLvl: Long         = 350L
    val EnergyPerLvl: Long      = 100L
    val AccuracyPerLvl: Long    = 200L
    val EvasionPerLvl: Long     = 50L
    val EnergyRegenPerLvl: Long = 7L
    val ExpPerLvl: Long         = 200L

    /** Голую сталь камень почти не чувствует, а вот огонь плавит его сильнее. */
    val PlainDamageTakenPct: Long = 20L
    val FireDamageTakenPct: Long  = 150L

    /** Его обычная атака бьёт по броне и HP РАЗДЕЛЬНО: 90% и 30% от урона. */
    val ArmorHitPct: Long = 90L
    val HpHitPct: Long    = 30L

    // ── Способности (применяются по кругу) ──────────────────────────────────
    /** Каменный всплеск: доля атаки в урон. */
    val SplashDamageFactor: Double = 0.5
    val SplashCostPerLvl: Long     = 7L

    /** Каменный валун: сколько валунов копится до залпа и сколько стоит каждый. */
    val BouldersToBurst: Int    = 3
    val BoulderCostPerLvl: Long = 14L
    /** Залп добавляет к двойной атаке по столько % от макс. HP и брони героя. */
    val BurstHeroStatPct: Long = 5L
    /** Шанс травмы, если залп дошёл до HP героя. */
    val BurstTraumaChancePct: Long = 20L
    /** Залп вдобавок срезает герою защиту и уклонение — на сколько % и надолго. */
    val BurstDebuffPct: Long  = 25L
    val BurstDebuffTurns: Int = 3

    /** Восстановление камня: сколько % своих максимумов он себе возвращает. */
    val RestoreArmorPct: Long   = 20L
    val RestoreHpPct: Long      = 5L
    val RestoreCostPerLvl: Long = 7L

    /** Вязкая земля: на сколько % режет точность и уклонение героя и надолго. */
    val GroundCutPct: Long     = 5L
    val GroundTurns: Int       = 3
    val GroundCostPerLvl: Long = 4L

    // ── Горение ─────────────────────────────────────────────────────────────
    /** Подожжённый камень бьёт слабее, теряет один валун и часть потолка брони.
     *  Текущая броня при этом не срезается — она просто больше не восстановится
     *  выше нового потолка. */
    val BurnedDamageCutPct: Long = 20L
    val BurnedTurns: Int         = 3
    val BurnedMaxArmorCut: Long  = 100L

    def stats(bossLvl: Long): FightStats = FightStats(
      atk      = AtkPerLvl * bossLvl,
      hp       = HpPerLvl * bossLvl,
      armor    = ArmorPerLvl * bossLvl,
      defence  = 0L,
      evasion  = EvasionPerLvl * bossLvl,
      accuracy = AccuracyPerLvl * bossLvl,
      energy   = EnergyPerLvl * bossLvl
    )

    def energyRegen(bossLvl: Long): Long = EnergyRegenPerLvl * bossLvl
    def expReward(bossLvl: Long): Long   = ExpPerLvl * bossLvl

    /** Огонь плавит камень, остальные стихии бьют как обычно. */
    def damageTakenMult(e: Element): Double = e match {
      case Element.Fire => FireDamageTakenPct / 100.0
      case _            => 1.0
    }

    def monsterName: String = s"$label ${Race.Elemental}"

    /** Камень горит — на том и держится вся тактика против него. */
    def immuneToBurn: Boolean = false

    /** Всплеск, валун, восстановление, вязкая земля и пропуск. */
    def abilities: Int = 5

    def plainDamageTakenMult: Double = PlainDamageTakenPct / 100.0

    def heroHitSplit: Option[(Double, Double)] = Some((ArmorHitPct / 100.0, HpHitPct / 100.0))

    def ingredient: MaterialKind = MaterialKind.MagicStone
    def set: ItemSet             = ItemSet.StoneGuard
  }

  // ── Гнилой Джо ──────────────────────────────────────────────────────────────
  case object RottenJoe extends MiniBoss("Гнилой", "Гнилого", Race.Undead) {

    val HpPerLvl: Long          = 2200L
    val AtkPerLvl: Long         = 200L
    val EnergyPerLvl: Long      = 100L
    val AccuracyPerLvl: Long    = 350L
    val EvasionPerLvl: Long     = 100L
    val EnergyRegenPerLvl: Long = 5L
    val ExpPerLvl: Long         = 175L

    /** Огонь — единственное, чего гниль по-настоящему боится. */
    val FireDamageTakenPct: Long = 150L

    // ── Способности (применяются по кругу) ──────────────────────────────────
    /** Ядовитый смрад: на сколько % травит героя и сколько стоит. */
    val StenchPoisonPct: Int    = 10
    val StenchCostPerLvl: Long  = 10L

    /** Широкий удар: доля атаки в урон, цена и шанс травмы при уроне по HP. */
    val SweepDamageFactor: Double   = 0.75
    val SweepCostPerLvl: Long       = 10L
    val SweepTraumaChancePct: Long  = 5L

    /** Гнилое восстановление: сколько % своего максимума HP он себе возвращает. */
    val RegrowHpPct: Long      = 10L
    val RegrowCostPerLvl: Long = 10L

    // ── «Отказывается умирать» ──────────────────────────────────────────────
    /** Сколько % HP он возвращает себе на первом, втором и третьем подъёме. */
    val ReviveHpPct: List[Long] = List(75L, 50L, 25L)
    /** Каждый подъём стоит ему сил: −25% к атаке на 4 хода. */
    val ReviveAtkCutPct: Long = 25L
    val ReviveAtkCutTurns: Int = 4
    /** С какого по счёту падения пламя упокаивает его насовсем. */
    val FireEndsFromDeath: Int = 2

    def stats(bossLvl: Long): FightStats = FightStats(
      atk      = AtkPerLvl * bossLvl,
      hp       = HpPerLvl * bossLvl,
      armor    = 0L,
      defence  = 0L,
      evasion  = EvasionPerLvl * bossLvl,
      accuracy = AccuracyPerLvl * bossLvl,
      energy   = EnergyPerLvl * bossLvl
    )

    def energyRegen(bossLvl: Long): Long = EnergyRegenPerLvl * bossLvl
    def expReward(bossLvl: Long): Long   = ExpPerLvl * bossLvl

    def damageTakenMult(e: Element): Double = e match {
      case Element.Fire => FireDamageTakenPct / 100.0
      case _            => 1.0
    }

    /** Гниль отлично горит. */
    def immuneToBurn: Boolean = false

    /** Горящий Джо хуже видит, куда бьёт. */
    override val burnAccuracyCutPct: Long = 20L

    /** Смрад, широкий удар, восстановление и пропуск. */
    def abilities: Int = 4

    def plainDamageTakenMult: Double = 1.0

    def heroHitSplit: Option[(Double, Double)] = None

    /** Трижды поднимается: сперва на три четверти, потом на половину, потом на
     *  четверть своего максимума. */
    override def revives: List[Long] = ReviveHpPct

    /** Имя у него собственное — раса в него не входит. */
    def monsterName: String = "Гнилой Джо"

    def ingredient: MaterialKind = MaterialKind.GhoulSkin
    def set: ItemSet             = ItemSet.Ghoul
  }

  /** Минибосс по имени варианта — для восстановления из сохранённого боя. */
  def byName(name: String): Option[MiniBoss] = values.find(_.entryName == name)

  /** Элементали — те минибоссы, что водятся в «Логове элементаля». Гнилой Джо
   *  туда не ходит: его встречают на раскопках схрона. */
  val elementals: IndexedSeq[MiniBoss] = values.filter(_.race == Race.Elemental)
}
