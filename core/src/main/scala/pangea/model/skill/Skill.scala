package pangea.model.skill

import enumeratum._
import io.circe.{Decoder, Encoder}
import pangea.model.hero.Hero

/** Активный навык — кнопка в бою, заканчивающая ход. Падает на оружие и на
  * нагрудник (см. [[Skill.weaponSkills]] / [[Skill.armorSkills]]) в виде ровно
  * одного навыка на предмет. Все формулы аддитивные по статам героя; ±20%
  * разброс применяется в момент броска снаружи (см. BattleState).
  *
  * `effect` (sealed ADT [[Skill.Effect]]) задаёт, ЧТО делает навык —
  * урон/лечение/ восстановление брони и их спец-варианты; конкретная логика
  * применения матчится в BattleState. `cooldown` — кд ПОСЛЕ применения;
  * `initialCooldown` — кд на старте боя (0 = готов сразу). `energyCost` —
  * стоимость в энергии (формула от уровня героя). `description` — текст для
  * инвентаря с плейсхолдером `{}` под стоимость энергии.
  */
sealed abstract class Skill(
  val label: String,
  val cooldown: Int,
  val initialCooldown: Int,
  val effect: Skill.Effect,
  val hitTemplate: String,
  val description: String
) extends EnumEntry {

  /** Базовое значение эффекта без рандома: урон / лечение / восстановление
    * брони.
    */
  def baseValue(hero: Hero, nowMs: Long): Double

  /** Стоимость применения в энергии (формула от уровня героя, округление вниз).
    */
  def energyCost(hero: Hero): Long

  /** Описание навыка для инвентаря — с подставленной стоимостью энергии и
    * перезарядкой (КД). Оба хвоста собираются здесь, а не в тексте каждого
    * навыка, чтобы формат был единым. */
  def describe(hero: Hero): String =
    description.replace("{}", energyCost(hero).toString) +
      s" Перезарядка: $cooldown ${Skill.turnsWord(cooldown)}."
}

object Skill extends Enum[Skill] {
  val values: IndexedSeq[Skill] = findValues

  /** Что делает навык. Спец-данные эффекта лежат в самом варианте (никаких
    * флагов «по всему навыку»); применение — единый матч в BattleState.
    */
  sealed trait Effect
  object Effect {

    /** Урон по мобу. `reducedByDefence` — если true, урон режется защитой моба.
      */
    final case class Damage(reducedByDefence: Boolean) extends Effect

    /** Лечение HP героя. */
    case object Heal extends Effect

    /** Восстановление брони героя (до максимума). */
    case object RepairArmor extends Effect

    /** Восстановление брони + временный %-баф итоговой защиты на `turns` ходов.
      */
    final case class GuardRepair(defencePct: Long, turns: Int) extends Effect

    /** Урон; герой теряет 10% текущего HP (бонус 8% тек.HP уже учтён в
      * baseValue).
      */
    case object BloodHarvest extends Effect

    /** Урон + наложение яда силой `poisonPct`% на моба. */
    final case class BleedDamage(poisonPct: Int) extends Effect

    /** Урон с шансом (2·Инт − уровень моба)% нанести двойной урон. */
    case object WeakSpotStrike extends Effect
  }

  // Процент текущего HP, который тратит Кровавая жатва (8 из них идут в урон — см. baseValue).
  val BloodHarvestHpCostPct: Long = 10L

  /** Русское склонение слова «ход» после числа: 1 ход, 2–4 хода, 5+ ходов
    * (с учётом 11–14 → «ходов»). */
  private def turnsWord(n: Int): String = {
    val mod100 = math.abs(n) % 100
    val mod10  = math.abs(n) % 10
    if (mod100 >= 11 && mod100 <= 14) "ходов"
    else mod10 match {
      case 1         => "ход"
      case 2 | 3 | 4 => "хода"
      case _         => "ходов"
    }
  }

  // ── Оружейные навыки ────────────────────────────────────────────────────────
  case object SweepingStrike
      extends Skill(
        label = "Размашистый удар",
        cooldown = 2,
        initialCooldown = 0,
        effect = Effect.Damage(reducedByDefence = true),
        hitTemplate =
          "Размашистый удар по дуге достиг врага нанеся ему {} урона!",
        description =
          "Размашистый удар по дуге, от которого сложно увернуться. Среднее влияние на урон от Силы, Интеллекта и Атаки. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      1.0 * b.str + 4.0 * b.int + 0.4 * f.atk
    }
    def energyCost(hero: Hero): Long = 10L + hero.lvl
  }

  case object QuickStrike
      extends Skill(
        label = "Быстрый удар",
        cooldown = 1,
        initialCooldown = 0,
        effect = Effect.Damage(reducedByDefence = true),
        hitTemplate = "Быстрый выпад застал врасплох, нанеся {} урона!",
        description =
          "Быстрый удар, в довесок к основному. Сильное влияние на урон от Интеллекта и Ловкости. Среднее влияние на урон от Точности. Слабое влияние от Атаки. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      4.0 * b.agi + 0.4 * f.accuracy + 4.0 * b.int + 0.1 * f.atk
    }
    def energyCost(hero: Hero): Long = 5L + hero.lvl / 2L
  }

  case object CunningStrike
      extends Skill(
        label = "Хитрый удар",
        cooldown = 2,
        initialCooldown = 0,
        effect = Effect.Damage(reducedByDefence = false),
        hitTemplate =
          "Подловив оппонента вы нашли его слепую зону и наносите ему {} урона!",
        description =
          "Хитрый удар, направленный в сторону куда отходит оппонент. Среднее влияние на урон от Силы, Интеллекта, Точности. Низкое влияние от Атаки. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      2.0 * b.str + 4.0 * b.int + 0.2 * f.atk + 0.5 * f.accuracy
    }
    def energyCost(hero: Hero): Long = 10L + hero.lvl
  }

  case object Ram
      extends Skill(
        label = "Таран",
        cooldown = 3,
        initialCooldown = 0,
        effect = Effect.Damage(reducedByDefence = false),
        hitTemplate =
          "Используя тяжесть своего тела и доспеха, вы врезаетесь во врага и вдавливаете его в стену, нанеся ему {} урона!",
        description =
          "Мягкая подкладка во внутренней стороне и выступающие элементы брони — этот доспех будто был создан с целью пробивать деревянные стены. Сильное влияние на урон от Телосложения. Среднее влияние от Защиты и Интеллекта. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      0.5 * f.defence + 0.75 * b.vit + 3.0 * b.int
    }
    def energyCost(hero: Hero): Long = 15L + hero.lvl
  }

  case object BloodHarvest
      extends Skill(
        label = "Кровавая жатва",
        cooldown = 3,
        initialCooldown = 2,
        effect = Effect.BloodHarvest,
        hitTemplate =
          "Утолив жажду своей кровью, вы наносите страшный удар на {} урона! Вы потеряли {} HP!",
        description =
          "Древняя руна ярко засветилась когда я случайно пролил на неё кровь. Это можно использовать себе на пользу. Сильное влияние на урон от Силы и HP. Среднее влияние от Атаки. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      // 8% текущего HP «вкладывается» в урон (см. BloodHarvestHpCostPct).
      1.5 * b.str + 0.6 * f.atk + 0.08 * hero.fightStats.hp
    }
    def energyCost(hero: Hero): Long = 8L + hero.lvl / 2L
  }

  case object Bleeding
      extends Skill(
        label = "Кровотечение",
        cooldown = 2,
        initialCooldown = 1,
        effect = Effect.BleedDamage(poisonPct = 4),
        hitTemplate =
          "Точный удар в нужную часть тела нанёс {} урона и кровь хлынула из раны потоком (-4%).",
        description =
          "Зазубренное лезвие, долы для кровотока — кажется, оружие создавалось с конкретной целью. Сильное влияние на урон от Ловкости и Интеллекта. Слабое от точности и атаки. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      4.0 * b.agi + 0.2 * f.accuracy + 0.2 * f.atk + 5.0 * b.int
    }
    def energyCost(hero: Hero): Long = 8L + hero.lvl / 2L
  }

  case object WeakSpotStrike
      extends Skill(
        label = "Удар в слабое место",
        cooldown = 2,
        initialCooldown = 1,
        effect = Effect.WeakSpotStrike,
        hitTemplate =
          "Вы замечаете слабое место противника и наносите {} урона точным ударом!",
        description =
          "Когда-то давно это оружие было создано для точных ударов способных оборвать жизнь сильных. Сильное влияние на урон от Ловкости, Точности, Интеллекта. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      2.0 * b.agi + 0.8 * f.accuracy + 4.0 * b.int
    }
    def energyCost(hero: Hero): Long = 10L + hero.lvl
  }

  // ── Бронные навыки ──────────────────────────────────────────────────────────
  case object MinorHeal
      extends Skill(
        label = "Малое исцеление",
        cooldown = 2,
        initialCooldown = 0,
        effect = Effect.Heal,
        hitTemplate = "Применив руну вы исцеляетесь на {} HP!",
        description =
          "Древняя руна всё ещё излучает магию. Если разобраться как активировать, то она станет надёжным союзником. Сильное влияние на исцеление от Интеллекта и Телосложения. Низкое влияние от HP. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      1.0 * b.vit + 2.0 * b.int + 0.2 * hero.effectiveMaxHp(nowMs)
    }
    def energyCost(hero: Hero): Long = 10L + hero.lvl / 2L
  }

  case object Reinforcement
      extends Skill(
        label = "Укрепление",
        cooldown = 2,
        initialCooldown = 1,
        effect = Effect.RepairArmor,
        hitTemplate =
          "Яркий свет руны заставляет дыры на доспехе зарастать прямо на глазах! Восстановлено {} брони.",
        description =
          "У этой руны есть свой пульс… Но по крайней мере из своих последних сил она поддерживает состояние снаряжения. Сильное влияние на восстановление от Защиты и Интеллекта. Слабое от брони. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      0.7 * f.defence + 0.2 * hero.effectiveMaxArmor(nowMs) + 4.0 * b.int
    }
    def energyCost(hero: Hero): Long = 10L + hero.lvl
  }

  case object Restoration
      extends Skill(
        label = "Восстановление",
        cooldown = 4,
        initialCooldown = 1,
        effect = Effect.Heal,
        hitTemplate =
          "Яркий свет от руны исцеляет вас на {} HP! Недавние раны исчезли не оставив и шрама.",
        description =
          "Яркая руна способная исцелять тяжелейшие ранения, однако после использования она надолго замолкает. Сильное влияние на исцеление от Интеллекта и Телосложения. Среднее от HP. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      3.0 * b.int + 2.0 * b.vit + 0.3 * hero.effectiveMaxHp(nowMs)
    }
    def energyCost(hero: Hero): Long = 15L + 2L * hero.lvl
  }

  case object Bulwark
      extends Skill(
        label = "Заслон",
        cooldown = 3,
        initialCooldown = 0,
        effect = Effect.GuardRepair(defencePct = 5L, turns = 3),
        hitTemplate =
          "Яркий свет руны плавно обволакивает ваше тело. Вы чувствуете себя крепче. (+{} брони, +5% защиты на 3 хода)",
        description =
          "От этой руны исходит едва заметное тепло… Кажется она до последнего защищала своего предыдущего владельца. Сильное влияние на восстановление от Интеллекта. Среднее от Защиты и Слабое от брони. Использование расходует {} энергии."
      ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      0.35 * f.defence + 0.1 * hero.effectiveMaxArmor(nowMs) + 4.0 * b.int
    }
    def energyCost(hero: Hero): Long = 12L + hero.lvl
  }

  // Оружие: 1,2,3,6,8,10. Нагрудник: 4,5,7,9,11.
  val weaponSkills: List[Skill] = List(
    SweepingStrike,
    QuickStrike,
    CunningStrike,
    BloodHarvest,
    Bleeding,
    WeakSpotStrike
  )
  val armorSkills: List[Skill] =
    List(MinorHeal, Reinforcement, Restoration, Bulwark, Ram)

  implicit val encoder: Encoder[Skill] =
    Encoder.encodeString.contramap(_.entryName)
  implicit val decoder: Decoder[Skill] = Decoder.decodeString.emap(s =>
    withNameOption(s).toRight(s"Unknown skill: $s")
  )
}
