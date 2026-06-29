package pangea.model.trauma

case class TraumaPenalties(
  atkPct:     Double = 0.0,
  strPct:     Double = 0.0,
  evasionPct: Double = 0.0,
  hpPct:      Double = 0.0,
  vitPct:     Double = 0.0,
  accPct:     Double = 0.0,
  concPct:    Double = 0.0,
  armorPct:   Double = 0.0,
  defPct:     Double = 0.0,
  intPct:     Double = 0.0
) {
  def +(o: TraumaPenalties): TraumaPenalties = TraumaPenalties(
    atkPct     = atkPct     + o.atkPct,
    strPct     = strPct     + o.strPct,
    evasionPct = evasionPct + o.evasionPct,
    hpPct      = hpPct      + o.hpPct,
    vitPct     = vitPct     + o.vitPct,
    accPct     = accPct     + o.accPct,
    concPct    = concPct    + o.concPct,
    armorPct   = armorPct   + o.armorPct,
    defPct     = defPct     + o.defPct,
    intPct     = intPct     + o.intPct
  )

  /** Каждый ненулевой штраф как «-X% характеристика», в порядке для показа. */
  def minuses: List[String] = {
    def fmt(pct: Double, label: String): Option[String] =
      Option.when(pct != 0.0)(s"-${math.round(pct * 100)}% $label")
    List(
      fmt(atkPct,     "урон"),
      fmt(strPct,     "сила"),
      fmt(evasionPct, "уклонение"),
      fmt(hpPct,      "HP"),
      fmt(vitPct,     "телосложение"),
      fmt(accPct,     "точность"),
      fmt(concPct,    "сосредоточение"),
      fmt(armorPct,   "броня"),
      fmt(defPct,     "защита"),
      fmt(intPct,     "интеллект")
    ).flatten
  }

  def shortText: String = minuses.mkString(", ")
}

object TraumaPenalties {
  val none: TraumaPenalties = TraumaPenalties()
}

sealed trait Trauma {
  def name: String
  def severity: Trauma.Severity
  def description: String
  def penalties: TraumaPenalties
}

object Trauma {
  sealed trait Severity
  case object Light  extends Severity
  case object Medium extends Severity
  case object Heavy  extends Severity

  // ── Лёгкие ──────────────────────────────────────────────────────────────
  case object BruisedLeg extends Trauma {
    val name        = "Ушиб ноги"
    val severity    = Light
    val description = "Тяжело ушибленная нога сильно ограничивает мобильность. -5% к ловкости, -5% к броне."
    val penalties   = TraumaPenalties(evasionPct = 0.05, armorPct = 0.05)
  }
  case object SmashedFinger extends Trauma {
    val name        = "Раздробленный палец"
    val severity    = Light
    val description = "Неудачный удар раздробил палец, и держать что-то в руках стало болезненно. -5% к урону, -5% к силе."
    val penalties   = TraumaPenalties(atkPct = 0.05, strPct = 0.05)
  }
  case object AchingRib extends Trauma {
    val name        = "Ноющее ребро"
    val severity    = Light
    val description = "Серьёзный удар по телу оставил синяк и больное ребро, напоминающее о себе при каждом телодвижении. -5% к HP, -5% к телосложению."
    val penalties   = TraumaPenalties(hpPct = 0.05, vitPct = 0.05)
  }
  case object TornEar extends Trauma {
    val name        = "Порванное ухо"
    val severity    = Light
    val description = "Вам чуть не отрезали ухо — кровь залила голову и шею, что мешает ориентироваться в пространстве. -5% к точности, -5% к сосредоточению."
    val penalties   = TraumaPenalties(accPct = 0.05, concPct = 0.05)
  }

  // ── Средние ─────────────────────────────────────────────────────────────
  case object CutAchilles extends Trauma {
    val name        = "Порезанное ахиллесово сухожилие"
    val severity    = Medium
    val description = "Травма, которая заставит даже быстрого человека ковылять по полю боя. -25% к ловкости, -15% к броне, -15% к защите."
    val penalties   = TraumaPenalties(evasionPct = 0.25, armorPct = 0.15, defPct = 0.15)
  }
  case object PiercedArm extends Trauma {
    val name        = "Проколотая рука"
    val severity    = Medium
    val description = "Проколотая рука может не остановить решительного бойца драться дальше, но она сделает обращение с оружием более трудным. -25% к урону, -25% к силе."
    val penalties   = TraumaPenalties(atkPct = 0.25, strPct = 0.25)
  }
  case object DeepBellyGash extends Trauma {
    val name        = "Глубокий порез живота"
    val severity    = Medium
    val description = "Глубокий порез на животе повредил мышцу, и больно не то чтобы двигаться, а даже стоять. -25% к HP, -25% к телосложению, -15% к броне, -15% к защите."
    val penalties   = TraumaPenalties(hpPct = 0.25, vitPct = 0.25, armorPct = 0.15, defPct = 0.15)
  }
  case object BrokenNose extends Trauma {
    val name        = "Сломанный нос"
    val severity    = Medium
    val description = "Тупым предметом был сломан нос, из-за чего дышать стало затруднительно. -25% к точности, -25% к сосредоточению."
    val penalties   = TraumaPenalties(accPct = 0.25, concPct = 0.25)
  }

  // ── Тяжёлые ─────────────────────────────────────────────────────────────
  case object KneecapDamage extends Trauma {
    val name        = "Повреждение коленной чашечки"
    val severity    = Heavy
    val description = "Хрупкая коленная чашечка была повреждена. В результате вызывает боль с каждым движением и ограничивает мобильность. -50% к ловкости, -15% к броне, -15% к защите."
    val penalties   = TraumaPenalties(evasionPct = 0.50, armorPct = 0.15, defPct = 0.15)
  }
  case object BrokenArm extends Trauma {
    val name        = "Сломанная рука"
    val severity    = Heavy
    val description = "Сломанная рука делает невозможным эффективное использование любого оружия. -50% к урону, -50% к силе."
    val penalties   = TraumaPenalties(atkPct = 0.50, strPct = 0.50)
  }
  case object DeepSideGash extends Trauma {
    val name        = "Глубокий разрез правого бока"
    val severity    = Heavy
    val description = "Глубокий разрез правого бока ободрал кожу, из-за чего стали видны рёбра. -50% к HP, -50% к телосложению."
    val penalties   = TraumaPenalties(hpPct = 0.50, vitPct = 0.50)
  }
  case object SplitSkull extends Trauma {
    val name        = "Расколотый череп"
    val severity    = Heavy
    val description = "Ваш череп получил множественные переломы, а мозг — сотрясение, что вызвало отёки мозга и повышенное давление внутри головы. -50% к точности, -50% к сосредоточению, -75% к интеллекту."
    val penalties   = TraumaPenalties(accPct = 0.50, concPct = 0.50, intPct = 0.75)
  }

  val light:  Vector[Trauma] = Vector(BruisedLeg, SmashedFinger, AchingRib, TornEar)
  val medium: Vector[Trauma] = Vector(CutAchilles, PiercedArm, DeepBellyGash, BrokenNose)
  val heavy:  Vector[Trauma] = Vector(KneecapDamage, BrokenArm, DeepSideGash, SplitSkull)
  val all:    Vector[Trauma] = light ++ medium ++ heavy

  def byName(name: String): Option[Trauma] = all.find(_.name == name)
}
