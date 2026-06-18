package pangea.model.trauma

sealed trait Trauma {
  def name: String
  def modifier: Double  // multiplier applied to stats, e.g. 0.95 = -5%
}

object Trauma {
  // Light: -5%
  case object BruisedLeg    extends Trauma { val name = "Ушиб ноги";                         val modifier = 0.95 }
  case object SmashedFinger extends Trauma { val name = "Раздробленный палец";               val modifier = 0.95 }
  case object AchingRib     extends Trauma { val name = "Ноющее ребро";                      val modifier = 0.95 }
  case object TornEar       extends Trauma { val name = "Порванное ухо";                     val modifier = 0.95 }
  // Medium: -25%
  case object CutAchilles   extends Trauma { val name = "Порезанное ахиллесово сухожилие";  val modifier = 0.75 }
  case object PiercedArm    extends Trauma { val name = "Проколотая рука";                   val modifier = 0.75 }
  case object DeepBellyGash extends Trauma { val name = "Глубокий порез живота";             val modifier = 0.75 }
  case object BrokenNose    extends Trauma { val name = "Сломанный нос";                     val modifier = 0.75 }
  // Heavy: -50%
  case object KneecapDamage extends Trauma { val name = "Повреждение коленной чашечки";      val modifier = 0.50 }
  case object BrokenArm     extends Trauma { val name = "Сломанная рука";                    val modifier = 0.50 }
  case object DeepSideGash  extends Trauma { val name = "Глубокий разрез бока";              val modifier = 0.50 }
  case object SplitSkull    extends Trauma { val name = "Расколотый череп";                  val modifier = 0.50 }

  val all: Vector[Trauma] = Vector(
    BruisedLeg, SmashedFinger, AchingRib, TornEar,
    CutAchilles, PiercedArm, DeepBellyGash, BrokenNose,
    KneecapDamage, BrokenArm, DeepSideGash, SplitSkull
  )

  def byName(name: String): Option[Trauma] = all.find(_.name == name)
}
