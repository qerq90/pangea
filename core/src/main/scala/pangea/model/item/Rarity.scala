package pangea.model.item

import enumeratum._
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

import pangea.domain.Rng

sealed trait Rarity extends EnumEntry {
  val factorR: Double
  val factorR1: Double
  val factorR3: Double
  val paramsChance: List[Long]

  def getNumOfExtraParams(rng: Rng): (Long, Rng) =
    paramsChance.foldLeft((0L, rng)) { case ((acc, r), chance) =>
      val (roll, next) = r.between(0L, 100L)
      ((roll + chance) / 100L + acc, next)
    }
}

object Rarity extends Enum[Rarity] with DoobieEnum[Rarity] {

  val values = findValues

  case object Gray extends Rarity {
    override val factorR: Double          = 2
    override val factorR1: Double         = 0.1
    override val factorR3: Double         = 2
    override val paramsChance: List[Long] = List(10)
  }

  case object White extends Rarity {
    override val factorR: Double          = 3
    override val factorR1: Double         = 0.15
    override val factorR3: Double         = 2
    override val paramsChance: List[Long] = List(30)
  }

  case object Green extends Rarity {
    override val factorR: Double          = 4
    override val factorR1: Double         = 0.2
    override val factorR3: Double         = 4
    override val paramsChance: List[Long] = List(70, 50)
  }

  case object Blue extends Rarity {
    override val factorR: Double          = 6
    override val factorR1: Double         = 0.3
    override val factorR3: Double         = 6
    override val paramsChance: List[Long] = List(100, 100)
  }

  case object Purple extends Rarity {
    override val factorR: Double          = 8
    override val factorR1: Double         = 0.5
    override val factorR3: Double         = 8
    override val paramsChance: List[Long] = List(100, 100, 70, 50)
  }

  case object Violet extends Rarity {
    override val factorR: Double          = 8
    override val factorR1: Double         = 0.5
    override val factorR3: Double         = 8
    override val paramsChance: List[Long] = List(100, 100, 100, 50)
  }

  case object Orange extends Rarity {
    override val factorR: Double          = 10
    override val factorR1: Double         = 0.7
    override val factorR3: Double         = 10
    override val paramsChance: List[Long] = List(100, 100, 100, 100, 50)
  }

  implicit val encoder: Encoder[Rarity] = (rarity: Rarity) =>
    rarity.entryName.asJson

  implicit val decoder: Decoder[Rarity] = (c: HCursor) =>
    c.as[String].map(Rarity.withName)
}
