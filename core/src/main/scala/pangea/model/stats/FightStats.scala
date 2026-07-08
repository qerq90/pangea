package pangea.model.stats

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}

case class FightStats(
  atk:      Long,
  hp:       Long,
  armor:    Long,
  defence:  Long,
  evasion:  Long,
  accuracy: Long,
  // Текущий запас энергии (пул героя, как hp/armor). Тратится на активные умения
  // и события лабиринта, восстанавливается в бою и на отдыхе. Максимум считается
  // формулой в Hero.maxEnergy. У мобов не используется (0).
  energy:   Long
)

object FightStats {
  implicit val encoder: Encoder[FightStats] = deriveEncoder[FightStats]

  implicit val decoder: Decoder[FightStats] = (c: HCursor) =>
    for {
      atk      <- c.get[Long]("atk")
      hp       <- c.get[Long]("hp")
      armor    <- c.get[Long]("armor")
      defence  <- c.get[Long]("defence")
      evasion  <- c.getOrElse[Long]("evasion")(0L)
      accuracy <- c.getOrElse[Long]("accuracy")(0L)
      energy   <- c.getOrElse[Long]("energy")(0L)
    } yield FightStats(atk, hp, armor, defence, evasion, accuracy, energy)

  implicit val meta: Meta[FightStats] = new Meta(pgDecoderGet, pgEncoderPut)
}
