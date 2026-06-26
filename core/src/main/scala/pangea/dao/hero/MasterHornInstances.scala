package pangea.dao.hero

import doobie.postgres.circe.jsonb.implicits._
import doobie.util.{Get, Put}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}

/** Doobie Meta для `master_horn_boosts JSONB` — `Map[String, Int]`. */
object MasterHornInstances {
  private implicit val mapEnc: Encoder[Map[String, Int]] = Encoder.encodeMap[String, Int]
  private implicit val mapDec: Decoder[Map[String, Int]] = Decoder.decodeMap[String, Int]

  implicit val getMasterHornBoosts: Get[Map[String, Int]] =
    Get[Json].temap(_.as[Map[String, Int]].left.map(_.getMessage))

  implicit val putMasterHornBoosts: Put[Map[String, Int]] =
    Put[Json].contramap[Map[String, Int]](_.asJson)
}
