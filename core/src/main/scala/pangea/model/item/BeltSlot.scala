package pangea.model.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/** Что лежит в ячейке пояса. Пояс — не предмет с зарядами, а сумка на поясе:
  * сколько в ней ячеек, решает редкость самого пояса ([[ItemDetails.Belt]]), а
  * в ячейке лежит либо фляга со своими зарядами, либо горсть пыли.
  *
  * Внутрь кладутся не целые предметы, а их суть: вид и редкость. По ним вещь
  * собирается заново, когда игрок достаёт её обратно, — у фляг и пыли кроме
  * вида с редкостью ничего своего и нет. */
sealed trait BeltSlot {

  /** Название для списка расходников в бою. */
  def name: String

  /** Сколько раз этим можно воспользоваться: заряды фляги, горсть пыли — один. */
  def uses: Int
}

object BeltSlot {

  /** Фляга в ячейке. Редкость хранится, чтобы знать её запас зарядов и собрать
    * вещь обратно такой, какой её положили. */
  final case class Flask(kind: FlaskKind, rarity: Rarity, charges: Int) extends BeltSlot {
    def name: String      = kind.label
    def uses: Int         = charges
    def maxCharges: Int   = FlaskKind.chargesFor(rarity)
    def spent: Flask      = copy(charges = (charges - 1).max(0))
    def refilled: Flask   = copy(charges = maxCharges)
    def empty: Boolean    = charges <= 0
  }

  /** Горсть пыли: одна ячейка — одна горсть. Высыпанная ячейка помнит, чем
    * была полна: после боя она сама доберёт такую же горсть из сумки, если
    * там есть. */
  final case class Dust(kind: MaterialKind, spent: Boolean = false) extends BeltSlot {
    def name: String  = kind.displayName
    def uses: Int     = if (spent) 0 else 1
    def poured: Dust  = copy(spent = true)
    def refilled: Dust = copy(spent = false)
  }

  /** Что из сумки вообще можно положить на пояс: фляга или пыль. */
  def of(item: Item): Option[BeltSlot] = item.details match {
    case f: ItemDetails.Flask =>
      FlaskKind.values.find(_.effect == f.effect).map(kind => Flask(kind, item.rarity, f.charges))
    case ItemDetails.Material(kind) if kind.gem.isDefined => Some(Dust(kind))
    case _                                                => None
  }

  private val flaskEnc: Encoder[Flask] = (f: Flask) =>
    Json.obj("type" -> "Flask".asJson, "kind" -> f.kind.entryName.asJson,
      "rarity" -> f.rarity.asJson, "charges" -> f.charges.asJson)

  private val dustEnc: Encoder[Dust] = (d: Dust) =>
    Json.obj("type" -> "Dust".asJson, "kind" -> d.kind.entryName.asJson,
      "spent" -> d.spent.asJson)

  implicit val encoder: Encoder[BeltSlot] = Encoder.instance {
    case f: Flask => flaskEnc(f)
    case d: Dust  => dustEnc(d)
  }

  implicit val decoder: Decoder[BeltSlot] = (c: HCursor) =>
    c.get[String]("type").flatMap {
      case "Flask" =>
        for {
          kind    <- c.get[String]("kind").map(FlaskKind.withName)
          rarity  <- c.get[Rarity]("rarity")
          charges <- c.getOrElse[Int]("charges")(0)
        } yield Flask(kind, rarity, charges)
      case "Dust" =>
        for {
          kind  <- c.get[String]("kind").map(MaterialKind.withName)
          spent <- c.getOrElse[Boolean]("spent")(false)
        } yield Dust(kind, spent)
      case other  => Left(io.circe.DecodingFailure(s"Неизвестное содержимое пояса: $other", c.history))
    }
}
