package pangea.model.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/** Что лежит в ячейке пояса. Пояс — не предмет с зарядами, а сумка на поясе:
  * сколько в ней ячеек, решает редкость самого пояса ([[ItemDetails.Belt]]), а
  * в ячейке лежит фляга со своими зарядами, горсть пыли, склянка отвара,
  * раскрывшаяся роза со своими раскрытиями или склянка святой воды. Одна
  * ячейка — один предмет.
  *
  * Внутрь кладутся не целые предметы, а их суть: вид и редкость. По ним вещь
  * собирается заново, когда игрок достаёт её обратно, — у фляг, пыли, отваров
  * и цветков кроме вида с запасом ничего своего и нет. */
sealed trait BeltSlot {

  /** Название для списка расходников в бою. */
  def name: String

  /** Сколько раз этим можно воспользоваться: заряды фляги, горсть пыли — один. */
  def uses: Int

  /** Ячейка после того, как из неё взяли одно: у фляги и розы — заряд, прочее
    * — целиком. Пустая ячейка помнит, чем была полна. */
  def taken: BeltSlot
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
    def taken: BeltSlot   = spent
  }

  /** Горсть пыли: одна ячейка — одна горсть. Высыпанная ячейка помнит, чем
    * была полна: после боя она сама доберёт такую же горсть из сумки, если
    * там есть. */
  final case class Dust(kind: MaterialKind, spent: Boolean = false) extends BeltSlot {
    def name: String  = kind.displayName
    def uses: Int     = if (spent) 0 else 1
    def poured: Dust  = copy(spent = true)
    def refilled: Dust = copy(spent = false)
    def taken: BeltSlot = poured
  }

  /** Склянка отвара в ячейке: одна ячейка — одна склянка, стопка в сумке
    * остаётся стопкой. Пустая ячейка помнит, чем была полна: после боя она
    * сама доберёт такой же отвар из сумки, если он там есть. */
  final case class Brew(kind: BrewKind, spent: Boolean = false) extends BeltSlot {
    def name: String   = kind.label
    def uses: Int      = if (spent) 0 else 1
    def drunk: Brew    = copy(spent = true)
    def refilled: Brew = copy(spent = false)
    def taken: BeltSlot = drunk
  }

  /** Раскрывшаяся роза в ячейке: цветок ложится целиком, со всеми своими
    * раскрытиями. Осыпавшаяся ячейка тоже помнит цветок — после боя доберёт
    * такой же из сумки, если он там есть. */
  final case class Rose(kind: RoseKind, charges: Int) extends BeltSlot {
    def name: String      = kind.itemName
    def uses: Int         = charges
    def maxCharges: Int   = RoseRates.Charges
    def spent: Rose       = copy(charges = (charges - 1).max(0))
    def refilled: Rose    = copy(charges = maxCharges)
    def withered: Boolean = charges <= 0
    def taken: BeltSlot   = spent
  }

  /** Нальёт ли ячейку ручей: только флягу с водой (см. [[FlaskEffect.refillsAtSpring]]). */
  def refillsAtSpring(slot: BeltSlot): Boolean = slot match {
    case f: Flask => f.kind.effect.refillsAtSpring
    case _        => false
  }

  /** Склянка святой воды: одна ячейка — одна склянка. Пустая помнит, чем была
    * полна, и после боя доберёт такую же из сумки, если там есть. */
  final case class HolyWater(spent: Boolean = false) extends BeltSlot {
    def name: String        = MaterialKind.HolyWater.displayName
    def uses: Int           = if (spent) 0 else 1
    def poured: HolyWater   = copy(spent = true)
    def refilled: HolyWater = copy(spent = false)
    def taken: BeltSlot     = poured
  }

  /** Что из сумки вообще можно положить на пояс: фляга, пыль, отвар (кроме
    * того, что в бою ни к чему — см. [[BrewKind.beltable]]) или цветок. */
  def of(item: Item): Option[BeltSlot] = item.details match {
    case f: ItemDetails.Flask =>
      FlaskKind.values.find(_.effect == f.effect).map(kind => Flask(kind, item.rarity, f.charges))
    case ItemDetails.Material(MaterialKind.HolyWater)     => Some(HolyWater())
    case ItemDetails.Material(kind) if kind.gem.isDefined => Some(Dust(kind))
    case ItemDetails.Brew(kind) if kind.beltable          => Some(Brew(kind))
    case r: ItemDetails.Rose                              => Some(Rose(r.kind, r.charges))
    case _                                                => None
  }

  private val flaskEnc: Encoder[Flask] = (f: Flask) =>
    Json.obj("type" -> "Flask".asJson, "kind" -> f.kind.entryName.asJson,
      "rarity" -> f.rarity.asJson, "charges" -> f.charges.asJson)

  private val dustEnc: Encoder[Dust] = (d: Dust) =>
    Json.obj("type" -> "Dust".asJson, "kind" -> d.kind.entryName.asJson,
      "spent" -> d.spent.asJson)

  private val brewEnc: Encoder[Brew] = (b: Brew) =>
    Json.obj("type" -> "Brew".asJson, "kind" -> b.kind.entryName.asJson,
      "spent" -> b.spent.asJson)

  private val holyEnc: Encoder[HolyWater] = (h: HolyWater) =>
    Json.obj("type" -> "Holy".asJson, "spent" -> h.spent.asJson)

  private val roseEnc: Encoder[Rose] = (r: Rose) =>
    Json.obj("type" -> "Rose".asJson, "kind" -> r.kind.entryName.asJson,
      "charges" -> r.charges.asJson)

  implicit val encoder: Encoder[BeltSlot] = Encoder.instance {
    case f: Flask => flaskEnc(f)
    case d: Dust  => dustEnc(d)
    case b: Brew  => brewEnc(b)
    case r: Rose  => roseEnc(r)
    case h: HolyWater => holyEnc(h)
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
      case "Brew" =>
        for {
          kind  <- c.get[String]("kind").map(BrewKind.withName)
          spent <- c.getOrElse[Boolean]("spent")(false)
        } yield Brew(kind, spent)
      case "Rose" =>
        for {
          kind    <- c.get[String]("kind").map(RoseKind.withName)
          charges <- c.getOrElse[Int]("charges")(0)
        } yield Rose(kind, charges)
      case "Holy" => c.getOrElse[Boolean]("spent")(false).map(HolyWater(_))
      case other  => Left(io.circe.DecodingFailure(s"Неизвестное содержимое пояса: $other", c.history))
    }
}
