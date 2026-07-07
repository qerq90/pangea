package pangea.model.item

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import pangea.model.skill.Skill

/** Специфичные для типа предмета данные. Общие боевые статы остаются плоскими на
 *  [[Item]]; сюда вынесено то, что есть лишь у части типов, чтобы «спросить яд у
 *  шлема» было невозможно на уровне компилятора — доступ только через `match`.
 *
 *  Сериализация: у каждого варианта свой кодек, объединённый диспатчем по полю
 *  `type` (см. [[ItemDetails.encoder]]/[[ItemDetails.decoder]]). */
sealed trait ItemDetails

object ItemDetails {

  /** У предмета нет спец-данных (большинство брони/оружия без активного навыка,
   *  а также NoItem). */
  case object Plain extends ItemDetails

  /** Оружие с активным навыком (один из `Skill.weaponSkills`). */
  case class Weapon(activeSkill: Skill) extends ItemDetails

  /** Нагрудник с активным навыком (один из `Skill.armorSkills`). */
  case class Armor(activeSkill: Skill) extends ItemDetails

  /** Предмет с зарядами (фляга, пояс). Операции над зарядами живут здесь —
   *  тотальные, без «тихого no-op» на неверном типе. */
  sealed trait Charged extends ItemDetails {
    def charges: Int
    def maxCharges: Int
    def withCharges(n: Int): Charged
    def refilled: Charged = withCharges(maxCharges)
    def spent: Charged    = withCharges((charges - 1).max(0))
  }

  case class Flask(effect: FlaskEffect, charges: Int, maxCharges: Int) extends Charged {
    def withCharges(n: Int): Flask = copy(charges = n)
  }

  case class Belt(potion: PotionKind, charges: Int, maxCharges: Int) extends Charged {
    def withCharges(n: Int): Belt = copy(charges = n)
  }

  object Belt {
    /** Длительность (в ходах) временных бафов от зелий пояса — атака/защита/уворот/
     *  концентрация. */
    val BuffRounds: Int = 5

    /** Вместимость зелий пояса (число зарядов) по редкости: чёрный/белый — 1,
     *  зелёный — 2, синий — 3, фиол./пурпур — 4, легендарный (оранжевый) — 5. */
    def capacityFor(rarity: Rarity): Int = rarity match {
      case Rarity.Gray | Rarity.White    => 1
      case Rarity.Green                  => 2
      case Rarity.Blue                   => 3
      case Rarity.Purple | Rarity.Violet => 4
      case Rarity.Orange                 => 5
    }
  }

  /** Трофей с убитого моба: раса (entryName) и вид трофея. */
  case class Trophy(race: String, kind: TrophyKind) extends ItemDetails

  /** Карта клада или её половинка (целость кодирует `Item.itemType`). */
  case class TreasureMap(zone: MapZone) extends ItemDetails

  // --- Покодечная сериализация с диспатчем по "type" ---

  private val weaponEnc: Encoder[Weapon]           = deriveEncoder
  private val weaponDec: Decoder[Weapon]           = deriveDecoder
  private val armorEnc:  Encoder[Armor]            = deriveEncoder
  private val armorDec:  Decoder[Armor]            = deriveDecoder
  private val flaskEnc:  Encoder[Flask]            = deriveEncoder
  private val flaskDec:  Decoder[Flask]            = deriveDecoder
  private val beltEnc:   Encoder[Belt]             = deriveEncoder
  private val beltDec:   Decoder[Belt]             = deriveDecoder
  private val trophyEnc: Encoder[Trophy]           = deriveEncoder
  private val trophyDec: Decoder[Trophy]           = deriveDecoder
  private val mapEnc:    Encoder[TreasureMap]      = deriveEncoder
  private val mapDec:    Decoder[TreasureMap]      = deriveDecoder

  private def tagged(tpe: String, body: Json): Json =
    body.deepMerge(Json.obj("type" -> tpe.asJson))

  implicit val encoder: Encoder[ItemDetails] = Encoder.instance {
    case Plain          => Json.obj("type" -> "Plain".asJson)
    case w: Weapon      => tagged("Weapon", weaponEnc(w))
    case a: Armor       => tagged("Armor", armorEnc(a))
    case f: Flask       => tagged("Flask", flaskEnc(f))
    case b: Belt        => tagged("Belt", beltEnc(b))
    case t: Trophy      => tagged("Trophy", trophyEnc(t))
    case m: TreasureMap => tagged("TreasureMap", mapEnc(m))
  }

  implicit val decoder: Decoder[ItemDetails] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "Plain"       => Right(Plain)
      case "Weapon"      => weaponDec(c)
      case "Armor"       => armorDec(c)
      case "Flask"       => flaskDec(c)
      case "Belt"        => beltDec(c)
      case "Trophy"      => trophyDec(c)
      case "TreasureMap" => mapDec(c)
      case other         => Left(DecodingFailure(s"Unknown ItemDetails type: $other", c.history))
    }
  }
}
