package pangea.model.item

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
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
    /** Длительность (в ходах) временных бафов от зелий пояса — атака/защита/уворот. */
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

  /** Пассивный навык предмета (один из `PassiveKind.poolFor(itemType)`). Падает на
   *  слоты, не несущие активного навыка/зелья (см. [[PassiveKind]]). */
  case class Passive(kind: PassiveKind) extends ItemDetails

  /** Трофей с убитого моба: раса (entryName) и вид трофея. `coef` задан только у
   *  трофеев с собственным коэффициентом (клык Белого волка: 6 × BossLvL); у
   *  остальных он берётся с вида. */
  case class Trophy(race: String, kind: TrophyKind, coef: Option[Double] = None) extends ItemDetails {
    def coefValue: Double = coef.getOrElse(kind.coef)
  }

  /** Карта клада или её половинка (целость кодирует `Item.itemType`). */
  case class TreasureMap(zone: MapZone) extends ItemDetails

  /** Камень-усилитель как предмет инвентаря (вид + грейд). */
  case class Gem(gem: pangea.model.item.Gem) extends ItemDetails

  /** Материал-ингредиент (мифрил и т.п.). */
  case class Material(kind: MaterialKind) extends ItemDetails

  /** Сюжетный предмет — какой именно (см. [[QuestItemKind]]). */
  case class Quest(kind: QuestItemKind) extends ItemDetails

  /** Отвар из трав — какой именно (см. [[BrewKind]]). */
  case class Brew(kind: BrewKind) extends ItemDetails

  /** Реликвия в доп. слоте (см. [[RelicKind]]): вид и сколько ударов ещё держит.
   *  Заряды игроку не показываются — реликвия рассыпается без предупреждения. */
  case class Relic(kind: RelicKind, charges: Int, maxCharges: Int) extends Charged {
    def withCharges(n: Int): Relic = copy(charges = n)
  }

  // --- Покодечная сериализация с диспатчем по "type" ---

  private val weaponEnc: Encoder[Weapon]           = deriveEncoder
  private val weaponDec: Decoder[Weapon]           = deriveDecoder
  private val armorEnc:  Encoder[Armor]            = deriveEncoder
  private val armorDec:  Decoder[Armor]            = deriveDecoder
  private val flaskEnc:  Encoder[Flask]            = deriveEncoder
  private val flaskDec:  Decoder[Flask]            = deriveDecoder
  private val beltEnc:   Encoder[Belt]             = deriveEncoder
  private val beltDec:   Decoder[Belt]             = deriveDecoder
  private val passiveEnc: Encoder[Passive]         = deriveEncoder
  private val passiveDec: Decoder[Passive]         = deriveDecoder
  private val trophyEnc: Encoder[Trophy]           = deriveEncoder
  // Рукописный: старые трофеи записаны без `coef`, и производный декодер их бы
  // не прочёл (см. LoreData — та же история).
  private val trophyDec: Decoder[Trophy]           = (c: HCursor) =>
    for {
      race <- c.get[String]("race")
      kind <- c.get[TrophyKind]("kind")
      coef <- c.getOrElse[Option[Double]]("coef")(None)
    } yield Trophy(race, kind, coef)
  private val mapEnc:    Encoder[TreasureMap]      = deriveEncoder
  private val mapDec:    Decoder[TreasureMap]      = deriveDecoder
  private val gemEnc:    Encoder[Gem]              = deriveEncoder
  private val gemDec:    Decoder[Gem]              = deriveDecoder
  private val matEnc:    Encoder[Material]         = deriveEncoder
  private val matDec:    Decoder[Material]         = deriveDecoder
  private val questEnc:  Encoder[Quest]            = deriveEncoder
  private val questDec:  Decoder[Quest]            = deriveDecoder
  private val brewEnc:   Encoder[Brew]             = deriveEncoder
  private val brewDec:   Decoder[Brew]             = deriveDecoder
  private val relicEnc:  Encoder[Relic]            = deriveEncoder
  private val relicDec:  Decoder[Relic]            = deriveDecoder

  private def tagged(tpe: String, body: Json): Json =
    body.deepMerge(Json.obj("type" -> tpe.asJson))

  implicit val encoder: Encoder[ItemDetails] = Encoder.instance {
    case Plain          => Json.obj("type" -> "Plain".asJson)
    case w: Weapon      => tagged("Weapon", weaponEnc(w))
    case a: Armor       => tagged("Armor", armorEnc(a))
    case f: Flask       => tagged("Flask", flaskEnc(f))
    case b: Belt        => tagged("Belt", beltEnc(b))
    case p: Passive     => tagged("Passive", passiveEnc(p))
    case t: Trophy      => tagged("Trophy", trophyEnc(t))
    case m: TreasureMap => tagged("TreasureMap", mapEnc(m))
    case g: Gem         => tagged("Gem", gemEnc(g))
    case m: Material    => tagged("Material", matEnc(m))
    case q: Quest       => tagged("Quest", questEnc(q))
    case b: Brew        => tagged("Brew", brewEnc(b))
    case r: Relic       => tagged("Relic", relicEnc(r))
  }

  implicit val decoder: Decoder[ItemDetails] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "Plain"       => Right(Plain)
      case "Weapon"      => weaponDec(c)
      case "Armor"       => armorDec(c)
      case "Flask"       => flaskDec(c)
      case "Belt"        => beltDec(c)
      case "Passive"     => passiveDec(c)
      case "Trophy"      => trophyDec(c)
      case "TreasureMap" => mapDec(c)
      case "Gem"         => gemDec(c)
      case "Material"    => matDec(c)
      case "Quest"       => questDec(c)
      case "Brew"        => brewDec(c)
      case "Relic"       => relicDec(c)
      case other         => Left(DecodingFailure(s"Unknown ItemDetails type: $other", c.history))
    }
  }
}
