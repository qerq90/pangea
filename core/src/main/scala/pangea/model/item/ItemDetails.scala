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

  /** Пояс — сумка для расходников: в ячейках лежат фляги и горсти пыли
    * ([[BeltSlot]]), а сколько ячеек, решает редкость самого пояса. Склянки,
    * которые пояс носил раньше, спрятаны до лучших времён: старая запись
    * читается как пустой пояс (см. декодер ниже). */
  case class Belt(slots: List[BeltSlot] = Nil) extends ItemDetails {
    def isEmpty: Boolean = slots.isEmpty

    /** Положить, если есть куда. */
    def put(slot: BeltSlot, capacity: Int): Option[Belt] =
      Option.when(slots.sizeIs < capacity)(copy(slots = slots :+ slot))

    /** Достать ячейку по номеру. */
    def at(idx: Int): Option[BeltSlot] = slots.lift(idx)

    def without(idx: Int): Belt = copy(slots = slots.patch(idx, Nil, 1))

    def updated(idx: Int, slot: BeltSlot): Belt = copy(slots = slots.updated(idx, slot))

    /** Пустые фляги с пояса долой: пить из них нечего, а место они держат. */
    def withoutEmpty: Belt = copy(slots = slots.filter(_.uses > 0))
  }

  object Belt {
    /** Сколько ячеек на поясе по его редкости: чёрный/белый — 1, зелёный — 2,
     *  синий — 3, фиол./пурпур — 4, легендарный (оранжевый) — 5. Числа те же,
     *  что были у зелий: сколько склянок пояс носил, столько держит и
     *  расходников. */
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

  /** Рунный камень (см. [[pangea.model.rune.RuneStone]]): ключ руны и размер
   *  плиты. Ключ, а не сама руна, — чтобы `model.item` не зависел от `model.rune`. */
  case class RuneStone(runeKey: String, size: pangea.model.rune.RuneStoneSize) extends ItemDetails

  /** Божественное оружие (см. [[DivineKind]]): вид и сколько ударов ещё держит.
   *  Носится в доп. слоте наравне с обычными вещами этого слота; заряды игроку
   *  не показываются — оружие рассыпается без предупреждения. */
  case class Divine(kind: DivineKind, charges: Int, maxCharges: Int) extends Charged {
    def withCharges(n: Int): Divine = copy(charges = n)
  }

  /** Раскрывшаяся роза (см. [[RoseKind]]): носится в доп. слоте и бьёт по всему
   *  полю, пока не кончатся раскрытия. */
  case class Rose(kind: RoseKind, charges: Int, maxCharges: Int) extends Charged {
    def withCharges(n: Int): Rose = copy(charges = n)
  }

  // --- Покодечная сериализация с диспатчем по "type" ---

  private val weaponEnc: Encoder[Weapon]           = deriveEncoder
  private val weaponDec: Decoder[Weapon]           = deriveDecoder
  private val armorEnc:  Encoder[Armor]            = deriveEncoder
  private val armorDec:  Decoder[Armor]            = deriveDecoder
  private val flaskEnc:  Encoder[Flask]            = deriveEncoder
  private val flaskDec:  Decoder[Flask]            = deriveDecoder
  private val beltEnc:   Encoder[Belt]             = deriveEncoder
  /** Пояс со склянкой — запись прежних времён: читаем её как пустой пояс.
    * Склянки спрятаны, и возвращать их в чужую ячейку незачем. */
  private val beltDec:   Decoder[Belt]             = (c: HCursor) =>
    c.getOrElse[List[BeltSlot]]("slots")(Nil).map(Belt(_))
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
  private val divineEnc:  Encoder[Divine]            = deriveEncoder
  private val divineDec:  Decoder[Divine]            = deriveDecoder
  private val roseEnc:    Encoder[Rose]              = deriveEncoder
  private val roseDec:    Decoder[Rose]              = deriveDecoder
  private val runeEnc:    Encoder[RuneStone]         = deriveEncoder
  private val runeDec:    Decoder[RuneStone]         = deriveDecoder

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
    case d: Divine      => tagged("Divine", divineEnc(d))
    case r: Rose        => tagged("Rose", roseEnc(r))
    case r: RuneStone   => tagged("RuneStone", runeEnc(r))
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
      case "Divine"      => divineDec(c)
      case "Rose"        => roseDec(c)
      case "RuneStone"   => runeDec(c)
      case other         => Left(DecodingFailure(s"Unknown ItemDetails type: $other", c.history))
    }
  }
}
