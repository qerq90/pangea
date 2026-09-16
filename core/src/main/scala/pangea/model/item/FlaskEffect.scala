package pangea.model.item

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.battle.{Buff, Element}

/** Что делает фляга при использовании в бою. Разновидности перечислены в
 *  [[FlaskKind]] — там же имена, описания и числа; здесь только сама механика,
 *  которую хранит предмет ([[ItemDetails.Flask]]). Глоток раунд не завершает и
 *  делит с зельем пояса правило «один расходник за раунд». */
sealed trait FlaskEffect {

  /** Чем эта фляга заправляется (см. [[RefillSource]]). */
  def refillSource: RefillSource = this match {
    case FlaskEffect.HealPercent(_) | FlaskEffect.AddBuff(_, _)                                     => RefillSource.Water
    case FlaskEffect.ArmorPercent(_) | FlaskEffect.EnergyPercent(_) | FlaskEffect.Splash(_) |
         FlaskEffect.Cleanse                                                                        => RefillSource.Alchemy
    case FlaskEffect.Smoke(_)                                                                       => RefillSource.Sleep
    case FlaskEffect.Vampiric(_, _)                                                                 => RefillSource.Blood
    case FlaskEffect.PoisonCoat(_)                                                                  => RefillSource.Poison
    case FlaskEffect.BleedCoat(_)                                                                   => RefillSource.Bleed
  }

  /** Ручей наполняет только воду — флягу целителя. */
  def refillsAtSpring: Boolean = refillSource == RefillSource.Water

  /** Густаво заправляет всё, кроме вампирской: та пьёт кровь сама. */
  def refillsAtGustavo: Boolean = refillSource != RefillSource.Blood

  /** Короткая подпись для кнопки боя: «🧪 Кузнец (3)». */
  def shortLabel: String = this match {
    case FlaskEffect.HealPercent(_)   => "Целитель"
    case FlaskEffect.AddBuff(_, _)    => "Настой"
    case FlaskEffect.ArmorPercent(_)  => "Кузнец"
    case FlaskEffect.EnergyPercent(_) => "Бодрость"
    case FlaskEffect.Splash(e)        => FlaskKind.splashShort(e)
    case FlaskEffect.Cleanse          => "Очищение"
    case FlaskEffect.Smoke(_)         => "Дым"
    case FlaskEffect.Vampiric(_, _)   => "Вампир"
    case FlaskEffect.PoisonCoat(_)    => "Яд"
    case FlaskEffect.BleedCoat(_)     => "Кровь"
  }

  /** Описание для инвентаря. */
  def describe: String = this match {
    case FlaskEffect.HealPercent(pct)   => s"Глоток: +$pct% макс. HP и +${FlaskRates.HealEnergyPct}% энергии."
    case FlaskEffect.AddBuff(_, rounds) => s"Глоток: настой действует $rounds раунда."
    case FlaskEffect.ArmorPercent(pct)  => s"Глоток: +$pct% макс. брони."
    case FlaskEffect.EnergyPercent(pct) => s"Глоток: +$pct% макс. энергии."
    case FlaskEffect.Splash(e)          => s"Плеснуть во врага: ${FlaskKind.splashText(e)}"
    case FlaskEffect.Cleanse            => "Глоток: снимает с вас горение, яд, кровотечение и все дебафы."
    case FlaskEffect.Smoke(rounds)      => s"Разбить о землю: $rounds раунда дым прячет вас от мобов вне пары — они не бьют сбоку и не помогают друг другу."
    case FlaskEffect.Vampiric(hits, pct) => s"Глоток: следующие $hits удара по HP лечат вас на $pct% нанесённого урона."
    case FlaskEffect.PoisonCoat(rounds) => s"Смазать оружие: $rounds раундов удары по HP травят врага."
    case FlaskEffect.BleedCoat(rounds)  => s"Смазать оружие: $rounds раундов удары по HP пускают врагу кровь."
  }
}

/** Чем заправляется фляга. Ручей — только вода; отвары — по своему источнику
 *  (см. BrewEffect.RefillFlask); Густаво — всё, кроме крови; кровь фляга берёт
 *  сама — с убитых, по шансу (см. FlaskRates.VampiricRefillPct). */
sealed trait RefillSource
object RefillSource {
  case object Water   extends RefillSource
  case object Alchemy extends RefillSource
  case object Sleep   extends RefillSource
  case object Blood   extends RefillSource
  case object Poison  extends RefillSource
  case object Bleed   extends RefillSource
}

object FlaskEffect {
  /** Фляга целителя: % макс. HP (и немного энергии — см. [[FlaskRates.HealEnergyPct]]). */
  case class HealPercent(percent: Int)              extends FlaskEffect
  /** Старый настой с бафом — в игре не выдаётся, оставлен для чтения записей. */
  case class AddBuff(buff: Buff, rounds: Int)       extends FlaskEffect
  /** Фляга кузнеца: % макс. брони. */
  case class ArmorPercent(percent: Int)             extends FlaskEffect
  /** Фляга бодрости: % макс. энергии. */
  case class EnergyPercent(percent: Int)            extends FlaskEffect
  /** Стихийная фляга: гарантированный прок стихии по врагу, без урона. */
  case class Splash(element: Element)               extends FlaskEffect
  /** Фляга очищения: снимает с героя всё вредное. */
  case object Cleanse                               extends FlaskEffect
  /** Дымная фляга: `rounds` раундов мобы вне пары не бьют сбоку и не помогают. */
  case class Smoke(rounds: Int)                     extends FlaskEffect
  /** Вампирская фляга: `hits` ударов по HP лечат на `percent` урона. */
  case class Vampiric(hits: Int, percent: Int)      extends FlaskEffect
  /** Фляга яда: `rounds` раундов удары по HP травят. */
  case class PoisonCoat(rounds: Int)                extends FlaskEffect
  /** Фляга крови: `rounds` раундов удары по HP пускают кровь. */
  case class BleedCoat(rounds: Int)                 extends FlaskEffect

  implicit val healPercentEncoder: Encoder[HealPercent]     = deriveEncoder
  implicit val healPercentDecoder: Decoder[HealPercent]     = deriveDecoder
  implicit val addBuffEncoder:     Encoder[AddBuff]         = deriveEncoder
  implicit val addBuffDecoder:     Decoder[AddBuff]         = deriveDecoder
  implicit val armorEncoder:       Encoder[ArmorPercent]    = deriveEncoder
  implicit val armorDecoder:       Decoder[ArmorPercent]    = deriveDecoder
  implicit val energyEncoder:      Encoder[EnergyPercent]   = deriveEncoder
  implicit val energyDecoder:      Decoder[EnergyPercent]   = deriveDecoder
  implicit val splashEncoder:      Encoder[Splash]          = deriveEncoder
  implicit val splashDecoder:      Decoder[Splash]          = deriveDecoder
  implicit val smokeEncoder:       Encoder[Smoke]           = deriveEncoder
  implicit val smokeDecoder:       Decoder[Smoke]           = deriveDecoder
  implicit val vampiricEncoder:    Encoder[Vampiric]        = deriveEncoder
  implicit val vampiricDecoder:    Decoder[Vampiric]        = deriveDecoder
  implicit val poisonCoatEncoder:  Encoder[PoisonCoat]      = deriveEncoder
  implicit val poisonCoatDecoder:  Decoder[PoisonCoat]      = deriveDecoder
  implicit val bleedCoatEncoder:   Encoder[BleedCoat]       = deriveEncoder
  implicit val bleedCoatDecoder:   Decoder[BleedCoat]       = deriveDecoder

  implicit val encoder: Encoder[FlaskEffect] = deriveEncoder
  implicit val decoder: Decoder[FlaskEffect] = deriveDecoder
}
