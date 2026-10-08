package pangea.model.battle

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/** Переключатели боя, которые герой ставит раз и надолго: кнопка в бою меняет
  * их, и выбор держится до следующего нажатия, а не до конца схватки. Живут в
  * `heroes.battle_prefs`, поэтому кодек рукописный: новая настройка не должна
  * обнулять те, что игрок уже выставил.
  *
  * @param miasma стелет ли герой миазмы тьмы («Некромант», порог 10)
  */
final case class BattlePrefs(miasma: Boolean = true) {
  def withMiasma(on: Boolean): BattlePrefs = copy(miasma = on)
}

object BattlePrefs {
  val default: BattlePrefs = BattlePrefs()

  implicit val encoder: Encoder[BattlePrefs] = (p: BattlePrefs) =>
    Json.obj("miasma" -> p.miasma.asJson)

  implicit val decoder: Decoder[BattlePrefs] = (c: HCursor) =>
    // Миазмы включены по умолчанию: порог 10 на то и брали.
    c.getOrElse[Boolean]("miasma")(true).map(BattlePrefs(_))
}
