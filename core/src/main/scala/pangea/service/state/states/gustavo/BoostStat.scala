package pangea.service.state.states.gustavo

import pangea.model.stats.ParamsBuff

/** Характеристики, которые бафает Густаво своими зельями. `key` — идентичность
 *  (используется в `StatBoost.name` и в списке уже-использованных бесплатных зелий),
 *  `label` — подпись кнопки, `potion` — название зелья для флейвора, `buff` — сама
 *  прибавка (+15% к соответствующей характеристике). */
sealed abstract class BoostStat(
  val key:    String,
  val label:  String,
  val potion: String,
  val buff:   ParamsBuff
) {
  /** Имя бафа в `StatBoosts` — повтор того же ключа не стакается, а вытесняет прежний. */
  def boostName: String = s"gustavo:$key"
}

object BoostStat {
  case object Str extends BoostStat("str", "Сила",         "зелье быка",       ParamsBuff(15, 0, 0, 0))
  case object Vit extends BoostStat("vit", "Телосложение", "зелье медоеда",    ParamsBuff(0, 15, 0, 0))
  case object Agi extends BoostStat("agi", "Ловкость",     "зелье кошки",      ParamsBuff(0, 0, 15, 0))
  case object Int extends BoostStat("int", "Интеллект",    "зелье злобомозга", ParamsBuff(0, 0, 0, 15))

  val all: List[BoostStat] = List(Str, Vit, Agi, Int)

  def byKey(k: String): Option[BoostStat] = all.find(_.key == k)
}
