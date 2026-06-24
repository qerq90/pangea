package pangea.model.monster

import pangea.model.stats.FightStats

case class Monster(
  id: Long,
  lvl: Long,
  race: Race,
  rarity: Rarity,
  fightStats: FightStats,
  marked: Boolean = false // модификатор «Отмеченный тьмой»
) {
  def name: String = {
    val base = s"${rarity} ${race}"
    if (marked) s"${Monster.MarkedPrefix} $base" else base
  }
}

object Monster {
  val MarkedPrefix: String = "Отмеченный тьмой"
}
