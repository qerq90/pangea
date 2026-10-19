package pangea.model.item

/** Числа щита. Щит держат в руке, как одноручное оружие, но это не оружие: с
  * двумя щитами герой бьёт голыми руками (см. `Equipment.unarmed`). Умение у
  * щита одно и всегда своё — Блок (см. `Skill.Block`). */
object ShieldRates {

  /** Защита, которую щит даёт всегда: три за каждый уровень вещи, поверх
    * случайных статов. */
  val DefencePerLvl: Long = 3L

  /** Гнёзд у щита — по таблице оружия, но не больше двух. */
  val MaxSockets: Int = 2

  /** Сколько урона снимает Блок у серого и белого щита, %. */
  val BaseBlockPct: Long = 50L

  /** И сколько добавляет каждая редкость выше белой, %. */
  val BlockPctPerRarity: Long = 5L

  /** Сколько урона снимает Блок у щита этой редкости: серый и белый — половину,
    * дальше по пять процентов за каждую редкость выше белой. Фиолетовая и
    * пурпурная — разные редкости, так что легендарный снимает 75%. */
  def blockPct(rarity: Rarity): Long =
    BaseBlockPct + BlockPctPerRarity * (Rarity.values.indexOf(rarity) - Rarity.values.indexOf(Rarity.White)).max(0)
}
