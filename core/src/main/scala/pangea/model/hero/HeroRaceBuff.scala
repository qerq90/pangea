package pangea.model.hero

import pangea.model.monster.Race

/** Расовые множители финальных характеристик игрока: тело/сила/интеллект/ловкость.
 *  Применяются только к героям. Округление результата умножения — В БОЛЬШУЮ СТОРОНУ
 *  (см. HeroRaceBuff.apply). */
case class HeroRaceBuff(
  vitBuff: Double,
  strBuff: Double,
  intBuff: Double,
  agiBuff: Double
) {
  def applyVit(v: Long): Long = HeroRaceBuff.ceil(v, vitBuff)
  def applyStr(v: Long): Long = HeroRaceBuff.ceil(v, strBuff)
  def applyInt(v: Long): Long = HeroRaceBuff.ceil(v, intBuff)
  def applyAgi(v: Long): Long = HeroRaceBuff.ceil(v, agiBuff)
}

object HeroRaceBuff {
  private def ceil(v: Long, m: Double): Long = math.ceil(v.toDouble * m).toLong

  private val byRace: Map[Race, HeroRaceBuff] = Map(
    Race.Murloc  -> HeroRaceBuff(vitBuff = 0.8, strBuff = 0.8, intBuff = 1.2, agiBuff = 1.2),
    Race.Elf     -> HeroRaceBuff(vitBuff = 1.0, strBuff = 0.8, intBuff = 1.1, agiBuff = 1.1),
    Race.Orc     -> HeroRaceBuff(vitBuff = 1.2, strBuff = 1.2, intBuff = 0.8, agiBuff = 0.8),
    Race.Goblin  -> HeroRaceBuff(vitBuff = 0.9, strBuff = 1.1, intBuff = 0.8, agiBuff = 1.2),
    Race.Demon   -> HeroRaceBuff(vitBuff = 0.8, strBuff = 1.2, intBuff = 1.2, agiBuff = 0.8),
    Race.Gnome   -> HeroRaceBuff(vitBuff = 1.2, strBuff = 1.1, intBuff = 0.9, agiBuff = 0.8),
    Race.Human   -> HeroRaceBuff(vitBuff = 1.0, strBuff = 1.0, intBuff = 1.0, agiBuff = 1.0),
    Race.Khajiit -> HeroRaceBuff(vitBuff = 0.8, strBuff = 1.0, intBuff = 1.0, agiBuff = 1.2)
  )

  def of(race: Race): HeroRaceBuff = byRace(race)
}
