package pangea.model.hero

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.monster.Race

/** Сколько кого герой положил и кому за это уже прилетело.
  *
  * Считаются только основные расы: у элементалей, зверья и нежити мстить
  * некому — у них нет ни родни, ни счётов (см. [[Race.mortals]]).
  *
  * @param byRace  раса (entryName) → сколько её положили за всю жизнь
  * @param avenged раса → сколько раз она уже приходила за расплатой
  */
final case class KillLog(byRace: Map[String, Long] = Map.empty, avenged: Map[String, Int] = Map.empty) {

  def count(race: Race): Long = byRace.getOrElse(race.entryName, 0L)

  def avengedTimes(race: Race): Int = avenged.getOrElse(race.entryName, 0)

  /** Записать павших. Чужие расы молча пропускаются. */
  def add(races: List[String]): KillLog = {
    val mortal = races.filter(r => Race.withNameOption(r).exists(Race.mortals.contains))
    if (mortal.isEmpty) this
    else copy(byRace = mortal.foldLeft(byRace)((m, r) => m.updated(r, m.getOrElse(r, 0L) + 1L)))
  }

  /** Сколько расплат раса уже заслужила — по одной на каждые `threshold` убитых. */
  private def earned(race: String, threshold: Long): Int =
    (byRace.getOrElse(race, 0L) / threshold.max(1L)).toInt

  /** Кому герой задолжал прямо сейчас: расы, у которых заслуженных расплат
    * больше, чем случившихся. Порядок — от самой «злой» (больше всего долгов,
    * при равенстве — кого больше убито), чтобы очередь была предсказуемой. */
  def owed(threshold: Long): List[Race] =
    byRace.keys.toList
      .filter(r => earned(r, threshold) > avenged.getOrElse(r, 0))
      .flatMap(Race.withNameOption)
      .filter(Race.mortals.contains)
      .sortBy(r => (-(earned(r.entryName, threshold) - avengedTimes(r)), -count(r), r.entryName))

  /** Раса пришла за своим: долг закрыт, следующий — ещё через `threshold` убитых. */
  def markAvenged(race: Race): KillLog =
    copy(avenged = avenged.updated(race.entryName, avengedTimes(race) + 1))
}

object KillLog {
  val empty: KillLog = KillLog()

  implicit val encoder: Encoder[KillLog] = (k: KillLog) =>
    Json.obj("byRace" -> k.byRace.asJson, "avenged" -> k.avenged.asJson)

  /** Рукописный декодер: журнал живёт в JSONB, и новое поле не должно стирать
    * уже накопленный счёт (см. заметку о производных декодерах). */
  implicit val decoder: Decoder[KillLog] = (c: HCursor) =>
    for {
      byRace  <- c.getOrElse[Map[String, Long]]("byRace")(Map.empty)
      avenged <- c.getOrElse[Map[String, Int]]("avenged")(Map.empty)
    } yield KillLog(byRace, avenged)
}
