package pangea.model.trauma

/** Выбор следующей травмы. Логика общая для смерти и для других источников
 *  травм (например, смерч огненного элементаля): травмы копятся по тирам —
 *  сперва добираются все лёгкие, потом средние, потом тяжёлые. Уже полученные
 *  из пула исключаются, поэтому одна и та же травма не выпадает дважды. */
object TraumaRoll {

  /** Пул, из которого выбирается следующая травма при уже имеющихся `existing`.
   *  Пустой — значит собраны вообще все травмы (максимум). */
  def pool(existing: List[String]): Seq[Trauma] = {
    val hasAllLight  = Trauma.light.forall(t => existing.contains(t.name))
    val hasAllMedium = Trauma.medium.forall(t => existing.contains(t.name))
    if (!hasAllLight) Trauma.light.filterNot(t => existing.contains(t.name))
    else if (!hasAllMedium) Trauma.medium.filterNot(t => existing.contains(t.name))
    else Trauma.heavy.filterNot(t => existing.contains(t.name))
  }

  /** Сколько держится травма — 8 часов с момента получения. */
  val DurationMs: Long = 8L * 3600L * 1000L
}
