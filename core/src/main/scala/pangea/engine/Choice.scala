package pangea.engine

/** `row` — индекс ряда кнопок в клавиатуре. Кнопки с одинаковым `row` оказываются
 *  в одной строке клавиатуры; ряды отсортированы по возрастанию `row`. Сами кнопки
 *  внутри ряда — в порядке появления в списке. По умолчанию у всех кнопок `row = 0`,
 *  и каждая кнопка идёт в новой строке (см. VkRenderer для пояснений). */
case class Choice(
  id:    String,
  label: String,
  data:  Map[String, String] = Map.empty,
  color: ChoiceColor          = ChoiceColor.Primary,
  row:   Option[Int]          = None
)

object Choice {
  /** Самая длинная подпись, которую принимает клавиатура ВК; длиннее — ошибка
    * отправки и «зависший» экран у игрока. */
  val MaxLabelLength: Int = 40

  /** Подпись, укладывающаяся в лимит: лишнее срезается с многоточием. */
  def fit(label: String): String =
    if (label.length <= MaxLabelLength) label else label.take(MaxLabelLength - 1) + "…"
}
