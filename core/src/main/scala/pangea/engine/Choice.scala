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
