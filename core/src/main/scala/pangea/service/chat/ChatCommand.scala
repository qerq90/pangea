package pangea.service.chat

/** Команды, которые бот слушает в общей беседе Пангеи. Пока одна: «Передать»
 *  в ответ (или пересылкой) на сообщение того, кому передают. */
object ChatCommand {

  /** Слово, с которого начинается передача. */
  val Transfer: String = "передать"

  // `(?U)` обязателен: без него `\b` и `\w` кириллицу за буквы не считают, и
  // «2 штук» в конце строки не находится.
  private val CountByWord = raw"(?iU)(\d+)\s*шт[а-я.]*".r
  private val CountByX    = raw"(?iU)(?:^|\s)[xх]\s*(\d+)".r

  /** Текст после «Передать»: что именно хотят отдать. `None` — команда не наша.
    *
    * Пустой запрос («Передать» и всё) тоже годится: отправитель увидит у себя
    * в личке всю сумку и выберет вещь кнопкой. */
  def transferQuery(text: String): Option[String] = {
    val trimmed = text.trim
    Option.when(trimmed.toLowerCase.startsWith(Transfer))(trimmed.drop(Transfer.length).trim)
  }

  /** Сколько штук просят. Понимает «2 штуки», «2 шт», «х2»; по умолчанию одна. */
  def count(query: String): Int =
    CountByWord.findFirstMatchIn(query).orElse(CountByX.findFirstMatchIn(query))
      .flatMap(_.group(1).toIntOption).getOrElse(1).max(1)

  /** Название вещи: запрос без количества. */
  def itemQuery(query: String): String =
    CountByX.replaceAllIn(CountByWord.replaceAllIn(query, " "), " ")
      .replaceAll(raw"\s+", " ").trim

  /** Подходит ли вещь под запрос: сравниваем по «голому» названию, без эмодзи
    * редкости и без «[Ур.N]». Пустой запрос подходит всему. */
  def matches(query: String, name: String): Boolean = {
    val needle = normalize(query)
    needle.isEmpty || normalize(name).contains(needle)
  }

  private def normalize(s: String): String =
    s.toLowerCase
      .replaceAll(raw"\[ур\.\d+\]", " ")
      .replaceAll(raw"[^\p{L}\p{N} ]", " ")
      .replaceAll(raw"\s+", " ")
      .trim
}
