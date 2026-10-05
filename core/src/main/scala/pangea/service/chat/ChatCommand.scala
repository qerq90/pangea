package pangea.service.chat

/** Команды, которые бот слушает в общей беседе Пангеи: «Передать» в ответ
 *  (или пересылкой) на сообщение того, кому передают, и две команды о себе —
 *  «Мой профиль» и «Моё снаряжение», ответ на которые уходит туда же, в
 *  беседу: о себе рассказывают при всех. */
object ChatCommand {

  /** Слово, с которого начинается передача. */
  val Transfer: String = "передать"

  /** Команда о себе: ответ бот пишет в саму беседу. */
  sealed trait Self
  object Self {
    /** Всё то же, что игрок видит по кнопке «Персонаж». */
    case object Profile extends Self

    /** Слоты снаряжения: что надето, без характеристик. */
    case object Gear extends Self
  }

  /** Чем отличать сообщения беседы друг от друга в защите от повторов.
    *
    * Брать `id` нельзя: в беседе ВК присылает боту нулевой `id`, а настоящий
    * номер кладёт в `conversation_message_id`. С нулями подряд идущие команды
    * выглядят для защиты одним и тем же сообщением, и вторая молча пропадает —
    * пока игрок не нажмёт что-нибудь в личке и не собьёт счётчик. Со стороны
    * это выглядит так, будто команда работает через раз и зависит от места.
    *
    * Номер беседы возвращается отрицательным: личные сообщения считаются
    * положительными, и два потока не должны мешать друг другу. */
  def eventKey(id: Long, conversationMessageId: Option[Long]): Long =
    -conversationMessageId.filter(_ > 0L).getOrElse(id)

  /** Распознать команду о себе. Сравниваем по «голой» строке: регистр, лишние
    * пробелы и знаки не важны, «ё» и «е» — одно и то же, потому что набирают
    * и так и этак. */
  def selfCommand(text: String): Option[Self] = bare(text) match {
    case "мой профиль"    => Some(Self.Profile)
    case "мое снаряжение" => Some(Self.Gear)
    case _                => None
  }

  private def bare(s: String): String =
    s.toLowerCase.replace('ё', 'е')
      .replaceAll(raw"[^\p{L}\p{N} ]", " ")
      .replaceAll(raw"\s+", " ")
      .trim

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
    * редкости и без «[Ур.N]». Регистр не важен, уровень в запросе не нужен.
    * Пустой запрос подходит всему. */
  def matches(query: String, name: String): Boolean = {
    val needle = normalize(query)
    needle.isEmpty || normalize(name).contains(needle)
  }

  /** Названо ли ровно это: «надколотый Череп» — тот самый «Надколотый череп». */
  def sameName(query: String, name: String): Boolean = {
    val needle = normalize(query)
    needle.nonEmpty && needle == normalize(name)
  }

  private def normalize(s: String): String =
    s.toLowerCase
      .replaceAll(raw"\[ур\.\d+\]", " ")
      .replaceAll(raw"[^\p{L}\p{N} ]", " ")
      .replaceAll(raw"\s+", " ")
      .trim
}
