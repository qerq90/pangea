package pangea.model.user

/** Почта для кассового чека. Игрок присылает её свободным сообщением, поэтому
  * адрес приходится разбирать самим: лишние пробелы снять, регистр привести к
  * нижнему, а что не похоже на адрес — не брать вовсе.
  *
  * Проверка нарочно нестрогая по сравнению с полным стандартом: наша задача —
  * отсеять случайный текст («ага», «сколько стоит?»), а не спорить с почтовым
  * сервером о допустимых знаках. */
object ReceiptEmail {

  /** Длиннее этого адресов не бывает: 64 на имя, 255 на домен, плюс собачка. */
  val MaxLength: Int = 254

  /** Разобрать присланное сообщение как адрес. None — это не адрес, и менять
    * записанное не нужно. */
  def parse(text: String): Option[String] = {
    val value = text.trim.toLowerCase
    Option.when(looksLikeEmail(value))(value)
  }

  private def looksLikeEmail(value: String): Boolean =
    value.nonEmpty && value.length <= MaxLength &&
      !value.exists(_.isWhitespace) &&
      value.count(_ == '@') == 1 && {
        val Array(name, domain) = value.split("@", 2)
        name.nonEmpty && domain.length >= 3 &&
          // в домене есть точка, и она не по краям: «mail.ru», а не «.ru» и не «mail.»
          domain.indexOf('.') > 0 && domain.indexOf('.') < domain.length - 1 &&
          !domain.contains("..") && !domain.endsWith(".")
      }
}
