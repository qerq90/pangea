package pangea.model.referral

import pangea.model.user.UserId

/** Код друга: восемь цифр вида «4821-3907». Цифры — чтобы раскладка клавиатуры
  * не мешала: русская «С» и латинская «C» выглядят одинаково, а цифры — одни на
  * всех.
  *
  * Код не хранится — он считается из номера аккаунта и обратно, перемешанный
  * обратимым умножением: соседние аккаунты получают непохожие коды. Восьми
  * цифр хватит на сто миллионов аккаунтов. */
object FriendCode {

  private val Modulus: Long = 100000000L
  // Взаимно просто с 10^8 (нечётное и не кратно пяти) — иначе перемешивание не
  // обратить.
  private val Mult: Long    = 37139213L
  private val Shift: Long   = 52837461L
  private val Inverse: Long = BigInt(Mult).modInverse(BigInt(Modulus)).toLong

  /** Код аккаунта для показа: «4821-3907». */
  def of(userId: UserId): String = {
    val digits = f"${Math.floorMod(userId.value * Mult + Shift, Modulus)}%08d"
    s"${digits.take(4)}-${digits.drop(4)}"
  }

  /** Чей это код. Пробелы, дефисы и прочее между цифрами не мешают; `None` —
    * цифр не восемь. */
  def parse(text: String): Option[UserId] = {
    val digits = text.filter(_.isDigit)
    Option.when(digits.length == 8)(digits).flatMap(_.toLongOption).map(n =>
      UserId(Math.floorMod(Math.floorMod(n - Shift, Modulus) * Inverse, Modulus)))
  }
}
