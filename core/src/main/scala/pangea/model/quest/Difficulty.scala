package pangea.model.quest

/** Сложность задания — римское число своими знаками: кость 1, череп 5, гроб 10,
  * кладбище 50, тьма 100. Правила римские, с вычитанием: 4 = 🦴💀, 9 = 🦴⚰️,
  * 40 = ⚰️🪦. */
object Difficulty {
  val Bone: String     = "🦴" // 1
  val Skull: String    = "💀" // 5
  val Coffin: String   = "⚰️" // 10
  val Cemetery: String = "🪦" // 50
  val Darkness: String = "🌑" // 100 — знак предложен, ждёт утверждения

  private val Digits: List[(Int, String)] = List(
    100 -> Darkness,
    90  -> (Coffin + Darkness),
    50  -> Cemetery,
    40  -> (Coffin + Cemetery),
    10  -> Coffin,
    9   -> (Bone + Coffin),
    5   -> Skull,
    4   -> (Bone + Skull),
    1   -> Bone
  )

  /** «Сложность» числом от 1 до 399 в знаках; ниже единицы — одна кость. */
  def render(n: Int): String = {
    @annotation.tailrec
    def go(left: Int, digits: List[(Int, String)], acc: StringBuilder): String =
      digits match {
        case Nil                              => acc.toString
        case (v, s) :: _ if left >= v         => go(left - v, digits, acc.append(s))
        case _ :: rest                        => go(left, rest, acc)
      }
    go(n.max(1), Digits, new StringBuilder)
  }
}
