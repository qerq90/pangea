package pangea.domain

case class Rng(seed: Long) {
  def nextLong: (Long, Rng) = {
    val next = seed * 6364136223846793005L + 1442695040888963407L
    (next, Rng(next))
  }

  // [min, max)
  def between(min: Long, max: Long): (Long, Rng) = {
    val (l, rng) = nextLong
    val range    = max - min
    val value    = ((l & Long.MaxValue) % range) + min
    (value, rng)
  }

  def nextBoolean: (Boolean, Rng) = {
    val (l, rng) = nextLong
    (l > 0, rng)
  }

  def pick[A](list: List[A]): (A, Rng) = {
    val (i, rng) = between(0L, list.length.toLong)
    (list(i.toInt), rng)
  }
}
