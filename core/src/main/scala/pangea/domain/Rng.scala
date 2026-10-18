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
    // Младшие биты LCG ходят по короткому кругу (самый младший — через шаг), и
    // остаток от деления на чётное число повторял бы чётность шага: выбор из
    // восьми умений оружия, выпавшего на чётном шаге, брал бы только половину.
    // Перед делением биты перемешиваются — старшие доходят до младших.
    val value    = ((Rng.mix(l) & Long.MaxValue) % range) + min
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

object Rng {

  /** Перемешать биты (финал murmur3): каждый бит результата зависит от всех
    * битов входа. */
  private def mix(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 33)) * 0xff51afd7ed558ccdL
    val z2 = (z1 ^ (z1 >>> 33)) * 0xc4ceb9fe1a85ec53L
    z2 ^ (z2 >>> 33)
  }
}
