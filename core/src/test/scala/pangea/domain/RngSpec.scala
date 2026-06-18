package pangea.domain

import zio.test._

object RngSpec extends ZIOSpecDefault {
  def spec = suite("RngSpec")(
    test("same seed produces same sequence") {
      val (a1, _) = Rng(42L).between(0L, 100L)
      val (a2, _) = Rng(42L).between(0L, 100L)
      assertTrue(a1 == a2)
    },
    test("between returns value in [min, max)") {
      val values = (0L to 50L).map(seed => Rng(seed).between(5L, 15L)._1)
      assertTrue(values.forall(v => v >= 5L && v < 15L))
    },
    test("pick selects an element from the list") {
      val list   = List("a", "b", "c")
      val values = (0L to 30L).map(seed => Rng(seed).pick(list)._1)
      assertTrue(values.forall(v => list.contains(v)))
    },
    test("rng advances on each call") {
      val rng         = Rng(42L)
      val (_, rng2)   = rng.nextLong
      assertTrue(rng2.seed != rng.seed)
    }
  )
}
