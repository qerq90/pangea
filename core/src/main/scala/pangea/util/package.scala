package pangea

import scala.util.Random

package object util {
  implicit class ListOps[T](l: List[T]) {
    private val r = new Random()

    def random: T = l(r.between(0, l.length))
  }
}
