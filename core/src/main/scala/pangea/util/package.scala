package pangea

import pangea.domain.Rng

package object util {
  implicit class ListOps[T](l: List[T]) {
    def pickWith(rng: Rng): (T, Rng) = rng.pick(l)
  }
}
