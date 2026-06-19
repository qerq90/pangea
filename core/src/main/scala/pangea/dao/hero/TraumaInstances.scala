package pangea.dao.hero

import doobie.util.{Get, Put}

object TraumaInstances {
  implicit val getTraumaNames: Get[List[String]] =
    Get[String].map(s => if (s.isEmpty) Nil else s.split(",").toList)

  implicit val putTraumaNames: Put[List[String]] =
    Put[String].contramap(_.mkString(","))
}
