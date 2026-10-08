package pangea.dao.user

import doobie.Read
import doobie.implicits._
import pangea.model.user.User
import zio.test._

/** Выборки `users` перечисляют колонки руками, а `Read[User]` читает их по
  * позиции — несовпадение компилятор не ловит, оно всплывает только на живой
  * базе. Этот тест и есть тот компилятор. */
object UserDaoSpec extends ZIOSpecDefault {

  override def spec = suite("UserDao")(
    test("колонок в выборке столько же, сколько полей читает Read[User]") {
      assertTrue(UserDaoLive.Columns.split(",").length == Read[User].length)
    }
  )
}
