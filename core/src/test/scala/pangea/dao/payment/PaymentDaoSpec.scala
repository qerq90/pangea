package pangea.dao.payment

import doobie.Read
import doobie.implicits._
import pangea.model.payment.Payment
import zio.test._

/** Выборки `payments` перечисляют колонки руками, а `Read[Payment]` читает их
  * по позиции — несовпадение компилятор не ловит, оно всплывает только на живой
  * базе. Этот тест и есть тот компилятор. */
object PaymentDaoSpec extends ZIOSpecDefault {

  override def spec = suite("PaymentDao")(
    test("колонок в выборке столько же, сколько полей читает Read[Payment]") {
      assertTrue(PaymentDaoLive.Columns.split(",").length == Read[Payment].length)
    }
  )
}
