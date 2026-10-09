package pangea.service.admin

import zio.test._

/** Этим паролем закрыта не только панель, но и выдача логов наружу
  * (`GET /logs`), где лежат peer_id игроков, тексты сообщений и номера
  * заказов. Поэтому правило допуска проверяем отдельно. */
object AdminConfigSpec extends ZIOSpecDefault {

  override def spec = suite("Пароль админки")(

    test("пароль не задан — не пускаем никого, даже с пустой строкой") {
      val empty = AdminConfig(None)
      assertTrue(!empty.grants(None)) &&
      assertTrue(!empty.grants(Some(""))) &&
      assertTrue(!empty.grants(Some("что угодно")))
    },

    test("верный пароль пускает, неверный и отсутствующий — нет") {
      val config = AdminConfig(Some("s3cret"))
      assertTrue(config.grants(Some("s3cret"))) &&
      assertTrue(!config.grants(Some("s3cre"))) &&
      assertTrue(!config.grants(Some("s3cret "))) &&
      assertTrue(!config.grants(Some("S3CRET"))) &&
      assertTrue(!config.grants(None))
    }
  )
}
