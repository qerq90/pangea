package pangea.service.sender.vk

import org.http4s.Status
import zio.test._

object VkApiSpec extends ZIOSpecDefault {

  private val htmlBody = "<html><head><title>414 Request-URI Too Large</title></head></html>"

  def spec = suite("VkApi.failOnBadResponse")(
    test("2xx с обычным ответом ВК — успех") {
      VkApi.failOnBadResponse(Status.Ok, """{"response":1}""").as(assertCompletes)
    },
    test("2xx с полем error — VkApiError с кодом и текстом ВК") {
      VkApi
        .failOnBadResponse(Status.Ok, """{"error":{"error_code":914,"error_msg":"Message is too long"}}""")
        .flip
        .map(err => assertTrue(err == VkApi.VkApiError(914, "Message is too long")))
    },
    // Именно этот случай раньше считался успехом: сообщение не доставлено, а
    // герой переезжал в новое состояние без экрана (залипание в лавке Ришелье).
    test("414 от прокси — ошибка с кодом статуса") {
      VkApi
        .failOnBadResponse(Status.UriTooLong, htmlBody)
        .flip
        .map(err => assertTrue(err == VkApi.VkApiError(414, htmlBody)))
    },
    test("2xx с не-JSON телом — ошибка") {
      VkApi
        .failOnBadResponse(Status.Ok, htmlBody)
        .flip
        .map(err => assertTrue(err.getMessage.contains("non-JSON")))
    },
    test("длинное тело обрезается в тексте ошибки") {
      val long = "x" * 5000
      VkApi
        .failOnBadResponse(Status.UriTooLong, long)
        .flip
        .map(err => assertTrue(err.getMessage.length < 500, err.getMessage.endsWith("…")))
    }
  )
}
