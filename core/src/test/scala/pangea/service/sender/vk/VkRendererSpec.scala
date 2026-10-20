package pangea.service.sender.vk

import pangea.engine.{Choice, Screen}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.vk.{Attachment, AttachmentType}
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.UserResponse
import pangea.service.sender.Api
import zio.{Ref, Task, ZIO}
import zio.test._

/** Клавиатура ВК: не больше десяти рядов и пяти кнопок в ряду. Сообщение,
  * которое в это не уложилось, ВК отклоняет целиком — игрок вместо экрана
  * видит «Произошла ошибка», поэтому лишние ряды рендерер ужимает сам. */
object VkRendererSpec extends ZIOSpecDefault {

  private def rows(n: Int): List[List[Choice]] =
    (0 until n).toList.map(i => List(Choice(s"B$i", s"Кнопка $i", row = Some(i))))

  private val testUser = User(UserId(1L), VkId("vk"), TelegramId("tg"))

  /** Апи-заглушка: запоминает клавиатуру, которую рендерер собрал к отправке. */
  private class CapturingApi(sent: Ref[Option[Keyboard]]) extends Api {
    def getName(user: User): Task[UserResponse] = ZIO.fail(new Throwable("not needed"))
    def sendMessage(user: User, message: String, attachments: List[Attachment],
                    keyboard: Option[Keyboard]): Task[Unit] = sent.set(keyboard)
    def sendToChat(message: String): Task[Unit] = ZIO.unit
  }

  /** Апи-заглушка: запоминает вложения, с которыми ушло сообщение. */
  private class AttachmentsApi(sent: Ref[List[Attachment]]) extends Api {
    def getName(user: User): Task[UserResponse] = ZIO.fail(new Throwable("not needed"))
    def sendMessage(user: User, message: String, attachments: List[Attachment],
                    keyboard: Option[Keyboard]): Task[Unit] = sent.set(attachments)
    def sendToChat(message: String): Task[Unit] = ZIO.unit
  }

  override def spec = suite("VkRenderer")(

    test("картинка экрана уходит вложением: фото из альбома группы по ссылке, без картинки — без вложений") {
      for {
        sent <- Ref.make(List.empty[Attachment])
        api   = new AttachmentsApi(sent)
        _    <- new VkRenderer(api).show(testUser, Screen("текст", Nil, image = Some("photo-196380979_457239080")))
        with1 <- sent.get
        _    <- new VkRenderer(api).show(testUser, Screen("текст", Nil))
        none <- sent.get
      } yield assertTrue(with1 == List(Attachment(AttachmentType.Photo, "-196380979", "457239080"))) &&
              assertTrue(with1.map(_.toString) == List("photo-196380979_457239080")) &&
              assertTrue(none.isEmpty)
    },

    test("ссылка на фото разбирается и короткой, и полной, и из альбома; мусор — без вложения") {
      val photo = Some(Attachment(AttachmentType.Photo, "-196380979", "457239080"))
      assertTrue(Attachment.parse("photo-196380979_457239080") == photo) &&
      assertTrue(Attachment.parse("https://vk.ru/photo-196380979_457239080") == photo) &&
      assertTrue(Attachment.parse("https://vk.com/club196380979?z=photo-196380979_457239080%2Falbum-196380979_0") == photo) &&
      assertTrue(Attachment.parse("просто текст").isEmpty)
    },

    test("раскладка в пределах лимита не трогается") {
      val ten = rows(VkRenderer.MaxRows)
      assertTrue(VkRenderer.fit(ten) == ten) &&
      assertTrue(VkRenderer.fit(rows(3)) == rows(3))
    },

    test("лишние ряды перекладываются плотно, и кнопки не теряются") {
      val many   = rows(14)
      val fitted = VkRenderer.fit(many)
      assertTrue(fitted.size <= VkRenderer.MaxRows) &&
      assertTrue(fitted.forall(_.size <= VkRenderer.MaxButtonsPerRow)) &&
      assertTrue(fitted.flatten.map(_.id) == many.flatten.map(_.id)) &&
      // порядок кнопок сохраняется и на большой раскладке
      assertTrue(VkRenderer.fit(rows(40)).flatten.map(_.id) == rows(40).flatten.map(_.id))
    },

    test("столбик без рядов тоже ужимается: одиннадцать кнопок — уже отказ ВК") {
      // Сцена без `row` кладёт каждую кнопку в свой ряд — именно так
      // падало меню Ришелье, когда к нему прибавился уговор дня.
      val many = (0 until 12).toList.map(i => Choice(s"B$i", s"Кнопка $i"))
      for {
        sent <- Ref.make(Option.empty[Keyboard])
        _    <- new VkRenderer(new CapturingApi(sent)).show(testUser, Screen("текст", many))
        kb   <- sent.get
      } yield assertTrue(kb.exists(_.buttons.size <= VkRenderer.MaxRows)) &&
              assertTrue(kb.exists(_.buttons.forall(_.size <= VkRenderer.MaxButtonsPerRow))) &&
              // ни одной кнопки не потерялось
              assertTrue(kb.exists(_.buttons.map(_.size).sum == many.size))
    }
  )
}
