package pangea.service.sender.vk

import pangea.engine.Choice
import zio.test._

/** Клавиатура ВК: не больше десяти рядов и пяти кнопок в ряду. Сообщение,
  * которое в это не уложилось, ВК отклоняет целиком — игрок вместо экрана
  * видит «Произошла ошибка», поэтому лишние ряды рендерер ужимает сам. */
object VkRendererSpec extends ZIOSpecDefault {

  private def rows(n: Int): List[List[Choice]] =
    (0 until n).toList.map(i => List(Choice(s"B$i", s"Кнопка $i", row = Some(i))))

  override def spec = suite("VkRenderer")(

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
    }
  )
}
