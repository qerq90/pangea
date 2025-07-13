package pangea.model.vk.keyboard

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps

case class Keyboard(buttons: List[Button], one_time: Boolean, inline: Boolean) {
  def getJson: String = this.asJson.toString
}

object Keyboard {
  implicit val encoder: Encoder[Keyboard] = deriveEncoder[Keyboard]
}
