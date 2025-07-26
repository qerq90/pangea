package pangea.model.vk.model

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.generic.semiauto.deriveDecoder

case class UserResponse(
  response: List[User]
)

object UserResponse {
  implicit val decoder: Decoder[UserResponse] = deriveDecoder[UserResponse]
}

case class User(firstName: String, lastName: String)

object User {
  implicit val configuration: Configuration =
    Configuration.default.withSnakeCaseMemberNames

  implicit val decoder: Decoder[User] = deriveConfiguredDecoder
}
