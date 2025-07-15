package pangea.service.state

import io.circe.Json

case class UserAction(text: String, payload: Json)
