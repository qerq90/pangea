package pangea.dao.sendfailure

import doobie.util.transactor
import pangea.model.user.UserId
import zio.{Task, ZLayer}

/**
 * Журнал неудачных отправок сообщений. Пишется после исчерпания retry в
 * `VkApi.sendMessage` — чтобы потом разобраться, к кому и с каким payload'ом
 * не дошло.
 */
trait SendFailureDao {
  def record(
    userId:       Option[UserId],
    vkId:         String,
    messageText:  String,
    keyboardJson: Option[String],
    errorCode:    Option[Int],
    errorMessage: String,
    attempts:     Int,
    createdAt:    Long
  ): Task[Unit]
}

object SendFailureDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, SendFailureDaoLive] =
    ZLayer.fromFunction(new SendFailureDaoLive(_))
}
