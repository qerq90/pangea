package pangea.dao.sendfailure

import doobie.implicits.toSqlInterpolator
import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.user.UserId
import zio.Task
import zio.interop.catz._

class SendFailureDaoLive(xa: Transactor[Task]) extends SendFailureDao {
  override def record(
    userId:       Option[UserId],
    vkId:         String,
    messageText:  String,
    keyboardJson: Option[String],
    errorCode:    Option[Int],
    errorMessage: String,
    attempts:     Int,
    createdAt:    Long
  ): Task[Unit] =
    sql"""insert into send_failures(user_id, vk_id, message_text, keyboard_json, error_code, error_message, attempts, created_at)
          values(${userId.map(_.value)}, $vkId, $messageText, $keyboardJson, $errorCode, $errorMessage, $attempts, $createdAt)"""
      .update.run.transact(xa).unit
}
