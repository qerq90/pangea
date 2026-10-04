package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.model.quest.{BoardData, BoardKind}
import pangea.model.user.UserId
import zio.Task

/** Отметки доски заданий, которые ставятся не у доски, а там, где дело и
  * делается: караван считается разгромленным у разбитого обоза, пещера —
  * зачищенной в самой пещере. Награду герой забирает сам, вернувшись в гильдию.
  *
  * Доска живёт в `heroes.quest_data`. Если неделя сменилась, отметка ложится в
  * уже просроченную доску и сгорит вместе с ней при первом же заходе в
  * гильдию — так и задумано: неделя и есть срок.
  */
object BoardProgress {

  def load(heroDao: HeroDao, userId: UserId): Task[BoardData] =
    heroDao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).getOrElse(BoardData.empty))

  /** Взято ли сейчас такое задание и не закрыто ли уже. */
  def hunting(heroDao: HeroDao, userId: UserId, kind: BoardKind): Task[Boolean] =
    load(heroDao, userId).map(_.hunting(kind))

  /** Закрыть задание такого вида. Возвращает, было ли что закрывать. */
  def markDone(heroDao: HeroDao, userId: UserId, kind: BoardKind): Task[Boolean] =
    load(heroDao, userId).flatMap { data =>
      val next = data.markDone(kind)
      if (next == data) zio.ZIO.succeed(false)
      else heroDao.writeQuestData(userId, next.asJson).as(true)
    }
}
