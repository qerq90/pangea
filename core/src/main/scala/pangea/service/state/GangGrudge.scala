package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.model.monster.Race
import pangea.model.user.UserId
import zio.Task

/** Обида городской банды. Герой разогнал воров по объявлению с доски — шайка
  * этого не забыла и ждёт своего часа: через несколько вылазок в лабиринт его
  * найдёт их именной.
  *
  * Счёт живёт в журнале убитых (`heroes.kill_log`): там же, где и прочие счёты
  * рас с героем, и по тем же правилам — одна обида за раз, пока не сведётся.
  */
object GangGrudge {

  /** Через сколько осмотров лабиринта банда выходит на героя. */
  val MinExplores: Int = 5
  val MaxExplores: Int = 10

  /** Запомнить обиду: эта раса придёт через `after` осмотров. */
  def remember(heroDao: HeroDao, userId: UserId, race: Race, after: Int): Task[Unit] =
    KillLogData.read(heroDao, userId).flatMap(log =>
      KillLogData.write(heroDao, userId, log.grudge(race, after)))
}
