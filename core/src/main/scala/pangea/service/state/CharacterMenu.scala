package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.Task

/**
 * «Персонаж» — модальный экран, открываемый из любой локации. Открытие запоминает
 * локацию-источник в `heroes.return_state`, чтобы `HeroStatsState` мог вернуть игрока
 * туда же. Единая точка перехода для всех хабов (см. [[StateType.HeroStats]]).
 */
object CharacterMenu {

  /** Открыть «Персонаж» из локации `from`, запомнив её для возврата. */
  def open(heroDao: HeroDao, userId: UserId, from: StateType): Task[StateType] =
    heroDao.writeReturnState(userId, Some(from)).as(StateType.HeroStats)
}
