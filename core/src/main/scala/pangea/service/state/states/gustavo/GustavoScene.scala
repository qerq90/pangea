package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.model.hero.Hero
import pangea.model.user.User
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Общие для стейтов Густаво (`GustavoState`/`GustavoHealState`/`GustavoBoostState`)
 *  зависимости и хелперы: доступ к герою и durable-данным Густаво, цена зелья, форматирование
 *  оставшегося времени. Конкретные стейты подмешивают трейт и предоставляют `heroDao`. */
trait GustavoScene {

  protected def heroDao: HeroDao

  protected def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  protected def loadData(user: User): Task[GustavoData] =
    heroDao.readGustavoData(user.userId).map(_.flatMap(_.as[GustavoData].toOption).getOrElse(GustavoData.empty))

  protected def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  /** Цена зелья: 100 × уровень героя. */
  protected def cost(hero: Hero): Long = hero.lvl * GustavoData.CostPerLevel

  /** Цена пополнения припасов (фляга/пояс): 25 × уровень героя. */
  protected def supplyCost(hero: Hero): Long = hero.lvl * GustavoData.SupplyCostPerLevel

  /** Оставшиеся миллисекунды → строка с минутами, округление вверх, минимум 1. */
  protected def minsOf(ms: Long): String = ((ms + 59999L) / 60000L).max(1L).toString
}
