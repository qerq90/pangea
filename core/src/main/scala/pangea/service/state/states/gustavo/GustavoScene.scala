package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails}
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

  /** Заряды предмета (фляга/пояс), если это заряжаемый предмет. */
  protected def charged(item: Item): Option[ItemDetails.Charged] = item.details match {
    case c: ItemDetails.Charged => Some(c)
    case _                      => None
  }

  /** Цена пополнения фляги: 25 золота за каждый недостающий глоток. */
  protected def flaskRefillCost(hero: Hero): Long =
    charged(hero.equipment.flask)
      .map(c => (c.maxCharges - c.charges).toLong * GustavoData.FlaskRefillCostPerCharge)
      .getOrElse(0L)

  /** Цена пополнения пояса: 100 золота за каждую недостающую бутыль. */
  protected def beltRefillCost(hero: Hero): Long =
    charged(hero.equipment.belt)
      .map(c => (c.maxCharges - c.charges).toLong * GustavoData.BeltRefillCostPerBottle)
      .getOrElse(0L)

  /** Оставшиеся миллисекунды → строка с минутами, округление вверх, минимум 1. */
  protected def minsOf(ms: Long): String = ((ms + 59999L) / 60000L).max(1L).toString
}
