package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.model.user.UserId
import zio.{Random, Task, ZIO}

/** Логика появления продавца карт в таверне. «Раз в час фоновым роллом»
 *  реализовано лениво: при заходе в таверну катаем 25%, но не чаще, чем раз в час
 *  (гейт [[CardSellerData.nextRollAt]]) — повторные заходы шанс не накручивают.
 *  После покупки тот же гейт держит кулдаун 20 часов. */
object CardSeller {

  /** Текущие данные из `card_seller_data` (пусто, если колонка не заполнена). */
  def load(heroDao: HeroDao, userId: UserId): Task[CardSellerData] =
    heroDao.readCardSellerData(userId)
      .map(_.flatMap(_.as[CardSellerData].toOption).getOrElse(CardSellerData.empty))

  /** Прокатывает появление, если продавца сейчас нет и почасовой гейт пройден;
   *  возвращает актуальные данные (после возможного ролла). */
  def rollAndLoad(heroDao: HeroDao, userId: UserId, nowMs: Long): Task[CardSellerData] =
    load(heroDao, userId).flatMap { data =>
      val gated = data.present(nowMs) || data.nextRollAt.exists(_ > nowMs)
      if (gated) ZIO.succeed(data)
      else
        Random.nextLong.flatMap { seed =>
          val updated = rolled(nowMs, Rng(seed))
          heroDao.writeCardSellerData(userId, updated.asJson).as(updated)
        }
    }

  // Один почасовой ролл: 25% — продавец приходит на час с фиксированной ценой;
  // иначе пусто. В обоих случаях следующий ролл — не раньше чем через час.
  private def rolled(nowMs: Long, rng: Rng): CardSellerData = {
    val (roll, r1) = rng.between(0L, 100L)
    val nextRoll   = Some(nowMs + CardSellerData.RollIntervalMs)
    if (roll < CardSellerData.RollChancePercent) {
      val (price, _) = r1.between(CardSellerData.PriceMin, CardSellerData.PriceMax + 1L)
      CardSellerData(offerUntil = Some(nowMs + CardSellerData.PresenceMs), price = Some(price), nextRollAt = nextRoll)
    } else CardSellerData(offerUntil = None, price = None, nextRollAt = nextRoll)
  }
}
