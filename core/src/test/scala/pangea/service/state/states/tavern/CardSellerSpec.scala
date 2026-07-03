package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.model.user.UserId
import pangea.test.TestHeroDao
import zio.ZIO
import zio.test._

/** Гейт появления продавца: «раз в час» + отсутствие ролла, пока он уже здесь. */
object CardSellerSpec extends ZIOSpecDefault {

  private val userId = UserId(7L)

  private def daoWith(data: Option[CardSellerData]) =
    for {
      dao <- TestHeroDao.make
      _   <- data.fold[ZIO[Any, Throwable, Unit]](ZIO.unit)(d => dao.writeCardSellerData(userId, d.asJson))
    } yield dao

  override def spec = suite("CardSeller")(

    test("продавец уже присутствует → ролл не трогает данные") {
      val present = CardSellerData(offerUntil = Some(3600000L), price = Some(100L), nextRollAt = Some(3600000L))
      for {
        dao <- daoWith(Some(present))
        out <- CardSeller.rollAndLoad(dao, userId, nowMs = 0L)
      } yield assertTrue(out == present)
    },

    test("почасовой гейт ещё не пройден → ролл не трогает данные") {
      val gated = CardSellerData(offerUntil = None, price = None, nextRollAt = Some(3600000L))
      for {
        dao <- daoWith(Some(gated))
        out <- CardSeller.rollAndLoad(dao, userId, nowMs = 0L)
      } yield assertTrue(out == gated)
    },

    test("гейт открыт → после ролла следующий не раньше чем через час") {
      for {
        dao <- daoWith(None)
        out <- CardSeller.rollAndLoad(dao, userId, nowMs = 1000L)
      } yield assertTrue(out.nextRollAt.contains(1000L + CardSellerData.RollIntervalMs))
    }
  )
}
