package pangea.service.payout

import pangea.dao.hero.HeroDao
import pangea.dao.payout.PayoutDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.user.User
import zio.{Task, ZIO, ZLayer}

/** Отложенная выручка. Деньги за проданный лот кладутся в банковскую ячейку, а
 *  если она не приняла — ждут здесь и попадают герою на руки в первой же
 *  городской сцене. Так проданный товар не сгорает вместе с половиной серебра,
 *  если герой сразу после продажи погибнет в лабиринте. */
final case class Payouts(dao: PayoutDao, heroDao: HeroDao, content: SceneContent) {

  def queue(heroId: HeroId, silver: Long = 0L, doubloons: Long = 0L): Task[Unit] =
    ZIO.when(silver > 0L || doubloons > 0L)(dao.add(heroId, silver, doubloons)).unit

  /** Выдать всё, что накопилось, и сказать об этом. Вызывается в городе. */
  def deliver(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    dao.takeAll(hero.id).flatMap { case (silver, doubloons) =>
      ZIO.when(silver > 0L || doubloons > 0L)(
        ZIO.when(silver > 0L)(heroDao.updateSilver(user.userId, hero.silver + silver)) *>
          ZIO.when(doubloons > 0L)(heroDao.updateDoubloons(user.userId, hero.doubloons + doubloons)) *>
          renderer.show(user, Screen(content.format("bank.auction.payoutDelivered",
            "money" -> moneyLine(silver, doubloons)), Nil))
      ).unit
    }

  private def moneyLine(silver: Long, doubloons: Long): String =
    List(Option.when(silver > 0L)(s"🪙 $silver"), Option.when(doubloons > 0L)(s"🟡 $doubloons"))
      .flatten.mkString(" и ")
}

object Payouts {
  val live: ZLayer[PayoutDao with HeroDao with SceneContent, Nothing, Payouts] =
    ZLayer.fromFunction(Payouts(_, _, _))
}
