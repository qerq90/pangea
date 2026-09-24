package pangea.service.parcel

import pangea.dao.parcel.ParcelDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.Item
import pangea.model.parcel.Parcel
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import zio.{Task, ZIO, ZLayer}

/** Передача вещей между игроками. Отправитель отдаёт вещь из города, а
 *  получателю она приходит посылкой: как только он что-нибудь сделает в игре,
 *  посылка сама ложится в его банковскую ячейку. Не приняла ячейка (её нет или
 *  нет места) — вещь ждёт на почте в Торговом доме.
 *
 *  Посылками это сделано не для красоты: чужую сумку нельзя трогать в тот
 *  момент, когда её хозяин сам в ней роется, — запись бы затёрлась. Здесь всё,
 *  что попадает к получателю, кладётся под его собственным локом. */
final case class Parcels(dao: ParcelDao, bank: BankRepository, content: SceneContent) {

  /** Сколько мест можно разобрать за один заход: экран не резиновый. */
  val DeliveryBatch: Long = 10L

  def send(toHeroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel] =
    dao.add(toHeroId, fromName, item, now)

  def waiting(heroId: HeroId, limit: Long = DeliveryBatch): Task[List[Parcel]] =
    dao.list(heroId, limit)

  def waitingCount(heroId: HeroId): Task[Long] = dao.count(heroId)

  /** Забрать посылку себе: `false` — её уже забрали. */
  def claim(heroId: HeroId, parcel: Parcel): Task[Boolean] = dao.claim(parcel.id, heroId)

  /** Разложить посылки по ячейке героя и сказать ему об этом. Что не влезло,
    * остаётся на почте. */
  def deliver(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    dao.list(hero.id, DeliveryBatch).flatMap { parcels =>
      ZIO.when(parcels.nonEmpty) {
        for {
          delivered <- ZIO.foreach(parcels)(toVault(hero.id, _)).map(_.flatten)
          left      <- dao.count(hero.id)
          _ <- ZIO.when(delivered.nonEmpty) {
                 val names = delivered.map(p => s"${p.item.displayTitle} (${p.fromName})").mkString(", ")
                 val tail  = if (left > 0L) content.format("parcel.leftAtMail", "count" -> left.toString) else ""
                 renderer.show(user, Screen(content.format("parcel.delivered", "items" -> names) + tail, Nil))
               }
        } yield ()
      }.unit
    }

  /** Одна посылка в ячейку: кладём, а потом забираем её из списка. Если забрать
    * не вышло (кто-то успел раньше), вещь достаём обратно — иначе она удвоится.
    * Ячейка не приняла — посылка просто остаётся на почте. */
  private def toVault(heroId: HeroId, parcel: Parcel): Task[Option[Parcel]] =
    bank.deposit(heroId, parcel.item).either.flatMap {
      case Left(_) => ZIO.none
      case Right(_) =>
        dao.claim(parcel.id, heroId).flatMap {
          case true  => ZIO.some(parcel)
          case false => bank.withdraw(heroId, parcel.item.id).ignore.as(None)
        }
    }
}

object Parcels {
  val live: ZLayer[ParcelDao with BankRepository with SceneContent, Nothing, Parcels] =
    ZLayer.fromFunction(Parcels(_, _, _))
}
