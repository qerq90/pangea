package pangea.service.referral

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.dao.referral.ReferralDao
import pangea.engine.{Players, SceneContent}
import pangea.model.hero.Hero
import pangea.model.referral.FriendCode
import pangea.model.user.{User, UserId}
import pangea.repository.user.UserRepository
import pangea.service.payout.Payouts
import pangea.service.state.AzatData
import zio.{Task, ZIO, ZLayer}

/** Код друга. Код видно в «Персонаже»; ввести чужой можно, пока герой не выше
  * пятого уровня, и только один раз. Ввёл — Азат благословляет его на три дня, а
  * владельцу кода Рахадим передаёт дублоны: часть сразу, часть — когда друг
  * дорастёт до десятого уровня.
  *
  * Дублоны владельцу идут отложенной выплатой (см. [[Payouts]]): его героя здесь
  * не трогаем — его пишут только под его собственным локом. Дойдут они с его
  * ближайшим действием, где бы он ни был, а письмо уходит сразу. */
final case class Referrals(
  dao:      ReferralDao,
  heroDao:  HeroDao,
  userRepo: UserRepository,
  payouts:  Payouts,
  players:  Players,
  content:  SceneContent
) {
  import Referrals._

  def codeOf(user: User): String = FriendCode.of(user.userId)

  /** Можно ли сейчас ввести чужой код: уровень не выше пятого и код ещё не
    * вводили. */
  def canEnter(user: User, hero: Hero): Task[Boolean] =
    if (hero.lvl > MaxLevel) ZIO.succeed(false)
    else dao.referrerOf(user.userId).map(_.isEmpty)

  /** Ввести чужой код. `Left` — отказ (что сказать игроку), `Right` — принят. */
  def enter(user: User, hero: Hero, text: String, now: Long): Task[Either[String, String]] =
    FriendCode.parse(text) match {
      case None                                   => refuse("friendCode.notACode")
      case Some(_) if hero.lvl > MaxLevel          => refuse("friendCode.tooHigh", "level" -> MaxLevel.toString)
      case Some(owner) if owner == user.userId     => refuse("friendCode.own")
      case Some(owner) =>
        dao.referrerOf(user.userId).flatMap {
          case Some(_) => refuse("friendCode.already")
          case None =>
            ownerOf(owner).flatMap {
              case None => refuse("friendCode.unknown")
              case Some((ownerUser, ownerHero)) =>
                // Обменяться кодами нельзя: кто пришёл по твоему коду, тот твой
                // код уже не получит обратно.
                dao.referrerOf(owner).flatMap {
                  case Some(back) if back == user.userId => refuse("friendCode.mutual")
                  case _ =>
                    dao.add(user.userId, owner, now).flatMap {
                      case false => refuse("friendCode.already")
                      case true  => accept(user, ownerUser, ownerHero, now)
                    }
                }
            }
        }
    }

  private def accept(user: User, owner: User, ownerHero: Hero, now: Long): Task[Either[String, String]] =
    for {
      azat <- AzatData.load(heroDao, user.userId, now)
      _    <- heroDao.writeAzatData(user.userId, azat.blessedFor(BlessingMs, now).asJson)
      _    <- payouts.queue(ownerHero.id, doubloons = OwnerNow)
      name <- nameOf(user)
      _    <- players.notify(owner, content.format("friendCode.ownerNotice",
                "name" -> name, "now" -> OwnerNow.toString,
                "level" -> BonusLevel.toString, "later" -> LevelBonus.toString)).ignore
    } yield Right(content.format("friendCode.accepted", "days" -> BlessingDays.toString))

  /** Друг дорос до десятого уровня — владельцу кода ещё дублоны. Зовётся в
    * начале каждого действия героя от десятого уровня: отметка «заплачено»
    * ставится в том же запросе, что и проверка, так что платят ровно раз. */
  def payLevelBonus(user: User, hero: Hero): Task[Unit] =
    ZIO.when(hero.lvl >= BonusLevel)(dao.claimBonus(user.userId).flatMap {
      case None => ZIO.unit
      case Some(owner) =>
        ownerOf(owner).flatMap {
          // Владельца кода больше нет — платить некому.
          case None => ZIO.unit
          case Some((ownerUser, ownerHero)) =>
            payouts.queue(ownerHero.id, doubloons = LevelBonus) *>
              nameOf(user).flatMap(name => players.notify(ownerUser, content.format("friendCode.levelNotice",
                "name" -> name, "level" -> BonusLevel.toString, "doubloons" -> LevelBonus.toString)).ignore)
        }
    }).unit

  private def ownerOf(owner: UserId): Task[Option[(User, Hero)]] =
    for {
      user <- userRepo.getUserById(owner).option.map(_.flatten)
      hero <- heroDao.getHeroByUserId(owner)
    } yield for { u <- user; h <- hero } yield (u, h)

  private def nameOf(user: User): Task[String] =
    players.getDisplayName(user).orElse(ZIO.succeed(content.text("friendCode.someone")))

  private def refuse(key: String, args: (String, String)*): Task[Either[String, String]] =
    ZIO.succeed(Left(content.format(key, args: _*)))
}

object Referrals {

  /** До какого уровня включительно можно ввести чужой код. */
  val MaxLevel: Long = 5L

  /** Благословение Азата за введённый код. */
  val BlessingDays: Long = 3L
  val BlessingMs: Long   = BlessingDays * 24L * 60L * 60L * 1000L

  /** Сколько дублонов владельцу кода сразу и сколько — когда друг дорастёт до
    * [[BonusLevel]]. */
  val OwnerNow: Long   = 3L
  val BonusLevel: Long = 10L
  val LevelBonus: Long = 5L

  val live: ZLayer[ReferralDao with HeroDao with UserRepository with Payouts with Players with SceneContent, Nothing, Referrals] =
    ZLayer.fromFunction(Referrals(_, _, _, _, _, _))
}
