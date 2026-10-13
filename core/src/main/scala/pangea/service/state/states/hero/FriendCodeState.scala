package pangea.service.state.states.hero

import java.util.concurrent.TimeUnit
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.referral.Referrals
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/** «Код друга» из «Персонажа»: свой код, чтобы отдать его другу, и — пока герой
  * не выше пятого уровня и код ещё не вводили — место, куда вписать чужой.
  * Чужой код пишут обычным сообщением. */
case class FriendCodeState(
  heroDao:   HeroDao,
  referrals: Referrals,
  content:   SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Back" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (user, ua, renderer) => typed(user, ua, renderer) }
  )

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      can  <- referrals.canEnter(user, hero)
      _    <- renderer.show(user, screen(user, can))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def screen(user: User, canEnter: Boolean): Screen = {
    val card = content.format("friendCode.card",
      "code"   -> referrals.codeOf(user),
      "level"  -> Referrals.MaxLevel.toString,
      "days"   -> Referrals.BlessingDays.toString,
      "now"    -> Referrals.OwnerNow.toString,
      "bonusLevel" -> Referrals.BonusLevel.toString,
      "later"  -> Referrals.LevelBonus.toString)
    val hint = if (canEnter) "\n\n" + content.text("friendCode.enterHint") else ""
    Screen(card + hint, List(content.choice("Back", "friendCode.back")))
  }

  /** Написанное сообщение — попытка ввести чужой код. */
  private def typed(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    if (ua.text.trim.isEmpty) enter(user, renderer).as(StateType.FriendCode)
    else
      for {
        now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        hero <- getHero(user)
        said <- referrals.enter(user, hero, ua.text, now)
        _    <- renderer.show(user, Screen(said.merge, Nil))
        _    <- enter(user, renderer)
      } yield StateType.FriendCode

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
}
