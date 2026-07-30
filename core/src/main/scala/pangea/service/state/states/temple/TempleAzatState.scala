package pangea.service.state.states.temple

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{AzatState, Hero}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Храм Азата: вход, Жрец (лор + благословение) и переход в Зал Азата. Благословение
 *  — пожертвование 250 дублонов даёт недельный баф ([[AzatState.blessingUntil]]) и
 *  250 мгновенных отдыхов. */
case class TempleAzatState(heroDao: HeroDao, content: SceneContent) extends State {
  import TempleAzatState._

  private val branch = new Branch(
    routes = Map(
      "Priest"       -> Target.Run { (u, _, r) => showPriest(u, r) },
      "Hall"         -> Target.Goto(StateType.HallAzat),
      "LeaveTemple"  -> Target.Goto(StateType.CityCenter),
      "WhoIsAzat"    -> Target.Run { (u, _, r) => showWho(u, r) },
      "AskBlessing"  -> Target.Run { (u, _, r) => showBlessing(u, r) },
      "Donate"       -> Target.Run { (u, _, r) => donate(u, r) },
      "BackToPriest" -> Target.Run { (u, _, r) => showPriest(u, r) },
      "BackToTemple" -> Target.Run { (u, _, r) => enter(u, r).as(StateType.TempleAzat) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.TempleAzat) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = {
    val byId = content.screen("temple.enter").choices.map(c => c.id -> c).toMap
    val choices = List(
      byId("Priest").copy(row = Some(0)),
      byId("Hall").copy(color = ChoiceColor.Positive, row = Some(0)),
      byId("LeaveTemple").copy(color = ChoiceColor.Negative, row = Some(1))
    )
    renderer.show(user, Screen(content.text("temple.enter.text"), choices))
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showPriest(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, content.screen("temple.priest")).as(StateType.TempleAzat)

  private def showWho(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("temple.whoIsAzat"),
      List(content.choice("BackToPriest", "temple.back")))).as(StateType.TempleAzat)

  private def showBlessing(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      now  <- nowMs
      azat <- loadAzat(user)
      text  = content.format("temple.blessing.offer", "cost" -> BlessingCost.toString) +
                (if (azat.blessingActive(now))
                   "\n\n" + content.format("temple.blessing.active", "remaining" -> azat.blessingRemaining(now).getOrElse(""))
                 else "")
      choices = List(
        content.choice("Donate", "temple.blessing.donate"),
        content.choice("BackToPriest", "temple.back")
      )
      _ <- renderer.show(user, Screen(s"💰 ${hero.gold}  🪙 ${hero.doubloons}\n\n$text", choices))
    } yield StateType.TempleAzat

  private def donate(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      now  <- nowMs
      azat <- loadAzat(user)
      _ <-
        if (hero.doubloons < BlessingCost)
          renderer.show(user, Screen(content.format("temple.blessing.notEnough", "cost" -> BlessingCost.toString), Nil))
        else {
          // Продлеваем от максимума (текущий остаток или now) на неделю; +250 отдыхов.
          val base    = azat.blessingUntil.filter(_ > now).getOrElse(now)
          val updated = azat.copy(
            blessingUntil = Some(base + AzatState.BlessingDurationMs),
            instantRests  = azat.instantRests + AzatState.BlessingInstantRests
          )
          heroDao.updateDoubloons(user.userId, hero.doubloons - BlessingCost) *>
            saveAzat(user, updated) *>
            renderer.show(user, Screen(content.text("temple.blessing.granted"), Nil))
        }
      _ <- showPriest(user, renderer)
    } yield StateType.TempleAzat

  private def loadAzat(user: User): Task[AzatState] =
    heroDao.readAzatData(user.userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object TempleAzatState {
  val BlessingCost: Long = 250L
}
