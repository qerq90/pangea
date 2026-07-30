package pangea.service.state.states.temple

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{AzatState, CubeStatus, Hero}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/** Зал Азата: лор про кубы, подход к кубу (покупка/активация/открытие крафта) и
 *  пополнение зарядов у жреца. */
case class HallAzatState(heroDao: HeroDao, content: SceneContent) extends State {
  import HallAzatState._

  private val branch = new Branch(
    routes = Map(
      "ApproachCube"         -> Target.Run { (u, _, r) => approachCube(u, r) },
      "Recharge"             -> Target.Run { (u, _, r) => showRecharge(u, r) },
      "BackToTempleFromHall" -> Target.Goto(StateType.TempleAzat),
      "BuyCube"              -> Target.Run { (u, _, r) => buyCube(u, r) },
      "ActivateCube"         -> Target.Run { (u, _, r) => activateCube(u, r) },
      "OpenCube"             -> Target.Goto(StateType.Cube),
      "RechargeFull"         -> Target.Run { (u, _, r) => recharge(u, r, RechargeFullSilver, RechargeFullCharges) },
      "RechargeHalf"         -> Target.Run { (u, _, r) => recharge(u, r, RechargeHalfSilver, RechargeHalfCharges) },
      "BackToHall"           -> Target.Run { (u, _, r) => enter(u, r).as(StateType.HallAzat) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.HallAzat) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = {
    val byId = content.screen("hall.enter").choices.map(c => c.id -> c).toMap
    val choices = List(
      byId("ApproachCube").copy(color = ChoiceColor.Positive, row = Some(0)),
      byId("Recharge").copy(row = Some(0)),
      byId("BackToTempleFromHall").copy(color = ChoiceColor.Negative, row = Some(1))
    )
    renderer.show(user, Screen(content.text("hall.enter.text"), choices))
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Экран подхода к кубу — зависит от статуса владения.
  private def approachCube(user: User, renderer: Renderer): Task[StateType] =
    loadAzat(user).flatMap { azat =>
      azat.cube match {
        case CubeStatus.None =>
          renderer.show(user, Screen(content.format("hall.cube.absent", "price" -> CubePrice.toString),
            List(content.choice("BuyCube", "hall.cube.buy"), content.choice("BackToHall", "hall.back"))))
        case CubeStatus.FoundInactive =>
          renderer.show(user, Screen(content.format("hall.cube.inactive",
            "doubloons" -> ActivateDoubloons.toString, "silver" -> ActivateSilver.toString),
            List(content.choice("ActivateCube", "hall.cube.activate"), content.choice("BackToHall", "hall.back"))))
        case CubeStatus.Active =>
          renderer.show(user, Screen(content.text("hall.cube.active"),
            List(content.choice("OpenCube", "hall.cube.open"), content.choice("BackToHall", "hall.back"))))
      }
    }.as(StateType.HallAzat)

  private def buyCube(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      azat <- loadAzat(user)
      _ <- if (!azat.cubeAbsent) renderer.show(user, Screen(content.text("hall.cube.alreadyHave"), Nil))
           else if (hero.doubloons < CubePrice)
             renderer.show(user, Screen(content.format("hall.cube.notEnoughDoubloons", "price" -> CubePrice.toString), Nil))
           else
             heroDao.updateDoubloons(user.userId, hero.doubloons - CubePrice) *>
               saveAzat(user, azat.copy(cube = CubeStatus.Active, cubeCharges = AzatState.MaxCharges)) *>
               renderer.show(user, Screen(content.text("hall.cube.bought"), Nil))
      _ <- enter(user, renderer)
    } yield StateType.HallAzat

  private def activateCube(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      azat <- loadAzat(user)
      _ <- if (!azat.cubeFound) renderer.show(user, Screen(content.text("hall.cube.notFound"), Nil))
           else if (hero.doubloons < ActivateDoubloons || hero.gold < ActivateSilver)
             renderer.show(user, Screen(content.format("hall.cube.notEnoughActivate",
               "doubloons" -> ActivateDoubloons.toString, "silver" -> ActivateSilver.toString), Nil))
           else
             heroDao.updateDoubloons(user.userId, hero.doubloons - ActivateDoubloons) *>
               heroDao.updateGold(user.userId, hero.gold - ActivateSilver) *>
               saveAzat(user, azat.copy(cube = CubeStatus.Active, cubeCharges = AzatState.MaxCharges)) *>
               renderer.show(user, Screen(content.text("hall.cube.activated"), Nil))
      _ <- enter(user, renderer)
    } yield StateType.HallAzat

  private def showRecharge(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.format("hall.recharge.text",
      "full" -> RechargeFullSilver.toString, "half" -> RechargeHalfSilver.toString),
      List(
        content.choice("RechargeFull", "hall.recharge.full"),
        content.choice("RechargeHalf", "hall.recharge.half"),
        content.choice("BackToHall", "hall.back")
      ))).as(StateType.HallAzat)

  private def recharge(user: User, renderer: Renderer, cost: Long, charges: Int): Task[StateType] =
    for {
      hero <- getHero(user)
      azat <- loadAzat(user)
      _ <- if (!azat.hasCube) renderer.show(user, Screen(content.text("hall.recharge.noCube"), Nil))
           else if (azat.cubeCharges >= AzatState.MaxCharges)
             renderer.show(user, Screen(content.text("hall.recharge.full_already"), Nil))
           else if (hero.gold < cost)
             renderer.show(user, Screen(content.format("hall.recharge.notEnough", "cost" -> cost.toString), Nil))
           else {
             val newCharges = (azat.cubeCharges + charges).min(AzatState.MaxCharges)
             heroDao.updateGold(user.userId, hero.gold - cost) *>
               saveAzat(user, azat.copy(cubeCharges = newCharges)) *>
               renderer.show(user, Screen(content.format("hall.recharge.done", "charges" -> newCharges.toString), Nil))
           }
      _ <- enter(user, renderer)
    } yield StateType.HallAzat

  private def loadAzat(user: User): Task[AzatState] =
    heroDao.readAzatData(user.userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object HallAzatState {
  val CubePrice: Long          = 200L   // дублоны за покупку куба
  val ActivateDoubloons: Long  = 20L
  val ActivateSilver: Long     = 10000L
  val RechargeFullSilver: Long = 10000L
  val RechargeHalfSilver: Long = 5000L
  val RechargeFullCharges: Int = 50
  val RechargeHalfCharges: Int = 25
}
