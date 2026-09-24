package pangea.service.state.states.temple

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{AzatState, CubeStatus, Hero}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.artifact.{ArtifactKind, HeroArtifacts}
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.state.{AzatData, State, UserAction}
import java.util.concurrent.TimeUnit
import zio.{Task, ZIO}

/** Зал Азата: лор про кубы, подход к кубу (покупка/активация/открытие крафта) и
 *  пополнение зарядов у жреца. */
case class HallAzatState(
  heroDao:   HeroDao,
  content:   SceneContent,
  bank:      Option[BankRepository] = None,
  artifacts: Option[ArtifactRepository] = None
) extends State {
  import HallAzatState._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "ApproachCube"         -> Target.Run { (u, _, r) => approachCube(u, r) },
      "Recharge"             -> Target.Run { (u, _, r) => showRecharge(u, r) },
      "RechargeArtifact"     -> Target.Run { (u, ua, r) => rechargeArtifact(u, ua, r) },
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
            List(content.choice("BuyCube", "hall.cube.buy", "price" -> CubePrice.toString), content.choice("BackToHall", "hall.back"))))
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
      hero   <- getHero(user)
      azat   <- loadAzat(user)
      wallet <- purse.wallet(hero)
      _ <- if (!azat.cubeFound) renderer.show(user, Screen(content.text("hall.cube.notFound"), Nil))
           else if (hero.doubloons < ActivateDoubloons || !wallet.canAfford(ActivateSilver))
             renderer.show(user, Screen(content.format("hall.cube.notEnoughActivate",
               "doubloons" -> ActivateDoubloons.toString, "silver" -> ActivateSilver.toString), Nil))
           else
             heroDao.updateDoubloons(user.userId, hero.doubloons - ActivateDoubloons) *>
               purse.charge(user.userId, hero, ActivateSilver) *>
               saveAzat(user, azat.copy(cube = CubeStatus.Active, cubeCharges = AzatState.MaxCharges)) *>
               renderer.show(user, Screen(content.text("hall.cube.activated"), Nil))
      _ <- enter(user, renderer)
    } yield StateType.HallAzat

  private def showRecharge(user: User, renderer: Renderer): Task[StateType] =
    for {
      // Сборные артефакты Фета заряжает тот же Жрец — кнопки видит только их владелец.
      owned <- ownedArtifacts(user)
      artifactButtons = owned.map { case (kind, _) =>
        Choice("RechargeArtifact",
          Choice.fit(content.format("hall.recharge.artifact",
            "title" -> content.text(s"artifact.${kind.key}.title"),
            "cost"  -> HeroArtifacts.RechargeSilver.toString)),
          data = Map("kind" -> kind.entryName))
      }
      _ <- renderer.show(user, Screen(content.format("hall.recharge.text",
             "full" -> RechargeFullSilver.toString, "half" -> RechargeHalfSilver.toString),
             List(
               content.choice("RechargeFull", "hall.recharge.full"),
               content.choice("RechargeHalf", "hall.recharge.half")
             ) ++ artifactButtons :+ content.choice("BackToHall", "hall.back")))
    } yield StateType.HallAzat

  /** Какие артефакты есть у героя (и сколько в них зарядов). */
  private def ownedArtifacts(user: User): Task[List[(ArtifactKind, Int)]] =
    artifacts match {
      case None => ZIO.succeed(Nil)
      case Some(repo) =>
        getHero(user).flatMap(h => repo.get(h.id).either).map {
          case Right(all) => ArtifactKind.values.toList.map(k => k -> all.of(k)).collect {
            case (k, a) if a.owned => k -> a.charges
          }
          case Left(_) => Nil
        }
    }

  private def rechargeArtifact(user: User, ua: UserAction, renderer: Renderer): Task[StateType] = {
    val kind = ua.payload
      .flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("kind")))
      .flatMap(ArtifactKind.withNameOption)
    (kind, artifacts) match {
      case (Some(k), Some(repo)) =>
        for {
          hero   <- getHero(user)
          wallet <- purse.wallet(hero)
          all    <- repo.get(hero.id).mapError(e => new Throwable(e.toString))
          a       = all.of(k)
          cost    = HeroArtifacts.RechargeSilver
          _ <- if (!a.owned) renderer.show(user, Screen(content.text(s"artifact.${k.key}.notOwned"), Nil))
               else if (a.charges >= HeroArtifacts.MaxCharges)
                 renderer.show(user, Screen(content.text("hall.recharge.artifactFull"), Nil))
               else if (!wallet.canAfford(cost))
                 renderer.show(user, Screen(content.format("hall.recharge.artifactNotEnough", "cost" -> cost.toString), Nil))
               else
                 purse.charge(user.userId, hero, cost) *>
                   repo.recharge(hero.id, k).mapError(e => new Throwable(e.toString)).flatMap(done =>
                     renderer.show(user, Screen(content.format("hall.recharge.artifactDone",
                       "title" -> content.text(s"artifact.${k.key}.title"),
                       "charges" -> done.charges.toString), Nil)))
          res <- showRecharge(user, renderer)
        } yield res
      case _ => showRecharge(user, renderer)
    }
  }

  private def recharge(user: User, renderer: Renderer, cost: Long, charges: Int): Task[StateType] =
    for {
      hero   <- getHero(user)
      azat   <- loadAzat(user)
      wallet <- purse.wallet(hero)
      _ <- if (!azat.hasCube) renderer.show(user, Screen(content.text("hall.recharge.noCube"), Nil))
           else if (azat.cubeCharges >= AzatState.MaxCharges)
             renderer.show(user, Screen(content.text("hall.recharge.full_already"), Nil))
           else if (!wallet.canAfford(cost))
             renderer.show(user, Screen(content.format("hall.recharge.notEnough", "cost" -> cost.toString), Nil))
           else {
             val newCharges = (azat.cubeCharges + charges).min(AzatState.MaxCharges)
             purse.charge(user.userId, hero, cost) *>
               saveAzat(user, azat.copy(cubeCharges = newCharges)) *>
               renderer.show(user, Screen(content.format("hall.recharge.done", "charges" -> newCharges.toString), Nil))
           }
      _ <- enter(user, renderer)
    } yield StateType.HallAzat

  private def loadAzat(user: User): Task[AzatState] =
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      .flatMap(now => AzatData.load(heroDao, user.userId, now))

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
