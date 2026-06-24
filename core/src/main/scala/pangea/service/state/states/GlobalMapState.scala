package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}
import java.util.concurrent.TimeUnit

case class GlobalMapState(heroDao: HeroDao, content: SceneContent) extends State {

  // Снятая комната исцеляет травмы спустя 3 часа реального времени.
  private val RoomDurationMs = 3L * 60L * 60L * 1000L
  private val RoomStartKey   = "tavernRoomStartedAt"

  private val branch = new Branch(
    routes = Map(
      "Tavern"          -> Target.Run { (user, _, renderer) => showTavern(user, renderer).as(StateType.GlobalMap) },
      "Guild"           -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, Screen(content.text("globalMap.guild"), Nil)).as(StateType.GlobalMap) },
      "Construction"    -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, Screen(content.text("globalMap.construction"), Nil)).as(StateType.GlobalMap) },
      "ReturnToDungeon" -> Target.Goto(StateType.Dungeon),
      "RentRoom"        -> Target.Run { (user, _, renderer) => rentRoom(user, renderer) },
      "LeaveRoom"       -> Target.Run { (user, _, renderer) => leaveRoom(user, renderer) },
      "ConfirmLeaveRoom"-> Target.Run { (user, _, renderer) => confirmLeaveRoom(user, renderer) },
      "CancelLeaveRoom" -> Target.Run { (user, _, renderer) => showRoom(user, renderer).as(StateType.GlobalMap) },
      "StreetMerchants" -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, content.screen("globalMap.streetMerchants")).as(StateType.GlobalMap) },
      "MerchantRichelieu" -> Target.Goto(StateType.Merchant),
      "ReturnToCity"    -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) },
      "LeaveTavern"     -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) }
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      text  = content.format("globalMap.enter.text",
                "heroHp"  -> hero.fightStats.hp.toString,
                "heroMax" -> hero.effectiveMaxHp(now).toString,
                "heroArmor" -> hero.fightStats.armor.toString,
                "gold"    -> hero.gold.toString)
      _    <- renderer.show(user, Screen(text, content.screen("globalMap.enter").choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Таверна: если комната уже снята — показываем саму комнату, иначе предложение снять.
  private def showTavern(user: User, renderer: Renderer): Task[Unit] =
    roomStartedAt(user).flatMap {
      case Some(_) => showRoom(user, renderer)
      case None =>
        getHero(user).flatMap { hero =>
          val text = content.format("globalMap.tavern.text",
            "cost" -> roomCost(hero).toString,
            "gold" -> hero.gold.toString)
          renderer.show(user, Screen(text, content.screen("globalMap.tavern").choices))
        }
    }

  // Снять комнату: списываем золото, фиксируем время старта, показываем комнату.
  private def rentRoom(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      cost  = roomCost(hero)
      _ <- if (hero.gold < cost)
             renderer.show(user, Screen(
               content.format("globalMap.notEnoughGold",
                 "cost" -> cost.toString, "gold" -> hero.gold.toString), Nil))
           else
             heroDao.updateGold(user.userId, hero.gold - cost) *>
               heroDao.writeSceneData(user.userId, Json.obj(RoomStartKey -> now.asJson)) *>
               showRoom(user, renderer)
    } yield StateType.GlobalMap

  private def showRoom(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(
      content.text("globalMap.room.text"),
      List(Choice("LeaveRoom", content.text("globalMap.room.leaveLabel")))))

  // Уйти из комнаты: спустя 3 часа — лечим травмы; раньше — спрашиваем подтверждение.
  private def leaveRoom(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero    <- getHero(user)
      started <- roomStartedAt(user)
      _ <- started match {
             case None => showTavern(user, renderer)
             case Some(start) if now - start >= RoomDurationMs =>
               val healed   = hero.copy(traumaUntil = None, traumaNames = Nil)
               val maxHp    = healed.effectiveMaxHp(now)
               val maxArmor = healed.effectiveMaxArmor(now)
               heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, armor = maxArmor)) *>
                 heroDao.updateTrauma(user.userId, None, Nil) *>
                 heroDao.writeSceneData(user.userId, Json.Null) *>
                 renderer.show(user, Screen(content.text("globalMap.roomHealed"), Nil)) *>
                 showTavern(user, renderer)
             case Some(start) =>
               val remaining = formatRemaining(RoomDurationMs - (now - start))
               renderer.show(user, Screen(
                 content.format("globalMap.roomConfirmLeave", "remaining" -> remaining),
                 List(
                   Choice("ConfirmLeaveRoom", content.text("globalMap.roomLeaveYes")),
                   Choice("CancelLeaveRoom",  content.text("globalMap.roomLeaveNo")))))
           }
    } yield StateType.GlobalMap

  // Да: прерываем без исцеления — чистим комнату и возвращаемся в таверну.
  private def confirmLeaveRoom(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      showTavern(user, renderer).as(StateType.GlobalMap)

  private def roomStartedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.hcursor.get[Long](RoomStartKey).toOption))

  private def roomCost(hero: Hero): Long = (hero.lvl * 10L).max(10L)

  private def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val h    = secs / 3600
    val m    = (secs % 3600) / 60
    if (h > 0) s"${h}ч ${m}мин"
    else if (m > 0) s"${m}мин"
    else s"${secs}с"
  }

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
