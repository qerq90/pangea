package pangea.service.state.states.tavern

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Таверна «Золотой якорь». Меню локации: снять комнату (исцеляет травмы спустя 3
 * часа), доска заданий и трактирщик — отдельные состояния. Состояние снятой комнаты
 * хранится в `scene_data` (транзиентно) — пока она снята, меню заменяется экраном
 * комнаты.
 */
case class TavernState(heroDao: HeroDao, content: SceneContent) extends State {

  // Снятая комната исцеляет травмы спустя 3 часа реального времени.
  private val RoomDurationMs = 3L * 60L * 60L * 1000L
  private val RoomStartKey   = "tavernRoomStartedAt"

  private val branch = new Branch(
    routes = Map(
      "RentRoom"        -> Target.Run { (user, _, renderer) => rentRoom(user, renderer) },
      "LeaveRoom"       -> Target.Run { (user, _, renderer) => leaveRoom(user, renderer) },
      "ConfirmLeaveRoom"-> Target.Run { (user, _, renderer) => confirmLeaveRoom(user, renderer) },
      "CancelLeaveRoom" -> Target.Run { (user, _, renderer) => showRoom(user, renderer).as(StateType.Tavern) },
      "QuestBoard"      -> Target.Goto(StateType.QuestBoard),
      "Innkeeper"       -> Target.Goto(StateType.Innkeeper),
      "OpenCharacter"   -> Target.Run { (user, _, _) => CharacterMenu.open(heroDao, user.userId, StateType.Tavern) },
      "LeaveTavern"     -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.Tavern) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.HeroStats

  // Вход в таверну: если комната уже снята — показываем комнату, иначе меню.
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    roomStartedAt(user).flatMap {
      case Some(_) => showRoom(user, renderer)
      case None =>
        getHero(user).flatMap { hero =>
          val text = content.format("tavern.menu.text",
            "cost" -> roomCost(hero).toString,
            "gold" -> hero.gold.toString)
          renderer.show(user, Screen(text, content.screen("tavern.menu").choices))
        }
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Снять комнату: списываем золото, фиксируем время старта, показываем комнату.
  private def rentRoom(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      cost  = roomCost(hero)
      _ <- if (hero.gold < cost)
             renderer.show(user, Screen(
               content.format("tavern.notEnoughGold",
                 "cost" -> cost.toString, "gold" -> hero.gold.toString), Nil))
           else
             heroDao.updateGold(user.userId, hero.gold - cost) *>
               heroDao.writeSceneData(user.userId, Json.obj(RoomStartKey -> now.asJson)) *>
               showRoom(user, renderer)
    } yield StateType.Tavern

  private def showRoom(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(
      content.text("tavern.room.text"),
      List(content.choice("LeaveRoom", "tavern.room.leaveLabel"))))

  // Уйти из комнаты: спустя 3 часа — лечим травмы; раньше — спрашиваем подтверждение.
  private def leaveRoom(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      hero    <- getHero(user)
      started <- roomStartedAt(user)
      _ <- started match {
             case None => enter(user, renderer)
             case Some(start) if now - start >= RoomDurationMs =>
               val healed   = hero.copy(traumaUntil = None, traumaNames = Nil)
               val maxHp    = healed.effectiveMaxHp(now)
               val maxArmor = healed.effectiveMaxArmor(now)
               heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, armor = maxArmor)) *>
                 heroDao.updateTrauma(user.userId, None, Nil) *>
                 heroDao.writeSceneData(user.userId, Json.Null) *>
                 renderer.show(user, Screen(content.text("tavern.roomHealed"), Nil)) *>
                 enter(user, renderer)
             case Some(start) =>
               val remaining = formatRemaining(RoomDurationMs - (now - start))
               renderer.show(user, Screen(
                 content.format("tavern.roomConfirmLeave", "remaining" -> remaining),
                 List(
                   content.choice("ConfirmLeaveRoom", "tavern.roomLeaveYes"),
                   content.choice("CancelLeaveRoom",  "tavern.roomLeaveNo"))))
           }
    } yield StateType.Tavern

  // Да: прерываем без исцеления — чистим комнату и возвращаемся в меню таверны.
  private def confirmLeaveRoom(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      enter(user, renderer).as(StateType.Tavern)

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

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
