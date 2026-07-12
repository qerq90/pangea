package pangea.service.state.states.dungeon

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DungeonState(heroDao: HeroDao, inventoryRepo: pangea.repository.inventory.InventoryRepository, scheduler: Scheduler, content: SceneContent) extends State {

  import DungeonState.{MinTrackMs, MaxTrackMs, TrackAction, TrackingData, FindDelayMs, ResolveAction}

  private val branch = new Branch(
    routes = Map(
      "FindEvent"     -> Target.Run { (user, _, renderer) => findEvent(user, renderer) },
      "ResolveEvent"  -> Target.Run { (user, _, renderer) => resolveEvent(user, renderer) },
      "GoDarker"      -> Target.Run { (user, _, renderer) => goDarker(user, renderer) },
      "StopTracking"  -> Target.Run { (user, _, renderer) => stopTracking(user, renderer) },
      "GoLighter"     -> Target.Run { (user, _, renderer) => goLighter(user, renderer) },
      "GoToCity"       -> Target.Goto(StateType.GlobalMap),
      "OpenCharacter"  -> Target.Run { (user, _, _) => CharacterMenu.open(heroDao, user.userId, StateType.Dungeon) },
      "Rest"           -> Target.Goto(StateType.Rest)
    ),
    fallback = Target.Goto(StateType.Dungeon)
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.HeroStats

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero     <- getHero(user)
      tracking <- readTracking(user)
      // Пока идёт выслеживание тьмы — герой заперт в нём: никаких других
      // действий, только экран ожидания с единственной кнопкой «идти по следу».
      _        <- renderer.show(user, tracking.fold(enterScreen(hero, now))(_ => trackingScreen))
    } yield ()

  /** Экран этажа. Раскладка по рядам:
   *   - row 0: «Исследовать уровень» (зелёная);
   *   - row 1: «Двигаться к тьме»/«…к свету» — зелёные если ход открыт, красные если закрыт;
   *   - row 2: «В портал», «Персонаж»;
   *   - row 3: «Отдых» (зелёная). */
  private def enterScreen(hero: Hero, nowMs: Long): Screen = {
    val text       = content.format("dungeon.enter.text",
      "level"    -> hero.dungeonLevel.toString,
      "hp"       -> hero.fightStats.hp.toString,
      "maxHp"    -> hero.effectiveMaxHp(nowMs).toString,
      "armor"    -> hero.fightStats.armor.toString,
      "maxArmor" -> hero.effectiveMaxArmor(nowMs).toString,
      "energy"   -> hero.fightStats.energy.min(hero.maxEnergy(nowMs)).toString,
      "maxEnergy"-> hero.maxEnergy(nowMs).toString)
    val byId       = content.screen("dungeon.enter").choices.map(c => c.id -> c).toMap
    def mv(id: String, open: Boolean): pangea.engine.Choice =
      byId(id).copy(color = if (open) ChoiceColor.Positive else ChoiceColor.Negative, row = Some(1))
    val choices = List(
      byId("FindEvent").copy(color     = ChoiceColor.Positive, row = Some(0)),
      mv("GoDarker",  hero.canGoDarker),
      mv("GoLighter", hero.canGoLighter),
      byId("GoToCity").copy(row        = Some(2)),
      byId("OpenCharacter").copy(row   = Some(2)),
      byId("Rest").copy(color          = ChoiceColor.Positive, row = Some(3))
    )
    Screen(text, choices)
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Нажатие «Исследовать уровень»: показываем экран осмотра и планируем push —
   *  поллер разбудит героя синтетическим ResolveEvent через 5 секунд, тогда и
   *  выберется/покажется само событие. Игрок ничего не жмёт, просто ждёт. */
  private def findEvent(user: User, renderer: Renderer): Task[StateType] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _   <- renderer.show(user, Screen(content.text("dungeon.findEvent"), Nil))
      _   <- scheduler.schedule(user.userId, now + FindDelayMs, TaskKind.LevelSearch, StateType.Dungeon, ResolveAction)
    } yield StateType.Dungeon

  /** Пауза осмотра вышла (push от поллера) — выбираем и разыгрываем событие. */
  private def resolveEvent(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      idx    <- Random.nextIntBounded(StateType.events.size)
      event   = StateType.events(idx)
      result <- event match {
        case StateType.Battle => startBattle(user, hero)
        case StateType.Spring => healAtSpring(user, hero, now, renderer)
        case other            => ZIO.succeed(other)
      }
    } yield result

  private def healAtSpring(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] = {
    val maxHp    = hero.effectiveMaxHp(nowMs)
    val healed   = (maxHp - hero.fightStats.hp).max(0L)
    // Ручеёк дополнительно восстанавливает 10% максимума Энергии.
    val maxEn        = hero.maxEnergy(nowMs)
    val newEnergy    = (hero.fightStats.energy + (maxEn * 10 / 100L).max(1L)).min(maxEn)
    val energyBack   = newEnergy - hero.fightStats.energy
    val flask    = hero.equipment.flask
    val flaskDetails = flask.details match {
      case f: pangea.model.item.ItemDetails.Flask => Some(f)
      case _                                      => None
    }
    val hasFlask = flaskDetails.isDefined
    for {
      _ <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, energy = newEnergy))
      _ <- ZIO.foreachDiscard(flaskDetails)(f => heroDao.updateEquipment(user.userId,
             hero.equipment.copy(flask = flask.copy(details = f.refilled))))
      _ <- inventoryRepo.refillFlasks(hero.id).orElse(ZIO.unit)
      _ <- renderer.show(user, Screen(
             content.format("dungeon.spring",
               "healed"      -> healed.toString,
               "energy"      -> energyBack.toString,
               "flaskFilled" -> (if (hasFlask) content.text("dungeon.springFlaskFilled") else "")), Nil))
      roll   <- Random.nextIntBetween(1, 101)
      result <- if (roll <= 50)
                  renderer.show(user, Screen(content.text("dungeon.springAmbush"), Nil)) *>
                    startBattle(user, hero)
                else ZIO.succeed(StateType.Dungeon)
    } yield result
  }

  private def startBattle(user: User, hero: Hero): Task[StateType] =
    for {
      seed         <- Random.nextLong
      (monster, _)  = MonsterGenerator.generate(hero.dungeonLevel, Rng(seed))
      battle        = SoloPveBattle.from(monster, hero)
      _            <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      // Обычный бой: после добычи возврат в лабиринт — чистим routing в scene_data,
      // чтобы victory не подхватил чужой «куда вернуться» от прошлого события.
      _            <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
    } yield StateType.Battle

  /** Движение к свету (выше). Путь наверх открыт всегда, кроме первого этажа. */
  private def goLighter(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _ <- if (!hero.canGoLighter)
             renderer.show(user, Screen(content.text("dungeon.alreadyAtTop"), Nil)) *>
               renderer.show(user, enterScreen(hero, now))
           else {
             val newLevel = math.max(1, hero.dungeonLevel - 1)
             heroDao.updateDungeonLevel(user.userId, newLevel) *>
               renderer.show(user, enterScreen(hero.copy(dungeonLevel = newLevel), now))
           }
    } yield StateType.Dungeon

  /** Движение к тьме (глубже). Если путь уже открыт — просто спускаемся. Иначе
   *  запускается «выслеживание»: таймер 2–5 минут (в scene_data), по истечении
   *  которого герой выходит на Отмеченного тьмой ЦЕЛЕВОГО (следующего) уровня. */
  private def goDarker(user: User, renderer: Renderer): Task[StateType] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero     <- getHero(user)
      tracking <- readTracking(user)
      result <- (hero.canGoDarker, tracking) match {
        case (true, _)                              => descend(user, hero, now, renderer)
        // абсолютное дно лабиринта — выслеживать нечего, глубже пути нет
        case (false, _) if hero.dungeonLevel >= 150 => atDeepest(user, hero, now, renderer)
        case (false, None)                          => startTracking(user, now, renderer)
        // выслеживание уже идёт: сюда попадаем по срабатыванию таймера (поллер
        // шлёт синтетический GoDarker в момент дедлайна) — выходим на Отмеченного.
        case (false, Some(_))                       => encounterMarked(user, hero, renderer)
      }
    } yield result

  private def atDeepest(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] =
    (renderer.show(user, Screen(content.text("dungeon.deepestReached"), Nil)) *>
      renderer.show(user, enterScreen(hero, nowMs))).as(StateType.Dungeon)

  private def descend(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] = {
    val newLevel = math.min(150, hero.dungeonLevel + 1)
    heroDao.updateDungeonLevel(user.userId, newLevel) *>
      renderer.show(user, enterScreen(hero.copy(dungeonLevel = newLevel), nowMs))
        .as(StateType.Dungeon)
  }

  /** Старт выслеживания: пишем дедлайн в scene_data и планируем push-задачу —
   *  поллер сам разбудит героя синтетическим GoDarker в момент дедлайна (игрок
   *  ничего не жмёт, длительность ему не сообщается). На экране ожидания остаётся
   *  только «перестать искать». */
  private def startTracking(user: User, nowMs: Long, renderer: Renderer): Task[StateType] =
    for {
      extra   <- Random.nextLongBetween(MinTrackMs, MaxTrackMs)
      deadline = nowMs + extra
      _ <- heroDao.writeSceneData(user.userId, TrackingData(deadline).asJson)
      _ <- scheduler.schedule(user.userId, deadline, TaskKind.DarknessTracking, StateType.Dungeon, TrackAction)
      _ <- renderer.show(user, Screen(content.text("dungeon.trackingStart"), Nil))
      _ <- renderer.show(user, trackingScreen)
    } yield StateType.Dungeon

  /** Время выслеживания вышло — генерируем гарантированного Отмеченного тьмой
   *  ЦЕЛЕВОГО уровня (`dungeonLevel + 1`) и уходим в бой. Победа над ним двигает
   *  `maxDungeonLevel` (см. BattleState). scene_data чистим — таймер отработал. */
  private def encounterMarked(user: User, hero: Hero, renderer: Renderer): Task[StateType] =
    for {
      seed          <- Random.nextLong
      targetLevel    = math.min(150, hero.dungeonLevel + 1)
      (monster, _)   = MonsterGenerator.generateMarked(targetLevel, Rng(seed))
      battle         = SoloPveBattle.from(monster, hero)
      _ <- renderer.show(user, Screen(content.text("dungeon.trackingFound"), Nil))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
      _ <- scheduler.cancel(user.userId, TaskKind.DarknessTracking)
    } yield StateType.Battle

  /** Прекратить выслеживание без подтверждения: сбрасываем таймер и возвращаем
   *  обычный экран этажа. */
  private def stopTracking(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _    <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
      _    <- scheduler.cancel(user.userId, TaskKind.DarknessTracking)
      _    <- renderer.show(user, Screen(content.text("dungeon.trackingStopped"), Nil))
      _    <- renderer.show(user, enterScreen(hero, now))
    } yield StateType.Dungeon

  /** Экран ожидания во время выслеживания: единственная кнопка «перестать искать»
   *  (мгновенный выход, без переспроса). Продвижение делает таймер, не игрок —
   *  поэтому кнопки «идти по следу» нет. */
  private def trackingScreen: Screen =
    Screen(
      content.text("dungeon.trackingWait"),
      List(
        Choice("StopTracking", content.text("dungeon.trackingLeaveBtn"), color = ChoiceColor.Negative, row = Some(0))
      )
    )

  private def readTracking(user: User): Task[Option[TrackingData]] =
    heroDao.readSceneData(user.userId)
      .map(_.flatMap(_.as[TrackingData].toOption))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object DungeonState {

  // Диапазон выслеживания прохода вглубь: 2–5 минут. Игроку длительность не
  // сообщается — время определяется по стенным часам (`nowMs + rnd`).
  private val MinTrackMs: Long = 2L * 60L * 1000L
  private val MaxTrackMs: Long = 5L * 60L * 1000L

  // payload синтетического действия, которым поллер будит выслеживание в момент
  // дедлайна (маршрутизируется в goDarker, как и обычное нажатие «к тьме»).
  private val TrackAction: String = """{"action":"GoDarker"}"""

  // Пауза «осмотра уровня» перед показом события и payload, которым поллер её
  // завершает (маршрутизируется в resolveEvent).
  private val FindDelayMs: Long   = 5L * 1000L
  private val ResolveAction: String = """{"action":"ResolveEvent"}"""

  /** Прогресс выслеживания в scene_data: момент (мс), когда Отмеченный тьмой
   *  будет «выслежен». Пока это лежит в scene_data — герой в режиме ожидания. */
  final case class TrackingData(darknessTrackingUntil: Long)
  object TrackingData {
    implicit val encoder: Encoder[TrackingData] = deriveEncoder[TrackingData]
    implicit val decoder: Decoder[TrackingData] = deriveDecoder[TrackingData]
  }
}
