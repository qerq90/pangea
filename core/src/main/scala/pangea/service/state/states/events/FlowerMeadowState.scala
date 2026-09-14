package pangea.service.state.states.events

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{Hero, Knowledge}
import pangea.model.item.MaterialKind
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.states.events.FlowerMeadowState._
import pangea.service.state.{CharacterMenu, HerbLore, InventoryFeedback, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Поляна цветов (5% в лабиринте). Герой собирает 2–6 цветов, по одному каждые
  * 2–3 минуты — «афк», как осмотр логова элементаля. Что именно сорвано, зависит
  * от знаний: без «Знаний о цветах» нужного ранга это «странный цветок», и с
  * каждым таким есть шанс (интеллект ÷ 4 %) самому понять, какие цветы ценные.
  * Уйти можно в любой момент, без вопросов; «Персонаж» — обычное меню, по
  * возвращении поляна на месте: если цветок за это время «созрел», он выдаётся
  * сразу, иначе таймер ставится заново. */
case class FlowerMeadowState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  scheduler:     Scheduler,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "FlowerFind"    -> Target.Run { (u, _, r) => findOne(u, r) },
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.FlowerMeadow) },
      "LeaveMeadow"   -> Target.Run { (u, _, r) => leave(u, r) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.FlowerMeadow) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Dungeon, StateType.FlowerMeadow, StateType.HeroStats)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      scene <- readScene(user)
      _ <- scene match {
        case None =>
          for {
            count <- Random.nextIntBetween(MinFlowers, MaxFlowers + 1)
            _     <- scheduleNext(user, now, MeadowScene(count, 0L))
            _     <- renderer.show(user, meadowScreen)
          } yield ()
        // Вернулись из меню персонажа: созревший цветок — сразу, иначе таймер заново.
        case Some(s) if now >= s.nextAt => findOne(user, renderer).unit
        case Some(s) =>
          scheduler.schedule(user.userId, s.nextAt, TaskKind.FlowerMeadow, StateType.FlowerMeadow, FindAction) *>
            renderer.show(user, meadowScreen)
      }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def meadowScreen: Screen =
    Screen(content.text("flowerMeadow.enter.text"), List(
      content.choice("OpenCharacter", "common.character"),
      content.choice("LeaveMeadow", "flowerMeadow.leave").copy(color = ChoiceColor.Negative)
    ))

  /** Очередной цветок: ранг 98/2, вид случайный, узнан ли — по знаниям. */
  private def findOne(user: User, renderer: Renderer): Task[StateType] =
    for {
      now   <- nowMs
      hero  <- getHero(user)
      scene <- readScene(user)
      res <- scene match {
        case None => ZIO.succeed(StateType.Dungeon)
        case Some(s) =>
          for {
            rankRoll <- Random.nextIntBetween(1, 101)
            rank      = if (rankRoll <= HerbLore.RareHerbPct) 2 else 1
            pool      = MaterialKind.herbsOfRank(rank)
            idx      <- Random.nextIntBounded(pool.size)
            lore     <- HerbLore.readLore(heroDao, user.userId)
            kind      = HerbLore.recognised(lore, pool(idx))
            item      = MaterialGenerator.item(kind)
            persisted <- itemRepo.persist(hero.id, item)
            added     <- inventoryRepo.addItem(hero.id, persisted).as(true).catchAll(_ => ZIO.succeed(false))
            slots     <- InventoryFeedback.freeSlotsLine(inventoryRepo, content, hero.id)
            lost       = if (added) "" else "\n" + content.text("common.inventoryFull")
            _         <- renderer.show(user, Screen(
                           content.format("flowerMeadow.found", "flower" -> item.name) + lost + "\n" + slots, Nil))
            // Странный цветок — шанс самому понять, что к чему (только пока не знаешь простых трав).
            _         <- ZIO.when(kind == MaterialKind.StrangeFlower && !lore.knows(Knowledge.FlowersRank1))(
                           insight(user, hero, lore, now, renderer))
            left       = s.left - 1
            out <- if (left <= 0)
                     heroDao.writeSceneData(user.userId, Json.Null) *>
                       renderer.show(user, Screen(content.text("flowerMeadow.done"), Nil)).as(StateType.Dungeon)
                   else
                     scheduleNext(user, now, s.copy(left = left)) *>
                       renderer.show(user, meadowScreen).as(StateType.FlowerMeadow)
          } yield out
      }
    } yield res

  /** Бросок на догадку: интеллект ÷ 4 процентов. Удача — знания первого ранга, сам. */
  private def insight(user: User, hero: Hero, lore: pangea.model.hero.LoreData, now: Long, renderer: Renderer): Task[Unit] =
    Random.nextIntBetween(1, 101).flatMap { roll =>
      ZIO.when(roll <= HerbLore.insightChance(hero, now))(
        HerbLore.writeLore(heroDao, user.userId, lore.learn(Knowledge.FlowersRank1, alone = true)) *>
          renderer.show(user, Screen(
            content.text("flowerMeadow.insight") + "\n" +
              content.format("knowledge.gained", "title" -> Knowledge.FlowersRank1.title), Nil))
      ).unit
    }

  private def leave(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.FlowerMeadow) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("flowerMeadow.left"), Nil)).as(StateType.Dungeon)

  /** Следующий цветок через 2–3 минуты; момент — в сцене, чтобы пережить уход в меню. */
  private def scheduleNext(user: User, now: Long, scene: MeadowScene): Task[Unit] =
    for {
      delta <- Random.nextLongBetween(MinDelayMs, MaxDelayMs + 1L)
      at     = now + delta
      _     <- writeScene(user, scene.copy(nextAt = at))
      _     <- scheduler.schedule(user.userId, at, TaskKind.FlowerMeadow, StateType.FlowerMeadow, FindAction)
    } yield ()

  private def readScene(user: User): Task[Option[MeadowScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[MeadowScene].toOption))

  private def writeScene(user: User, scene: MeadowScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object FlowerMeadowState {
  val MinFlowers: Int  = 2
  val MaxFlowers: Int  = 6
  val MinDelayMs: Long = 2L * 60L * 1000L
  val MaxDelayMs: Long = 3L * 60L * 1000L

  val FindAction: String = """{"action":"FlowerFind"}"""

  /** Сколько цветов осталось и когда созреет следующий. */
  final case class MeadowScene(left: Int, nextAt: Long)
  object MeadowScene {
    implicit val encoder: Encoder[MeadowScene] = (s: MeadowScene) =>
      Json.obj("flowersLeft" -> s.left.asJson, "nextFlowerAt" -> s.nextAt.asJson)
    implicit val decoder: Decoder[MeadowScene] = (c: HCursor) =>
      for {
        left <- c.get[Int]("flowersLeft")
        at   <- c.getOrElse[Long]("nextFlowerAt")(0L)
      } yield MeadowScene(left, at)
  }
}
