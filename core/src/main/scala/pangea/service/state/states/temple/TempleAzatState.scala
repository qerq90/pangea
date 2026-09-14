package pangea.service.state.states.temple

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{AzatState, Hero}
import pangea.model.item.{Item, ItemType, MaterialKind}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{AzatData, NpcQuestDialog, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Храм Азата: вход, Жрец (лор + благословение) и переход в Зал Азата. Благословение
 *  — пожертвование 250 дублонов даёт недельный баф ([[AzatState.blessingUntil]]) и
 *  250 мгновенных отдыхов. */
case class TempleAzatState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {
  import TempleAzatState._

  /** «То, что не пропадает»: показать найденный камень — Жрец объясняет, что с
    * ним делать, отсыпает пыль и открывает первый рецепт куба. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Priest, "Priest")

  private val branch = new Branch(
    routes = Map(
      quest.questAction   -> Target.Run { (u, _, r) => questTalk(u, r) },
      quest.acceptAction  -> Target.Run { (u, _, r) => quest.accept(u, r) *> showPriest(u, r) },
      quest.declineAction -> Target.Run { (u, _, r) => showPriest(u, r) },
      // «Письмо Марисе»: Жрец помнит прихожанку и знает её подругу.
      "AskMarisa"    -> Target.Run { (u, _, r) => r.show(u, content.screen("marisa.priest.answer")).as(StateType.TempleAzat) },
      "SearchDolores" -> Target.Goto(StateType.MarisaSearch),
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

  /** Меню Жреца: кнопка задания встаёт перед «Назад». */
  private def showPriest(user: User, renderer: Renderer): Task[StateType] =
    quest.load(user).flatMap { quests =>
      val base    = content.screen("temple.priest")
      val (front, back) = base.choices.partition(_.id != "BackToTemple")
      // О Марисе — после того как о ней спросили Трактирщика.
      val marisa  = Option.when(quests.onStep(NpcQuest.Marisa, 2))(content.choice("AskMarisa", "marisa.priest.askLabel"))
      renderer.show(user, base.copy(choices = front ++ marisa.toList ++ quest.button(quests).toList ++ back))
    }.as(StateType.TempleAzat)

  /** Кнопка задания: завязка, пока не взято; показ камня, пока идёт. */
  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    quest.load(user).flatMap { quests =>
      if (quests.isDone(NpcQuest.Priest)) showPriest(user, renderer).unit
      else if (!quests.isTaken(NpcQuest.Priest)) quest.offer(user, renderer, quest.text("intro"))
      else questShowGem(user, renderer)
    }.as(StateType.TempleAzat)

  /** Показ камня: он остаётся у героя; Жрец отсыпает горсть такой же пыли (если
    * есть место), записывает рецепт и платит серебром и опытом. */
  private def questShowGem(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      _ <- inv.items.data.find(i => i.id != 0L && i.itemType == ItemType.Gem).flatMap(_.gem) match {
        case None =>
          renderer.show(user, Screen(quest.text("step1Fail"), Nil)) *> showPriest(user, renderer)
        case Some(gem) =>
          val dust: Item = MaterialGenerator.item(MaterialKind.dustOf(gem.kind))
          for {
            given <- itemRepo.persist(hero.id, dust)
                       .flatMap(d => inventoryRepo.addItem(hero.id, d)).as(true)
                       .catchAll(_ => ZIO.succeed(false))
            _     <- heroDao.updateSilver(user.userId, hero.silver + QuestSilver)
            done  <- quest.complete(user, hero, (q: NpcQuests) => q.withRecipe(NpcQuests.DustAssemblyRecipe))
            (_, expLine) = done
            _     <- renderer.show(user, Screen(quest.text("outro"), Nil))
            _     <- renderer.show(user, Screen(
                       if (given) quest.format("rewardDust", "dust" -> dust.name, "silver" -> QuestSilver.toString, "exp" -> expLine)
                       else quest.format("rewardNoRoom", "silver" -> QuestSilver.toString, "exp" -> expLine), Nil))
            _     <- showPriest(user, renderer)
          } yield ()
      }
    } yield ()

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
      _ <- renderer.show(user, Screen(s"🪙 ${hero.silver}  🟡 ${hero.doubloons}\n\n$text", choices))
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
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      .flatMap(now => AzatData.load(heroDao, user.userId, now))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object TempleAzatState {
  val BlessingCost: Long = 250L

  /** Серебро за показанный Жрецу камень. */
  val QuestSilver: Long = 50L
}
