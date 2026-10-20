package pangea.service.state.states.temple

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{AzatState, Hero, LoreData}
import pangea.model.item.{Item, ItemSet, ItemType, MaterialKind}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.purse.Purse
import pangea.service.state.{AzatData, CityExit, NpcQuestDialog, QuestGoods, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Храм Азата: вход, Жрец (лор + благословение) и переход в Зал Азата. Благословение
 *  — пожертвование 250 дублонов даёт недельный баф ([[AzatState.blessingUntil]]) и
 *  250 мгновенных отдыхов.
 *
 *  Жрец же рассказывает о некромантах — бесплатно, когда к нему отправил
 *  Трактирщик, — и продаёт святую воду. Тому, в ком тьма пустила корни (набор
 *  «Некромант» от шести вещей), воды он не даёт. */
case class TempleAzatState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  // Найденный камень мог сам лечь в Ларец Азата — показать можно и оттуда.
  artifacts:     Option[ArtifactRepository] = None
) extends State {
  import TempleAzatState._

  /** «То, что не пропадает»: показать найденный камень — Жрец объясняет, что с
    * ним делать, отсыпает пыль и открывает первый рецепт куба. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Priest, "Priest")

  /** Кошель: святую воду Жрец берёт серебром из рук. */
  private val purse = Purse(heroDao, None)

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
      CityExit.route,
      "WhoIsAzat"    -> Target.Run { (u, _, r) => showWho(u, r) },
      "AskBlessing"  -> Target.Run { (u, _, r) => showBlessing(u, r) },
      "Donate"       -> Target.Run { (u, _, r) => donate(u, r) },
      "BackToPriest" -> Target.Run { (u, _, r) => showPriest(u, r) },
      "PriestNecroLore" -> Target.Run { (u, _, r) => showNecroLore(u, r) },
      "HolyWater"    -> Target.Run { (u, _, r) => showHolyWater(u, r) },
      "BuyHolyWater" -> Target.Run { (u, _, r) => buyHolyWater(u, r) },
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
      byId("LeaveTemple").copy(color = ChoiceColor.Negative, row = Some(1)),
      CityExit.button(content, Some(1))
    )
    renderer.show(user, Screen(content.text("temple.enter.text"), choices))
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Меню Жреца: кнопка задания встаёт перед «Назад». */
  private def showPriest(user: User, renderer: Renderer): Task[StateType] =
    (quest.load(user) <*> readLore(user)).flatMap { case (quests, lore) =>
      val base    = content.screen("temple.priest")
      val (front, back) = base.choices.partition(_.id != "BackToTemple")
      // О некромантах — когда сюда отправил Трактирщик.
      val necro   = Option.when(lore.necroSent)(content.choice("PriestNecroLore", "temple.necroLoreLabel"))
      // Святая вода на виду у всех: продаст ли её Жрец, решается при покупке.
      val holy    = content.choice("HolyWater", "temple.holyWater.label")
      // О Марисе — после того как о ней спросили Трактирщика.
      val marisa  = Option.when(quests.onStep(NpcQuest.Marisa, 2))(content.choice("AskMarisa", "marisa.priest.askLabel"))
      renderer.show(user, base.copy(choices =
        front ++ necro.toList ++ List(holy) ++ marisa.toList ++ quest.button(quests).toList ++ back))
    }.as(StateType.TempleAzat)

  /** Рассказ о некромантах: Жрец его не продаёт. */
  private def showNecroLore(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("temple.necroLore"),
      List(content.choice("BackToPriest", "temple.back")))).as(StateType.TempleAzat)

  /** Святая вода: цена и кнопка покупки. */
  private def showHolyWater(user: User, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap { hero =>
      renderer.show(user, Screen(
        content.format("temple.holyWater.offer",
          "price" -> HolyWaterPrice.toString, "silver" -> hero.silver.toString),
        List(
          content.choice("BuyHolyWater", "temple.holyWater.buy", "price" -> HolyWaterPrice.toString),
          content.choice("BackToPriest", "temple.back"))))
    }.as(StateType.TempleAzat)

  /** Покупка склянки. Тому, в ком тьма уже пустила корни, — набор «Некромант»
    * от шести вещей, — Жрец воды не даёт. Не хватает серебра или места в
    * сумке — ничего не списываем. */
  private def buyHolyWater(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      wallet <- purse.wallet(hero)
      _ <- if (hero.sets.has(ItemSet.Necromancer, HolyWaterBannedFrom))
             renderer.show(user, Screen(content.text("temple.holyWater.refused"), Nil))
           else if (!wallet.canAfford(HolyWaterPrice))
             renderer.show(user, Screen(content.format("temple.holyWater.noSilver",
               "price" -> HolyWaterPrice.toString), Nil))
           else
             for {
               persisted <- itemRepo.persist(hero.id, MaterialGenerator.item(MaterialKind.HolyWater))
               added     <- inventoryRepo.addItem(hero.id, persisted).as(true).catchAll(_ => ZIO.succeed(false))
               _ <- if (!added) renderer.show(user, Screen(content.text("common.inventoryFull"), Nil))
                    else purse.charge(user.userId, hero, HolyWaterPrice) *>
                      renderer.show(user, Screen(content.text("temple.holyWater.bought"), Nil))
             } yield ()
      res <- showHolyWater(user, renderer)
    } yield res

  private def readLore(user: User): Task[LoreData] =
    heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  /** Кнопка задания: завязка, пока не взято; показ камня, пока идёт. */
  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    quest.load(user).flatMap { quests =>
      if (quests.isDone(NpcQuest.Priest)) showPriest(user, renderer).unit
      else if (!quests.isTaken(NpcQuest.Priest)) quest.offer(user, renderer, quest.text("intro"))
      else questShowGem(user, renderer)
    }.as(StateType.TempleAzat)

  /** Показ камня: он остаётся у героя — в сумке или в Ларце Азата; Жрец
    * отсыпает горсть такой же пыли (если есть место), записывает рецепт и платит
    * серебром и опытом. */
  private def questShowGem(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      goods <- QuestGoods.all(inventoryRepo, artifacts, hero.id)
      _ <- goods.map(_.item).find(_.itemType == ItemType.Gem).flatMap(_.gem) match {
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
          val updated = azat.blessedFor(AzatState.BlessingDurationMs, now)
            .copy(instantRests = azat.instantRests + AzatState.BlessingInstantRests)
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

  /** Склянка святой воды у Жреца. */
  val HolyWaterPrice: Long = 1000L

  /** С какого порога набора «Некромант» Жрец святой воды уже не продаёт. */
  val HolyWaterBannedFrom: Int = 6

  /** Серебро за показанный Жрецу камень. */
  val QuestSilver: Long = 50L
}
