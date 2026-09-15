package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, ItemDetails, ItemType}
import pangea.model.monster.Race
import pangea.model.quest.{NpcQuest, QuestData}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{CharacterMenu, NpcQuestDialog, NpcQuestLog, State, UserAction}
import zio.{Task, ZIO}

/** Трактирщик. Принимает квестовые предметы: из подходящих трофеев инвентаря
  * (тип `Trophy`, нужная раса) забирает САМЫЙ ЦЕННЫЙ — с наибольшим
  * коэффициентом вида (Реликвия 4.0 > Талисман 2.0 > Голова 1.0 > Мешок 0.5),
  * при равных коэффициентах — старший по уровню. Опыт начисляется по формуле
  * `5 + Ур.трофея × коэффициент(вид трофея)`, округляя вверх. Коэффициент
  * берётся из [[pangea.model.item.TrophyKind]].
  */
case class InnkeeperState(
  heroDao: HeroDao,
  inventoryRepo: InventoryRepository,
  content: SceneContent
) extends State {

  /** «Плата за первую кружку»: принести любой трофей — Трактирщик забирает самый
    * дешёвый, платит серебром и рассказывает, куда герой попал. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Innkeeper, "Inn")

  private val branch = new Branch(
    routes = Map(
      "TurnInQuest" -> Target.Run { (user, _, renderer) =>
        turnInQuest(user, renderer)
      },
      quest.questAction   -> Target.Run { (user, _, renderer) => questTalk(user, renderer) },
      quest.acceptAction  -> Target.Run { (user, _, renderer) => quest.accept(user, renderer) *> showMenu(user, renderer).as(StateType.Innkeeper) },
      quest.declineAction -> Target.Run { (user, _, renderer) => showMenu(user, renderer).as(StateType.Innkeeper) },
      "KinetLore"         -> Target.Run { (user, _, renderer) =>
        renderer.show(user, Screen(quest.text("lore"), List(content.choice("BackFromLore", quest.key("loreBack"))))).as(StateType.Innkeeper) },
      // «Письмо Марисе»: что Трактирщик знает об адресате.
      "AskMarisa"         -> Target.Run { (user, _, renderer) =>
        NpcQuestLog.modify(heroDao, user.userId)(q => if (q.onStep(NpcQuest.Marisa, 1)) q.update(NpcQuest.Marisa)(_.copy(step = 2)) else q) *>
          renderer.show(user, content.screen("marisa.innkeeper.answer")).as(StateType.Innkeeper) },
      "ContinueSearch"    -> Target.Run { (user, _, renderer) =>
        renderer.show(user, Screen(content.text("marisa.innkeeper.farewell"), Nil)).as(StateType.Tavern) },
      "OpenCharacter" -> Target.Run { (user, _, _) =>
        CharacterMenu.open(heroDao, user.userId, StateType.Innkeeper)
      },
      "ElementalLore"     -> Target.Run { (user, _, renderer) => offerLore(user, renderer) },
      "PayElementalLore"  -> Target.Run { (user, _, renderer) => payLore(user, renderer) },
      "JoeLore"           -> Target.Run { (user, _, renderer) => offerJoeLore(user, renderer) },
      "PayJoeLore"        -> Target.Run { (user, _, renderer) => payJoeLore(user, renderer) },
      "WolfLore"          -> Target.Run { (user, _, renderer) => offerWolfLore(user, renderer) },
      "PayWolfLore"       -> Target.Run { (user, _, renderer) => payWolfLore(user, renderer) },
      "BackFromInnkeeper" -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (user, _, renderer) =>
      showMenu(user, renderer).as(StateType.Innkeeper)
    }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets + StateType.HeroStats + StateType.Tavern

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    showMenu(user, renderer)

  override def action(
      user: User,
      ua: UserAction,
      renderer: Renderer
  ): Task[StateType] = branch.act(user, ua, renderer)

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      lore   <- readLore(user)
      quests <- quest.load(user)
      // Кнопка про элементалей появляется, только когда герой их уже видел, и
      // висит, пока он не заплатит за рассказ.
      loreBtn = Option.when(lore.metElemental && !lore.elementalLore)(
        content.choice("ElementalLore", "innkeeper.elementalLoreLabel"))
      // То же и про Гнилого Джо: кнопка висит, пока рассказ не куплен.
      joeBtn = Option.when(lore.metJoe && !lore.joeLore)(
        content.choice("JoeLore", "innkeeper.joeLoreLabel"))
      // И про Белого волка — после первой встречи на поляне.
      wolfBtn = Option.when(lore.metWolf && !lore.wolfLore)(
        content.choice("WolfLore", "innkeeper.wolfLoreLabel"))
      // Рассказ о Кинэте — награда за первое задание, дальше бесплатно.
      kinetBtn = Option.when(quests.isDone(NpcQuest.Innkeeper))(
        content.choice("KinetLore", quest.key("loreLabel")))
      // Письмо Марисе на руках, а о ней ещё не спрашивали.
      marisaBtn = Option.when(quests.onStep(NpcQuest.Marisa, 1))(
        content.choice("AskMarisa", "marisa.innkeeper.askLabel"))
      _ <- renderer.show(
        user,
        Screen(
          content.text("innkeeper.text"),
          List(
            Some(content.choice("TurnInQuest", "innkeeper.turnInLabel")),
            marisaBtn,
            quest.button(quests),
            kinetBtn,
            loreBtn,
            joeBtn,
            wolfBtn,
            Some(content.choice("OpenCharacter", "common.character")),
            Some(content.choice("BackFromInnkeeper", "innkeeper.backLabel"))
          ).flatten
        )
      )
    } yield ()

  /** Кнопка задания: завязка, пока не взято; сдача трофея, пока идёт. */
  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    quest.load(user).flatMap { quests =>
      if (quests.isDone(NpcQuest.Innkeeper)) showMenu(user, renderer)
      else if (!quests.isTaken(NpcQuest.Innkeeper)) quest.offer(user, renderer, quest.text("intro"))
      else questTurnIn(user, renderer)
    }.as(StateType.Innkeeper)

  /** Сдача: самый дешёвый трофей из сумки уходит Трактирщику, герой получает
    * серебро, опыт и рассказ. Без трофея — только упрёк. */
  private def questTurnIn(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      _ <- InnkeeperState.cheapestTrophy(inv.items.data) match {
        case None =>
          renderer.show(user, Screen(quest.text("step1Fail"), Nil)) *> showMenu(user, renderer)
        case Some(trophy) =>
          for {
            _        <- inventoryRepo.removeItem(trophy.id, hero.id).mapError(e => new Throwable(e.toString))
            _        <- heroDao.updateSilver(user.userId, hero.silver + InnkeeperState.QuestSilver)
            done     <- quest.complete(user, hero, identity)
            (_, expLine) = done
            _        <- renderer.show(user, Screen(quest.text("outro"), Nil))
            _        <- renderer.show(user, Screen(quest.format("reward",
                          "item" -> trophy.name, "silver" -> InnkeeperState.QuestSilver.toString, "exp" -> expLine), Nil))
            _        <- showMenu(user, renderer)
          } yield ()
      }
    } yield ()

  /** Предложение купить рассказ об элементалях. */
  private def offerLore(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(
      content.format("innkeeper.elementalLoreOffer", "price" -> InnkeeperState.LorePrice.toString),
      List(
        content.choice("PayElementalLore", "innkeeper.elementalLorePay"),
        content.choice("BackFromLore", "innkeeper.elementalLoreDecline")
      ))).as(StateType.Innkeeper)

  /** Оплата: списываем серебро, запоминаем покупку и рассказываем легенду. */
  private def payLore(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      lore <- readLore(user)
      _ <- if (lore.elementalLore) showMenu(user, renderer)
           else if (hero.silver < InnkeeperState.LorePrice)
             renderer.show(user, Screen(content.text("innkeeper.elementalLoreNoSilver"), Nil)) *>
               showMenu(user, renderer)
           else
             heroDao.updateSilver(user.userId, hero.silver - InnkeeperState.LorePrice) *>
               heroDao.writeLoreData(user.userId, lore.copy(elementalLore = true).asJson) *>
               renderer.show(user, Screen(
                 content.text("innkeeper.elementalLoreText"),
                 List(content.choice("BackFromLore", "innkeeper.elementalLoreDone")))) 
    } yield StateType.Innkeeper

  /** Предложение рассказа про Гнилого Джо: цена и две кнопки. */
  private def offerJoeLore(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(
      content.format("innkeeper.joeLoreOffer", "price" -> InnkeeperState.JoeLorePrice.toString),
      List(
        content.choice("PayJoeLore", "innkeeper.joeLorePay"),
        content.choice("BackFromLore", "innkeeper.joeLoreDecline")
      ))).as(StateType.Innkeeper)

  private def payJoeLore(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      lore <- readLore(user)
      _ <- if (lore.joeLore) showMenu(user, renderer)
           else if (hero.silver < InnkeeperState.JoeLorePrice)
             renderer.show(user, Screen(content.text("innkeeper.joeLoreNoSilver"), Nil)) *>
               showMenu(user, renderer)
           else
             heroDao.updateSilver(user.userId, hero.silver - InnkeeperState.JoeLorePrice) *>
               heroDao.writeLoreData(user.userId, lore.copy(joeLore = true).asJson) *>
               renderer.show(user, Screen(
                 content.text("innkeeper.joeLoreText"),
                 List(content.choice("BackFromLore", "innkeeper.joeLoreDone"))))
    } yield StateType.Innkeeper

  /** Предложение рассказа про Белого волка: цена и две кнопки. */
  private def offerWolfLore(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(
      content.format("innkeeper.wolfLoreOffer", "price" -> InnkeeperState.WolfLorePrice.toString),
      List(
        content.choice("PayWolfLore", "innkeeper.wolfLorePay"),
        content.choice("BackFromLore", "innkeeper.wolfLoreDecline")
      ))).as(StateType.Innkeeper)

  private def payWolfLore(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      lore <- readLore(user)
      _ <- if (lore.wolfLore) showMenu(user, renderer)
           else if (hero.silver < InnkeeperState.WolfLorePrice)
             renderer.show(user, Screen(content.text("innkeeper.wolfLoreNoSilver"), Nil)) *>
               showMenu(user, renderer)
           else
             heroDao.updateSilver(user.userId, hero.silver - InnkeeperState.WolfLorePrice) *>
               heroDao.writeLoreData(user.userId, lore.copy(wolfLore = true).asJson) *>
               renderer.show(user, Screen(
                 content.text("innkeeper.wolfLoreText"),
                 List(content.choice("BackFromLore", "innkeeper.wolfLoreDone"))))
    } yield StateType.Innkeeper

  private def readLore(user: User): Task[LoreData] =
    heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  // Сдать квест: забираем самый ценный подходящий трофей, начисляем опыт, закрываем задание.
  private def turnInQuest(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      data <- readQuests(user)
      _ <- data.flatMap(_.active) match {
        case None =>
          renderer.show(
            user,
            Screen(content.text("innkeeper.noActive"), Nil)
          ) *> showMenu(user, renderer)
        case Some(raceName) =>
          for {
            inv <- inventoryRepo
              .get(hero.id)
              .mapError(e => new Throwable(e.toString))
            matching = InnkeeperState.bestTrophyFor(inv.items.data, raceName)
            _ <- matching match {
              case None =>
                renderer.show(
                  user,
                  Screen(
                    content.format(
                      "innkeeper.noItem",
                      "race" -> Race.withName(raceName).toString
                    ),
                    Nil
                  )
                ) *>
                  showMenu(user, renderer)
              case Some(item) =>
                val expGained = questExp(item)
                val leveled   = hero.gainExp(expGained)
                inventoryRepo
                  .removeItem(item.id, hero.id)
                  .mapError(e => new Throwable(e.toString)) *>
                  heroDao.updateExpAndLevel(
                    user.userId,
                    leveled.exp,
                    leveled.lvl,
                    leveled.upgradePoints
                  ) *>
                  heroDao.updateDoubloons(user.userId, hero.doubloons + 1) *>
                  heroDao.writeQuestData(
                    user.userId,
                    data.get.copy(active = None).asJson
                  ) *>
                  renderer.show(
                    user,
                    Screen(
                      content.format(
                        "innkeeper.completed",
                        "item" -> item.name,
                        "exp"  -> expGained.toString
                      ),
                      Nil
                    )
                  ) *>
                  ZIO.when(leveled.lvl > hero.lvl)(
                    renderer.show(
                      user,
                      Screen(
                        content.format(
                          "innkeeper.levelUp",
                          "level" -> leveled.lvl.toString
                        ),
                        Nil
                      )
                    )
                  ) *>
                  showMenu(user, renderer)
            }
          } yield ()
      }
    } yield StateType.Innkeeper

  // Опыт за трофей: 5 + Ур.трофея × коэффициент(вид), округление вверх.
  private def questExp(trophy: Item): Long =
    math.ceil(5.0 + trophy.lvl.toDouble * InnkeeperState.trophyCoef(trophy)).toLong

  private def readQuests(user: User): Task[Option[QuestData]] =
    heroDao.readQuestData(user.userId).map(_.flatMap(_.as[QuestData].toOption))

  private def getHero(user: User): Task[Hero] =
    heroDao
      .getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object InnkeeperState {

  /** Сколько трактирщик просит за рассказ об элементалях. */
  val LorePrice: Long = 2000L

  /** Цена рассказа про Гнилого Джо — он попроще элементалей. */
  val JoeLorePrice: Long = 1000L

  /** Цена рассказа про Белого волка. */
  val WolfLorePrice: Long = 1500L

  /** Сколько серебра Трактирщик даёт за первый трофей. */
  val QuestSilver: Long = 100L

  /** Самый дешёвый трофей в сумке — для платы за первую кружку: наименьший
    * коэффициент вида, при равных — младший по уровню. */
  def cheapestTrophy(items: List[Item]): Option[Item] =
    items
      .filter(i => i.id != 0L && i.itemType == ItemType.Trophy)
      .sortBy(i => (trophyCoef(i), i.lvl))
      .headOption

  /** Трофей, который уйдёт в счёт задания по расе `raceName`: из подходящих
    * берём с наибольшим коэффициентом вида, при равных — старший по уровню
    * (он даёт больше опыта по формуле `5 + Ур. × коэффициент`). None, если
    * подходящих трофеев в инвентаре нет. */
  def bestTrophyFor(items: List[Item], raceName: String): Option[Item] =
    items
      .filter(_.details match {
        case ItemDetails.Trophy(race, _, _) => race == raceName
        case _                              => false
      })
      .sortBy(i => (-trophyCoef(i), -i.lvl))
      .headOption

  private def trophyCoef(item: Item): Double = item.details match {
    case t: ItemDetails.Trophy => t.coefValue
    case _                     => 0.0
  }
}
