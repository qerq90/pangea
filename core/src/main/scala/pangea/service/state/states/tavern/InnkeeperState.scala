package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, ItemDetails}
import pangea.model.monster.Race
import pangea.model.quest.QuestData
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{CharacterMenu, State, UserAction}
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

  private val branch = new Branch(
    routes = Map(
      "TurnInQuest" -> Target.Run { (user, _, renderer) =>
        turnInQuest(user, renderer)
      },
      "OpenCharacter" -> Target.Run { (user, _, _) =>
        CharacterMenu.open(heroDao, user.userId, StateType.Innkeeper)
      },
      "ElementalLore"     -> Target.Run { (user, _, renderer) => offerLore(user, renderer) },
      "PayElementalLore"  -> Target.Run { (user, _, renderer) => payLore(user, renderer) },
      "BackFromInnkeeper" -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (user, _, renderer) =>
      showMenu(user, renderer).as(StateType.Innkeeper)
    }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets + StateType.HeroStats

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    showMenu(user, renderer)

  override def action(
      user: User,
      ua: UserAction,
      renderer: Renderer
  ): Task[StateType] = branch.act(user, ua, renderer)

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    readLore(user).flatMap { lore =>
      // Кнопка про элементалей появляется, только когда герой их уже видел, и
      // висит, пока он не заплатит за рассказ.
      val loreBtn = Option.when(lore.metElemental && !lore.elementalLore)(
        content.choice("ElementalLore", "innkeeper.elementalLoreLabel"))
      renderer.show(
        user,
        Screen(
          content.text("innkeeper.text"),
          List(
            Some(content.choice("TurnInQuest", "innkeeper.turnInLabel")),
            loreBtn,
            Some(content.choice("OpenCharacter", "common.character")),
            Some(content.choice("BackFromInnkeeper", "innkeeper.backLabel"))
          ).flatten
        )
      )
    }

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

  /** Трофей, который уйдёт в счёт задания по расе `raceName`: из подходящих
    * берём с наибольшим коэффициентом вида, при равных — старший по уровню
    * (он даёт больше опыта по формуле `5 + Ур. × коэффициент`). None, если
    * подходящих трофеев в инвентаре нет. */
  def bestTrophyFor(items: List[Item], raceName: String): Option[Item] =
    items
      .filter(_.details match {
        case ItemDetails.Trophy(race, _) => race == raceName
        case _                           => false
      })
      .sortBy(i => (-trophyCoef(i), -i.lvl))
      .headOption

  private def trophyCoef(item: Item): Double = item.details match {
    case ItemDetails.Trophy(_, kind) => kind.coef
    case _                           => 0.0
  }
}
