package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType}
import pangea.model.monster.Race
import pangea.model.quest.QuestData
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.{Task, ZIO}

/** Трактирщик. Принимает квестовые предметы: забирает ПЕРВЫЙ подходящий трофей
  * из инвентаря (тип `Trophy`, нужная раса) и начисляет опыт по формуле `5 +
  * Ур.трофея × коэффициент(вид трофея)`, округляя вверх. Коэффициент берётся
  * из [[pangea.model.item.TrophyKind]].
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
    renderer.show(
      user,
      Screen(
        content.text("innkeeper.text"),
        List(
          content.choice("TurnInQuest", "innkeeper.turnInLabel"),
          content.choice("OpenCharacter", "common.character"),
          content.choice("BackFromInnkeeper", "innkeeper.backLabel")
        )
      )
    )

  // Сдать квест: забираем первый подходящий трофей, начисляем опыт, закрываем задание.
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
            matching = inv.items.data.find(i =>
              i.itemType == ItemType.Trophy && i.race.contains(raceName)
            )
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
    math.ceil(5.0 + trophy.lvl.toDouble * trophy.trophyKind.get.coef).toLong

  private def readQuests(user: User): Task[Option[QuestData]] =
    heroDao.readQuestData(user.userId).map(_.flatMap(_.as[QuestData].toOption))

  private def getHero(user: User): Task[Hero] =
    heroDao
      .getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
