package pangea.service.state.states.marisa

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{MarisaQuest, NpcQuestLog, State, UserAction}
import zio.{Task, ZIO}

/** Поиски Марисы после подсказки Жреца: дом Долорес Моньо в Портовом квартале,
  * письмо через дверь, и за дверью — сама Мариса. Прямая цепочка экранов без
  * ветвлений; в конце герой обещает зайти за ней, получает карту (если ещё не
  * вскрывал письмо) и остаётся в Портовом квартале. Шаг задания — третий:
  * Мариса ждёт, и карта спросит, брать ли её с собой. */
case class MarisaSearchState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "FoundLetter" -> Target.Run { (u, _, r) => r.show(u, content.screen("marisa.search.letter")).as(StateType.MarisaSearch) },
      "AskMarisa"   -> Target.Run { (u, _, r) => r.show(u, content.screen("marisa.search.marisa")).as(StateType.MarisaSearch) },
      "Agree"       -> Target.Run { (u, _, r) => r.show(u, content.screen("marisa.search.agree")).as(StateType.MarisaSearch) },
      "Promise"     -> Target.Run { (u, _, r) => promise(u, r) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.MarisaSearch) }
  )

  override def targetStates: Set[StateType] = Set(StateType.MarisaSearch, StateType.HarborQuarter)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("marisa.search.dolores"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** «Хорошо, я зайду за Вами»: Мариса ждёт, карта — у героя. */
  private def promise(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      _     <- NpcQuestLog.modify(heroDao, user.userId)(_.update(NpcQuest.Marisa)(_.copy(step = 3)))
      given <- MarisaQuest.revealMap(heroDao, inventoryRepo, itemRepo, user.userId, hero)
      _     <- renderer.show(user, Screen(content.text("marisa.search.alone"), Nil))
      _     <- ZIO.when(given)(renderer.show(user, Screen(
                 content.format("marisa.questItemAdded", "item" -> QuestItemKind.KelvinMap.displayName), Nil)))
    } yield StateType.HarborQuarter

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
