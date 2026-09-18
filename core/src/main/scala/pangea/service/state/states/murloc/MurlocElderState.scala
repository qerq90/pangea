package pangea.service.state.states.murloc

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{MurlocQuest, State, UserAction}
import zio.{Task, ZIO}

/** Старейшина Мрачноглаз выходит к герою в лабиринте после сотого убитого:
  * сюда ведёт добыча того боя (а если она вела в другую сцену — следующий
  * осмотр этажа, см. DungeonState). Короткий разговор в четыре реплики,
  * вялое согласие героя — и карта деревни в сумке, задание на втором шаге.
  * Герою-мурлоку старейшина говорит иначе (`<ключ>Kin`). */
case class MurlocElderState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "ElderWho"   -> Target.Run { (u, _, r) => say(u, r, "who", "ElderWhat", "whatLabel") },
      "ElderWhat"  -> Target.Run { (u, _, r) => say(u, r, "what", "ElderWhere", "whereLabel") },
      "ElderWhere" -> Target.Run { (u, _, r) => say(u, r, "where", "ElderAgree", "agreeLabel") },
      "ElderAgree" -> Target.Run { (u, _, r) => agree(u, r) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.MurlocElder) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Dungeon, StateType.MurlocElder)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap(hero => renderer.show(user, screen(hero, "meet", "ElderWho", "whoLabel")))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Реплика старейшины и одна кнопка — следующая реплика героя. */
  private def screen(hero: Hero, textKey: String, next: String, labelKey: String): Screen =
    Screen(
      content.text("murlocVillage.elder." + MurlocQuest.key(hero, textKey)),
      List(content.choice(next, "murlocVillage.elder." + MurlocQuest.key(hero, labelKey))))

  private def say(user: User, renderer: Renderer, textKey: String, next: String, labelKey: String): Task[StateType] =
    getHero(user).flatMap(hero => renderer.show(user, screen(hero, textKey, next, labelKey))).as(StateType.MurlocElder)

  /** Герой соглашается — как умеет; старейшина отдаёт карту и уходит. */
  private def agree(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      _     <- renderer.show(user, Screen(content.text("murlocVillage.elder." + MurlocQuest.key(hero, "agree")), Nil))
      given <- MurlocQuest.giveMap(heroDao, inventoryRepo, itemRepo, user.userId, hero)
      _     <- ZIO.when(given)(renderer.show(user, Screen(
                 content.format("marisa.questItemAdded", "item" -> QuestItemKind.MurlocVillageMap.displayName), Nil)))
    } yield StateType.Dungeon

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
