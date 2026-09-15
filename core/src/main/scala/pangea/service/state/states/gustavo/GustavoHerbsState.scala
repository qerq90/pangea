package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Knowledge
import pangea.model.item.QuestItemKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{HerbLore, MarisaQuest, State, UserAction}
import zio.Task

/** «Расскажи о травах». Густаво учит нехотя: пока герой носит ему «странные
  * цветки» по цене сена, ему выгодно, чтобы тот ничего не понимал. Поэтому
  * первый трактат стоит 15 000, второй — 30 000, и что сказать герою, зависит от
  * того, что тот уже знает и как узнал — сам или по книге. */
case class GustavoHerbsState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State with GustavoScene {

  private val branch = new Branch(
    routes = Map(
      "BuyTreatise1" -> Target.Run { (u, _, r) => buy(u, r, QuestItemKind.FlowerTreatise1, HerbLore.Treatise1Price) },
      "BuyTreatise2" -> Target.Run { (u, _, r) => buy(u, r, QuestItemKind.FlowerTreatise2, HerbLore.Treatise2Price) },
      "Back"         -> Target.Goto(StateType.Gustavo)
    ),
    fallback = Target.Run { (u, _, r) => talk(u, r).as(StateType.GustavoHerbs) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Gustavo, StateType.GustavoHerbs)

  override def enter(user: User, renderer: Renderer): Task[Unit] = talk(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Что Густаво скажет — по знаниям героя, и только потом по книге в сумке:
    * знание, добытое своим умом, важнее недочитанного трактата. Трактат о том,
    * что герой уже знает, тихо уходит из сумки. */
  private def talk(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      lore0 <- HerbLore.readLore(heroDao, user.userId)
      // Трактат о том, что герой уже знает, тихо уходит из сумки.
      settled <- HerbLore.settleBooks(heroDao, inventoryRepo, user.userId, hero, lore0)
      (lore, _) = settled
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      back   = content.choice("Back", "gustavo.herbs.back")
      screen =
        if (lore.knows(Knowledge.FlowersRank2))
          Screen(content.text("gustavo.herbs.nothingMore"), List(back))
        // Книга о ещё не известном — дочитай; о том, что и так знаешь, уже выброшена выше.
        else if (MarisaQuest.has(items, QuestItemKind.FlowerTreatise1) || MarisaQuest.has(items, QuestItemKind.FlowerTreatise2))
          Screen(content.text("gustavo.herbs.finishReading"), List(back))
        else if (lore.knows(Knowledge.FlowersRank1)) {
          val key = if (lore.learnedAlone(Knowledge.FlowersRank1)) "gustavo.herbs.selfTaught" else "gustavo.herbs.graduate"
          Screen(content.format(key, "price" -> HerbLore.Treatise2Price.toString), List(
            content.choice("BuyTreatise2", "gustavo.herbs.buy2", "price" -> HerbLore.Treatise2Price.toString).copy(color = ChoiceColor.Positive),
            back))
        } else
          Screen(content.format("gustavo.herbs.firstLesson", "price" -> HerbLore.Treatise1Price.toString), List(
            content.choice("BuyTreatise1", "gustavo.herbs.buy1", "price" -> HerbLore.Treatise1Price.toString).copy(color = ChoiceColor.Positive),
            back))
      _ <- renderer.show(user, screen)
    } yield ()

  /** Купить трактат: серебро — Густаво, книга — в сумку (места не занимает). */
  private def buy(user: User, renderer: Renderer, book: QuestItemKind, price: Long): Task[StateType] =
    for {
      hero <- getHero(user)
      _ <- if (hero.silver < price)
             renderer.show(user, Screen(content.format("gustavo.herbs.noSilver", "price" -> price.toString), Nil))
           else
             heroDao.updateSilver(user.userId, hero.silver - price) *>
               MarisaQuest.give(inventoryRepo, itemRepo, hero, book) *>
               renderer.show(user, Screen(
                 content.text("gustavo.herbs.bought") + "\n" +
                   content.format("marisa.questItemAdded", "item" -> book.displayName), Nil))
      _ <- talk(user, renderer)
    } yield StateType.GustavoHerbs
}
