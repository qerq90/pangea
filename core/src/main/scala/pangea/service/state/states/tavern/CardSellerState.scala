package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.TreasureMapGenerator
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * «Подозрительный человек» — продавец карт клада в таверне. Появление катает
 * [[CardSeller]] (раз в час, 25%); здесь — сам диалог: интро → «что это?» → цена →
 * покупка/отказ. Покупка списывает дублоны и выдаёт целую карту по уровню героя,
 * затем ставит 20-часовой кулдаун. Не хватает дублонов — продавец ждёт ещё час.
 */
case class CardSellerState(
  heroDao:        HeroDao,
  inventoryRepo:  InventoryRepository,
  itemRepository: ItemRepository,
  content:        SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "WhatIsIt"      -> Target.Run { (u, _, r) => r.show(u, content.screen("cardSeller.pitch")).as(StateType.CardSeller) },
      "HowMuch"       -> Target.Run { (u, _, r) => showPrice(u, r) },
      "BuyCard"       -> Target.Run { (u, _, r) => buyCard(u, r) },
      "DeclineCard"   -> Target.Goto(StateType.Tavern),
      "NeedDoubloons" -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.CardSeller) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Tavern, StateType.CardSeller)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("cardSeller.intro")).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Экран цены: цена берётся из durable-данных (стабильна на всё присутствие).
  private def showPrice(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      data <- CardSeller.load(heroDao, user.userId)
      res <- if (!data.present(now)) gone(user, renderer)
             else {
               val price   = data.price.getOrElse(CardSellerData.PriceMax)
               val choices = content.screen("cardSeller.price").choices
               renderer.show(user, Screen(
                 content.format("cardSeller.price.text", "price" -> price.toString), choices))
                 .as(StateType.CardSeller)
             }
    } yield res

  private def buyCard(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      data <- CardSeller.load(heroDao, user.userId)
      res <- if (!data.present(now)) gone(user, renderer)
             else {
               val price = data.price.getOrElse(CardSellerData.PriceMax)
               if (hero.doubloons < price) notEnough(user, renderer)
               else finalizePurchase(user, hero, price, now, renderer)
             }
    } yield res

  // Хватает дублонов: сначала кладём карту (переполнено → не покупаем и не списываем,
  // чтобы не потерять дублоны и карту), и только при успехе списываем + ставим кулдаун.
  private def finalizePurchase(user: User, hero: Hero, price: Long, now: Long, renderer: Renderer): Task[StateType] =
    for {
      map       <- ZIO.succeed(TreasureMapGenerator.create(hero.lvl, half = false))
      persisted <- itemRepository.persist(hero.id, map)
      added     <- inventoryRepo.addItem(hero.id, persisted).as(true).catchAll(_ => ZIO.succeed(false))
      res <- if (!added)
               renderer.show(user, Screen(content.text("common.inventoryFull"), Nil)).as(StateType.Tavern)
             else {
               val bought = CardSellerData(offerUntil = None, price = None,
                 nextRollAt = Some(now + CardSellerData.PurchaseCooldownMs))
               heroDao.updateDoubloons(user.userId, hero.doubloons - price) *>
                 heroDao.writeCardSellerData(user.userId, bought.asJson) *>
                 renderer.show(user, Screen(content.format("cardSeller.bought", "name" -> map.name), Nil))
                   .as(StateType.Tavern)
             }
    } yield res

  // Не хватает дублонов: ожидание НЕ продлеваем — продавец ждёт свой исходный час.
  private def notEnough(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, content.screen("cardSeller.notEnough")).as(StateType.CardSeller)

  private def gone(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("cardSeller.gone"), Nil)).as(StateType.Tavern)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
