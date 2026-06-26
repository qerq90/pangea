package pangea.service.state.states.guild

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/**
 * Приём трофеев в Гильдии Искателей. Кнопка «Сдать трофеи» забирает все
 * предметы с `itemType == Trophy` из инвентаря и начисляет за каждый
 * `ceil(5 + lvl × coef)` репутации, где `coef` зависит от вида трофея
 * (Реликвия 4 · Талисман 2 · Голова 1 · Мешок с пожитками 0.5).
 */
case class TrophyExchangeState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "SubmitTrophies"      -> Target.Run { (user, _, renderer) => submit(user, renderer) },
      "LeaveTrophyExchange" -> Target.Goto(StateType.Guild)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.TrophyExchange) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("guild.trophyExchange"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def submit(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero      <- heroDao.getHeroByUserId(user.userId)
                     .flatMap(ZIO.fromOption(_))
                     .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
      inventory <- inventoryRepo.get(hero.id).orElseFail(new Throwable("Inventory unavailable"))
      trophies   = inventory.items.data.filter(i => i.id != 0L && i.itemType == ItemType.Trophy)
      gained     = trophies.map(TrophyExchangeState.reputationFor).sum
      _         <- ZIO.foreachDiscard(trophies)(t => inventoryRepo.removeItem(t.id, hero.id).orElse(ZIO.unit))
      _         <- ZIO.when(gained > 0)(heroDao.updateGuildReputation(user.userId, hero.guildReputation + gained))
      msg        = if (trophies.isEmpty) content.text("guild.noTrophies")
                   else content.format("guild.trophiesSubmitted",
                     "count" -> trophies.length.toString,
                     "gained" -> gained.toString,
                     "total"  -> (hero.guildReputation + gained).toString)
      _         <- renderer.show(user, Screen(msg, Nil))
      _         <- enter(user, renderer)
    } yield StateType.TrophyExchange
}

object TrophyExchangeState {
  // Коэффициенты по виду трофея. Имя trophy формируется LootGenerator как
  // "<категория> (<раса>)" — категория = первое слово до пробела.
  private val coefficients: Map[String, Double] = Map(
    "Реликвия"          -> 4.0,
    "Талисман"          -> 2.0,
    "Голова"            -> 1.0,
    "Мешок с пожитками" -> 0.5
  )

  /** Репутация за один трофей: `ceil(5 + lvl × coef)`. Неизвестный вид → coef = 1. */
  def reputationFor(item: Item): Long = {
    val category = item.name.split(" \\(").headOption.getOrElse(item.name)
    val coef     = coefficients.getOrElse(category, 1.0)
    math.ceil(5.0 + item.lvl.toDouble * coef).toLong
  }
}
