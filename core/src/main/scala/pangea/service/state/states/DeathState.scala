package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.battle.SoloPveBattle
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.model.trauma.Trauma
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DeathState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  override def targetStates: Set[StateType] = Set(StateType.Rest)

  // Death is an effect node, not a screen: entering it runs the full death event
  // (penalties, trauma, drops) immediately — no "continue" button — then routes
  // straight to Rest via autoAdvance.
  override def autoAdvance: Option[StateType] = Some(StateType.Rest)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now          <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero         <- heroDao.getHeroByUserId(user.userId)
                        .flatMap(ZIO.fromOption(_))
                        .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
      battleJson   <- heroDao.readActiveBattle(user.userId)
      monsterName   = battleJson
                        .flatMap(_.as[SoloPveBattle].toOption)
                        .map(_.toMonster.name)
                        .getOrElse("Монстр")
      expLost       = (hero.exp * 0.1).toLong.max(0L)
      newExp        = (hero.exp - expLost).max(0L)
      goldLost      = hero.gold / 2
      newGold       = hero.gold - goldLost

      // traumas that are still active are kept; expired ones reset to empty list
      existingNames = if (hero.traumaActive(now)) hero.traumaNames else Nil

      // pick the right tier: light → medium → heavy (progression); empty pool
      // means every trauma is already collected (max reached)
      hasAllLight   = Trauma.light.forall(t => existingNames.contains(t.name))
      hasAllMedium  = Trauma.medium.forall(t => existingNames.contains(t.name))
      pool          = if (!hasAllLight)
                        Trauma.light.filterNot(t => existingNames.contains(t.name))
                      else if (!hasAllMedium)
                        Trauma.medium.filterNot(t => existingNames.contains(t.name))
                      else
                        Trauma.heavy.filterNot(t => existingNames.contains(t.name))

      traumaUntil   = now + 8L * 3600 * 1000

      _            <- heroDao.updateExpAndLevel(user.userId, newExp, hero.lvl, hero.upgradePoints)
      _            <- heroDao.updateGold(user.userId, newGold)
      _            <- heroDao.clearActiveBattle(user.userId)
      // Первое сообщение после смерти — сразу убираем боевую клавиатуру, чтобы
      // не висела поверх «обморока».
      _            <- renderer.show(user, Screen(
                        content.format("death.penalty",
                          "expLost"  -> expLost.toString,
                          "goldLost" -> goldLost.toString), Nil, hideKeyboard = true))
      _            <- applyTrauma(user, existingNames, pool, traumaUntil, renderer)

      _            <- dropItems(user, hero.id, monsterName, renderer)
      // Время в мёртвом режиме растёт с уровнем героя и асимптотически
      // стремится к 90 минутам, не достигая их:
      //   deathTimeMinutes = 90 - 89 * exp(-k * (level - 1))
      // На 1 уровне == 1 минута; k — коэффициент роста.
      deathRestMinutes = 90.0 - 89.0 * math.exp(-DeathState.RestGrowthK * (hero.lvl - 1L))
      deathRestMs      = (deathRestMinutes * 60000.0).toLong
      _            <- heroDao.writeSceneData(user.userId, Json.obj(
                        "restDurationMs" -> deathRestMs.asJson,
                        "postDeath"      -> true.asJson))
    } yield ()

  // Unused in the normal flow (Death routes onward via autoAdvance). Kept as a
  // safe fallback so a hero somehow sitting in Death still moves to Rest.
  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ZIO.succeed(StateType.Rest)

  // Даёт новую травму из текущего тира; если пул пуст (все травмы уже собраны —
  // максимум), новую не выдаёт, лишь продлевает таймер снятия на 8 часов и
  // показывает соответствующее сообщение.
  private def applyTrauma(
    user:          User,
    existingNames: List[String],
    pool:          Seq[Trauma],
    traumaUntil:   Long,
    renderer:      Renderer
  ): Task[Unit] =
    if (pool.isEmpty)
      heroDao.updateTrauma(user.userId, Some(traumaUntil), existingNames) *>
        renderer.show(user, Screen(content.text("death.traumaMax"), Nil))
    else
      for {
        idx   <- Random.nextIntBetween(0, pool.length)
        trauma = pool(idx)
        _     <- heroDao.updateTrauma(user.userId, Some(traumaUntil), existingNames :+ trauma.name)
        _     <- renderer.show(user, Screen(
                   content.format("death.trauma",
                     "traumaName"  -> trauma.name,
                     "description" -> trauma.description), Nil))
      } yield ()

  private def dropItems(user: User, heroId: pangea.model.hero.HeroId, monsterName: String, renderer: Renderer): Task[Unit] =
    for {
      inventory <- inventoryRepo.get(heroId).orElse(ZIO.succeed(
                     pangea.model.inventory.Inventory(0L, heroId, 0L,
                       pangea.model.inventory.Inventory.Items(Nil))))
      realItems  = inventory.items.data.filter(_.id != 0L)
      _         <- ZIO.foreachDiscard(realItems) { item =>
                     Random.nextIntBetween(0, 4).flatMap { roll =>
                       ZIO.when(roll == 0) {
                         inventoryRepo.removeItem(item.id, heroId).orElse(ZIO.unit) *>
                           renderer.show(user, Screen(
                             content.format("death.itemDropped",
                               "monsterName" -> monsterName,
                               "itemName"    -> item.name), Nil))
                       }
                     }
                   }
    } yield ()
}

object DeathState {
  // Коэффициент роста времени мёртвого режима по уровню героя (см. формулу в enter).
  val RestGrowthK: Double = 0.01
}
