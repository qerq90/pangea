package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.battle.ActiveBattle
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
                        .flatMap(_.as[ActiveBattle].toOption)
                        .map(_.toMonster.name)
                        .getOrElse("Монстр")
      expLost       = (hero.exp * 0.1).toLong.max(0L)
      newExp        = (hero.exp - expLost).max(0L)
      goldLost      = hero.gold / 2
      newGold       = hero.gold - goldLost

      // traumas that are still active are kept; expired ones reset to empty list
      existingNames = if (hero.traumaActive(now)) hero.traumaNames else Nil

      // pick the right tier: light → medium → heavy (progression)
      hasAllLight   = Trauma.light.forall(t => existingNames.contains(t.name))
      hasAllMedium  = Trauma.medium.forall(t => existingNames.contains(t.name))
      pool          = if (!hasAllLight)
                        Trauma.light.filterNot(t => existingNames.contains(t.name))
                      else if (!hasAllMedium)
                        Trauma.medium.filterNot(t => existingNames.contains(t.name))
                      else {
                        val remaining = Trauma.heavy.filterNot(t => existingNames.contains(t.name))
                        if (remaining.isEmpty) Trauma.heavy else remaining
                      }

      traumaIdx    <- Random.nextIntBetween(0, pool.length)
      trauma        = pool(traumaIdx)
      traumaUntil   = now + 8L * 3600 * 1000
      newTraumaNames = existingNames :+ trauma.name

      _            <- heroDao.updateExpAndLevel(user.userId, newExp, hero.lvl, hero.upgradePoints)
      _            <- heroDao.updateGold(user.userId, newGold)
      _            <- heroDao.updateTrauma(user.userId, Some(traumaUntil), newTraumaNames)
      _            <- heroDao.clearActiveBattle(user.userId)
      _            <- renderer.show(user, Screen(
                        content.format("death.penalty",
                          "expLost"  -> expLost.toString,
                          "goldLost" -> goldLost.toString), Nil))
      _            <- renderer.show(user, Screen(
                        content.format("death.trauma",
                          "traumaName"  -> trauma.name,
                          "description" -> trauma.description), Nil))

      _            <- dropItems(user, hero.id, monsterName, renderer)
      deathRestMs   = hero.dungeonLevel.toLong * 2L * 60L * 1000L
      _            <- heroDao.writeSceneData(user.userId, Json.obj("restDurationMs" -> deathRestMs.asJson))
    } yield ()

  // Unused in the normal flow (Death routes onward via autoAdvance). Kept as a
  // safe fallback so a hero somehow sitting in Death still moves to Rest.
  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ZIO.succeed(StateType.Rest)

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
