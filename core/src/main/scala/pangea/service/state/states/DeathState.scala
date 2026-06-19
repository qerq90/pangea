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
import pangea.service.state.states.battle.BattleState
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DeathState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  override def targetStates: Set[StateType] = Set(StateType.Rest)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("death.enter"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
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
      newLevel      = BattleState.computeLevel(newExp)
      levelDiff     = (hero.lvl - newLevel).max(0L)
      newUpgrade    = (hero.upgradePoints - levelDiff * 4).max(0L)
      goldLost      = hero.gold / 2
      newGold       = hero.gold - goldLost
      traumaUntil   = now + 8L * 3600 * 1000
      traumaIdx    <- Random.nextIntBetween(0, Trauma.all.length)
      trauma        = Trauma.all(traumaIdx)

      _            <- heroDao.updateExpAndLevel(user.userId, newExp, newLevel, newUpgrade)
      _            <- heroDao.updateGold(user.userId, newGold)
      _            <- heroDao.updateTrauma(user.userId, Some(traumaUntil), Some(trauma.name))
      _            <- heroDao.clearActiveBattle(user.userId)
      _            <- renderer.show(user, Screen(
                        content.format("death.penalty",
                          "expLost"  -> expLost.toString,
                          "goldLost" -> goldLost.toString), Nil))
      _            <- renderer.show(user, Screen(
                        content.format("death.trauma",
                          "traumaName" -> trauma.name,
                          "penalty"    -> s"${((1.0 - trauma.modifier) * 100).toInt}%"), Nil))

      _            <- dropItems(user, hero.id, monsterName, renderer)
      deathRestMs   = hero.dungeonLevel.toLong * 2L * 60L * 1000L
      _            <- heroDao.writeSceneData(user.userId, Json.obj("restDurationMs" -> deathRestMs.asJson))
    } yield StateType.Rest

  // Each unequipped inventory item has a 25% independent drop chance on death
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
