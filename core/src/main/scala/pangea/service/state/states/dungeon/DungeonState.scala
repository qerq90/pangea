package pangea.service.state.states.dungeon

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.ActiveBattle
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DungeonState(heroDao: HeroDao, inventoryRepo: pangea.repository.inventory.InventoryRepository, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "FindEvent"     -> Target.Run { (user, _, renderer) => findEvent(user, renderer) },
      "GoDarker"      -> Target.Run { (user, _, renderer) => changeDungeonLevel(user, renderer, +1) },
      "GoLighter"     -> Target.Run { (user, _, renderer) => changeDungeonLevel(user, renderer, -1) },
      "GoToCity"       -> Target.Goto(StateType.GlobalMap),
      "OpenCharacter"  -> Target.Run { (user, _, _) => CharacterMenu.open(heroDao, user.userId, StateType.Dungeon) },
      "Rest"           -> Target.Goto(StateType.Rest)
    ),
    fallback = Target.Goto(StateType.Dungeon)
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.HeroStats

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _    <- renderer.show(user, enterScreen(hero, now))
    } yield ()

  /** Экран этажа. Раскладка по рядам:
   *   - row 0: «Исследовать уровень» (зелёная);
   *   - row 1: «Двигаться к тьме»/«…к свету» — зелёные если ход открыт, красные если закрыт;
   *   - row 2: «В портал», «Персонаж»;
   *   - row 3: «Отдых» (зелёная). */
  private def enterScreen(hero: Hero, nowMs: Long): Screen = {
    val text       = content.format("dungeon.enter.text",
      "level"    -> hero.dungeonLevel.toString,
      "hp"       -> hero.fightStats.hp.toString,
      "maxHp"    -> hero.effectiveMaxHp(nowMs).toString,
      "armor"    -> hero.fightStats.armor.toString,
      "maxArmor" -> hero.effectiveMaxArmor(nowMs).toString)
    val byId       = content.screen("dungeon.enter").choices.map(c => c.id -> c).toMap
    def mv(id: String, open: Boolean): pangea.engine.Choice =
      byId(id).copy(color = if (open) ChoiceColor.Positive else ChoiceColor.Negative, row = Some(1))
    val choices = List(
      byId("FindEvent").copy(color     = ChoiceColor.Positive, row = Some(0)),
      mv("GoDarker",  hero.canGoDarker),
      mv("GoLighter", hero.canGoLighter),
      byId("GoToCity").copy(row        = Some(2)),
      byId("OpenCharacter").copy(row   = Some(2)),
      byId("Rest").copy(color          = ChoiceColor.Positive, row = Some(3))
    )
    Screen(text, choices)
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def findEvent(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      _      <- renderer.show(user, Screen(content.text("dungeon.findEvent"), Nil))
      idx    <- Random.nextIntBounded(StateType.events.size)
      event   = StateType.events(idx)
      result <- event match {
        case StateType.Battle => startBattle(user, hero)
        case StateType.Spring => healAtSpring(user, hero, now, renderer)
        case other            => ZIO.succeed(other)
      }
    } yield result

  private def healAtSpring(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] = {
    val maxHp    = hero.effectiveMaxHp(nowMs)
    val healed   = (maxHp - hero.fightStats.hp).max(0L)
    val flask    = hero.equipment.flask
    val hasFlask = flask.itemType != pangea.model.item.ItemType.NoItem && flask.maxCharges.isDefined
    for {
      _ <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp))
      _ <- ZIO.when(hasFlask)(heroDao.updateEquipment(user.userId,
             hero.equipment.copy(flask = flask.copy(charges = flask.maxCharges))))
      _ <- inventoryRepo.refillFlasks(hero.id).orElse(ZIO.unit)
      _ <- renderer.show(user, Screen(
             content.format("dungeon.spring",
               "healed"      -> healed.toString,
               "flaskFilled" -> (if (hasFlask) content.text("dungeon.springFlaskFilled") else "")), Nil))
      roll   <- Random.nextIntBetween(1, 101)
      result <- if (roll <= 50)
                  renderer.show(user, Screen(content.text("dungeon.springAmbush"), Nil)) *>
                    startBattle(user, hero)
                else ZIO.succeed(StateType.Dungeon)
    } yield result
  }

  private def startBattle(user: User, hero: Hero): Task[StateType] =
    for {
      seed         <- Random.nextLong
      (monster, _)  = MonsterGenerator.generate(hero.dungeonLevel, Rng(seed))
      slots         = List(hero.equipment.weapon, hero.equipment.chestPlate)
                        .flatMap(it => it.activeSkill.map(s => pangea.model.battle.SkillSlotState(it.id, s)))
      battle        = ActiveBattle.fromMonster(monster).copy(skillSlots = slots)
      _            <- heroDao.writeActiveBattle(user.userId, battle.asJson)
    } yield StateType.Battle

  private def changeDungeonLevel(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      now     <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero    <- getHero(user)
      allowed  = if (delta > 0) hero.canGoDarker else hero.canGoLighter
      _ <- if (!allowed) {
             val msg = if (delta > 0) "dungeon.darknessNotDefeated" else "dungeon.alreadyAtTop"
             renderer.show(user, Screen(content.text(msg), Nil)) *>
               renderer.show(user, enterScreen(hero, now))
           } else {
             val newLevel = math.max(1, math.min(150, hero.dungeonLevel + delta))
             heroDao.updateDungeonLevel(user.userId, newLevel) *>
               renderer.show(user, enterScreen(hero.copy(dungeonLevel = newLevel), now))
           }
    } yield StateType.Dungeon

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
