package pangea.service.state.states.events.treasure

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

/**
 * Эффект-нода цепочки боёв: спавнит очередной бой той же расы и сразу уводит в
 * [[StateType.Battle]] (`autoAdvance`). В `scene_data` кладёт «роутинг» добычи —
 * куда вернуться после боя:
 *   - ещё остались бои → снова сюда ([[StateType.TreasureMobsFight]]);
 *   - это был последний бой → к выдаче схрона ([[StateType.TreasureSchron]]).
 * Туда же прокидывается обновлённый [[TreasureMobsChain]] (`remaining - 1`).
 */
case class TreasureMobsFightState(heroDao: HeroDao, content: SceneContent) extends State {

  override def targetStates: Set[StateType] = Set(StateType.Battle)

  override def autoAdvance: Option[StateType] = Some(StateType.Battle)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      raw   <- heroDao.readSceneData(user.userId)
      chain <- ZIO.fromOption(raw.flatMap(_.as[TreasureMobsChain].toOption))
                 .orElseFail(new Throwable(s"No treasure chain for user ${user.userId}"))
      seed  <- Random.nextLong
      (monster, _) = MonsterGenerator.generateOfRace(hero.dungeonLevel, Race.withName(chain.race), Rng(seed))
      afterThis    = chain.remaining - 1
      returnTarget = if (afterThis > 0) StateType.TreasureMobsFight else StateType.TreasureSchron
      routing      = LootData(
                       items       = Nil,
                       golds       = Nil,
                       returnState = Some(returnTarget),
                       eventData   = Some(chain.copy(remaining = afterThis).asJson))
      _ <- heroDao.writeActiveBattle(user.userId, SoloPveBattle.from(monster, hero).asJson)
      _ <- heroDao.writeSceneData(user.userId, routing.asJson)
      _ <- renderer.show(user, Screen(content.text("treasureMobs.nextFight"), Nil))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ZIO.succeed(StateType.Battle)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
