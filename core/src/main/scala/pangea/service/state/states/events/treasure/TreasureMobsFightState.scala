package pangea.service.state.states.events.treasure

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

/**
 * Эффект-нода: мобы, выкопавшие сокровище, нападают все разом — один групповой
 * бой на `remaining` мобов одной расы, и сразу в [[StateType.Battle]]
 * (`autoAdvance`). В `scene_data` кладёт «роутинг» добычи: после победы — к
 * выдаче схрона ([[StateType.TreasureSchron]]), туда же прокидывается
 * [[TreasureMobsChain]] с диапазоном дублонов.
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
      race   = Race.withName(chain.race)
      count  = chain.remaining.max(1)
      // Каждому мобу — свой бросок генерации и своя стартовая энергия.
      monsters <- ZIO.foreach(List.fill(count)(()))(_ =>
                    Random.nextLong.map(seed => MonsterGenerator.generateOfRace(hero.dungeonLevel, race, Rng(seed))._1))
      energies <- ZIO.foreach(monsters)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      routing  = LootData(
                   items       = Nil,
                   silvers     = Nil,
                   returnState = Some(StateType.TreasureSchron),
                   eventData   = Some(chain.copy(remaining = 0).asJson))
      _ <- heroDao.writeActiveBattle(user.userId, SoloPveBattle.fromGroup(monsters, hero, energies).asJson)
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
