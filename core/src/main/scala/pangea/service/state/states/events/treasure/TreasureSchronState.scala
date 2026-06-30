package pangea.service.state.states.events.treasure

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.loot.SchronGenerator
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

/**
 * Эффект-нода: после победы во всей цепочке боёв — «Вы нашли уже заботливо
 * выкопанный схрон». Катаем добычу схрона ([[SchronGenerator]], раса = раса
 * мобов цепочки, 2–3 дублона) и уводим на общий экран добычи (`autoAdvance` →
 * [[StateType.Loot]]), который её и выдаёт.
 */
case class TreasureSchronState(heroDao: HeroDao, content: SceneContent) extends State {

  override def targetStates: Set[StateType] = Set(StateType.Loot)

  override def autoAdvance: Option[StateType] = Some(StateType.Loot)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      raw   <- heroDao.readSceneData(user.userId)
      chain <- ZIO.fromOption(raw.flatMap(_.as[TreasureMobsChain].toOption))
                 .orElseFail(new Throwable(s"No treasure chain for user ${user.userId}"))
      seed  <- Random.nextLong
      (reward, _) = SchronGenerator.roll(
                      Race.withName(chain.race), hero.dungeonLevel.toLong,
                      chain.doubloonMin, chain.doubloonMax, Rng(seed))
      loot  = LootData(
                items     = reward.items,
                golds     = if (reward.gold > 0L) List(reward.gold) else Nil,
                doubloons = reward.doubloons)
      _ <- renderer.show(user, Screen(content.text("treasureMobs.schron"), Nil))
      _ <- heroDao.writeSceneData(user.userId, loot.asJson)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ZIO.succeed(StateType.Loot)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
