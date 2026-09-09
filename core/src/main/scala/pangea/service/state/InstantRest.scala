package pangea.service.state

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.schedule.TaskKind
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import zio.{Task, ZIO}

/**
 * Мгновенный отдых благословения Азата: восстанавливает HP, броню и энергию
 * сразу, тратя один заряд из `azat.instantRests`.
 *
 * Живёт отдельно от [[pangea.service.state.states.RestState]], потому что нужен
 * ещё и лабиринту: когда заряды есть, кнопка «Отдых» тратит их СРАЗУ, не уводя
 * героя в ожидание у костра.
 */
object InstantRest {

  /** Потратить заряд и восстановить героя. Возвращает остаток зарядов, либо None,
    * если тратить было нечего (тогда вызывающий сам решает, что делать). */
  def use(
      heroDao: HeroDao,
      scheduler: Scheduler,
      content: SceneContent,
      user: User,
      nowMs: Long,
      renderer: Renderer
  ): Task[Option[Int]] =
    for {
      azat <- AzatData.load(heroDao, user.userId, nowMs)
      res <-
        if (azat.instantRests <= 0) ZIO.succeed(Option.empty[Int])
        else
          heroDao.getHeroByUserId(user.userId)
            .flatMap(ZIO.fromOption(_))
            .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
            .flatMap { hero =>
              val left     = azat.instantRests - 1
              val maxHp    = hero.effectiveMaxHp(nowMs)
              val maxArmor = hero.effectiveMaxArmor(nowMs)
              val maxEn    = hero.maxEnergy(nowMs)
              heroDao.updateFightStats(user.userId,
                hero.fightStats.copy(hp = maxHp, armor = maxArmor, energy = maxEn)) *>
                AzatData.save(heroDao, user.userId, azat.copy(instantRests = left)) *>
                // Снимаем возможное пробуждение и чистим сцену отдыха: герой уже бодр.
                scheduler.cancel(user.userId, TaskKind.Revive) *>
                heroDao.writeSceneData(user.userId, Json.Null) *>
                renderer.show(user, Screen(
                  content.format("rest.instantDone", "n" -> left.toString), Nil)).as(Some(left))
            }
    } yield res
}
