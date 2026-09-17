package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.user.User
import zio.{Task, ZIO}

/** Найм союзника кончается через [[pangea.model.squad.AllyRates.HireMs]]: при
  * следующем визите героя в любое из мест, где отряд на виду, отработавшие
  * уходят из отряда с репликой и садятся за стол таверны снова через
  * [[pangea.model.squad.AllyRates.OffDutyMs]]. Возвращает героя уже без них. */
object SquadDuty {

  def settle(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[Hero] = {
    val (squad, gone) = hero.squad.expire(nowMs)
    if (gone.isEmpty) ZIO.succeed(hero)
    else
      heroDao.updateSquad(user.userId, squad) *>
        ZIO.foreachDiscard(gone)(k =>
          renderer.show(user, Screen(content.format("squad.dayOver", "name" -> k.name), Nil))) *>
        ZIO.succeed(hero.copy(squad = squad))
  }
}
