package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.user.User
import zio.{Task, ZIO}

/** Срок в отряде выходит у всех, только по-разному. Найм наёмника кончается
  * через [[pangea.model.squad.AllyRates.HireMs]]: он уходит с репликой и
  * садится за стол таверны снова через [[pangea.model.squad.AllyRates.OffDutyMs]].
  * Поднятого с алтаря держит тёмная сила, и её хватает на
  * [[pangea.model.squad.AllyRates.UndeadMs]] — потом кости рассыпаются, и
  * ждать его неоткуда.
  *
  * Считается это при следующем визите героя в любое из мест, где отряд на
  * виду. Возвращает героя уже без ушедших. */
object SquadDuty {

  def settle(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[Hero] = {
    // Поднятые до того, как у нежити завёлся срок, получают его с этой минуты.
    val dated         = hero.squad.settleUndead(nowMs)
    val (squad, gone) = dated.expire(nowMs)
    if (gone.isEmpty && dated == hero.squad) ZIO.succeed(hero)
    else
      heroDao.updateSquad(user.userId, squad) *>
        ZIO.foreachDiscard(gone)(a =>
          renderer.show(user, Screen(content.format(leaveKey(a), "name" -> a.name), Nil))) *>
        ZIO.succeed(hero.copy(squad = squad))
  }

  /** Чем прощаются: наёмник отработал день, кости поднятого рассыпались, а крыса
    * просто утекла в щель. */
  private def leaveKey(a: pangea.model.squad.Ally): String = a.kind match {
    case pangea.model.squad.AllyKind.Rat => "squad.ratFled"
    case _ if a.undead.isDefined         => "squad.undeadCrumbled"
    case _                               => "squad.dayOver"
  }
}
