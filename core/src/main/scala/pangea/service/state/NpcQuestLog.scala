package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.model.hero.Hero
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Доступ к сюжетным заданиям героя и общие для всех NPC куски: чтение и
  * запись `heroes.npc_quests`, награда опытом, хуки из чужих сцен (победа в
  * бою, сдача трофеев), которые двигают счётчики, не зная подробностей. */
object NpcQuestLog {

  def load(heroDao: HeroDao, userId: UserId): Task[NpcQuests] =
    heroDao.readNpcQuests(userId).map(_.flatMap(_.as[NpcQuests].toOption).getOrElse(NpcQuests.empty))

  def save(heroDao: HeroDao, userId: UserId, quests: NpcQuests): Task[Unit] =
    heroDao.writeNpcQuests(userId, quests.asJson)

  def modify(heroDao: HeroDao, userId: UserId)(f: NpcQuests => NpcQuests): Task[NpcQuests] =
    load(heroDao, userId).flatMap { q =>
      val next = f(q)
      if (next == q) ZIO.succeed(q) else save(heroDao, userId, next).as(next)
    }

  /** Опыт за задание: как за пять боёв на текущем этаже. */
  def expReward(hero: Hero): Long = NpcQuest.ExpPerDungeonLevel * hero.dungeonLevel.toLong.max(1L)

  /** Начислить опыт за задание и записать уровень. Возвращает героя после. */
  def grantExp(heroDao: HeroDao, hero: Hero): Task[Hero] = {
    val leveled = hero.gainExp(expReward(hero))
    heroDao.updateExpAndLevel(hero.userId, leveled.exp, leveled.lvl, leveled.upgradePoints).as(leveled)
  }

  /** Победа в бою: Густаво считает павших, пока действует его зелье. `slain` —
    * сколько мобов легло (в группе — все разом). Ничего не читает, если задание
    * не на том шаге, где это важно. */
  def onVictory(heroDao: HeroDao, userId: UserId, slain: Int, potionActive: Boolean): Task[Unit] =
    modify(heroDao, userId) { q =>
      if (!q.onStep(NpcQuest.Gustavo, 2) || !potionActive) q
      else q.update(NpcQuest.Gustavo) { p =>
        val counted = p.counter + slain.toLong
        if (counted >= NpcQuest.GustavoVictoriesGoal) p.copy(step = 3, counter = counted)
        else p.copy(counter = counted)
      }
    }.unit

  /** Сдача трофеев: Горн считает заработанную репутацию с момента, как задание
    * взято. Потраченная не вычитается — считается заработок, а не остаток. */
  def onReputation(heroDao: HeroDao, userId: UserId, gained: Long): Task[Unit] =
    modify(heroDao, userId) { q =>
      if (!q.onStep(NpcQuest.Horn, 1) || gained <= 0L) q
      else q.update(NpcQuest.Horn) { p =>
        val counted = p.counter + gained
        if (counted >= NpcQuest.HornReputationGoal) p.copy(step = 2, counter = counted)
        else p.copy(counter = counted)
      }
    }.unit
}
