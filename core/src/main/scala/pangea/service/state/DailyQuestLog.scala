package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.model.hero.Hero
import pangea.model.quest._
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Ежедневные поручения горожан: у Рахадима, Ришелье, Горна и Густаво.
  *
  * У каждого свой набор, на сутки выпадает одно — какое, решает бросок в тот
  * день, когда герой к нему зашёл. Сутки считаются по Москве
  * ([[DailyRates.dayOf]]), так что к полуночи поручения меняются у всех разом,
  * а не через двадцать четыре часа с чьей-то сдачи.
  *
  * Считается прогресс тремя путями. Что видно по самому герою — убитые,
  * репутация — сверяется со снимком, взятым при выдаче поручения. Городские
  * дела прибавляются там, где они и делаются: у прилавка, у стойки, в кузне. А
  * поручения «принеси» ([[pangea.model.quest.DailyBring]]) не копят ничего:
  * у них прогресс считает [[DailyDialog]] прямо по сумке.
  */
object DailyQuestLog {

  def load(heroDao: HeroDao, userId: UserId): Task[DailyQuests] =
    heroDao.readDailyQuests(userId).map(_.flatMap(_.as[DailyQuests].toOption).getOrElse(DailyQuests.empty))

  def save(heroDao: HeroDao, userId: UserId, quests: DailyQuests): Task[Unit] =
    heroDao.writeDailyQuests(userId, quests.asJson)

  /** Сегодняшнее поручение горожанина. Нет или вчерашнее — катаем новое:
    * герой узнаёт о нём, только когда сам зайдёт. */
  def todays(heroDao: HeroDao, hero: Hero, npc: DailyNpc, nowMs: Long): Task[DailyTask] = {
    val day = DailyRates.dayOf(nowMs)
    load(heroDao, hero.userId).flatMap { all =>
      all.today(npc, day) match {
        case Some(task) => ZIO.succeed(refreshed(task, hero))
        case None =>
          val pool = DailyKind.of(npc)
          val kind = weighted(pool, pickFor(day * 1000003L + hero.id.value, pool.map(_.weight).sum))
          val task = DailyTask(kind, day, from = counterOf(kind, hero), pick = pickOf(kind, day, hero))
          save(heroDao, hero.userId, all.updated(npc, task)).as(task)
      }
    }
  }

  /** Взять поручение: со счётчиковых снимаем мерку прямо сейчас, чтобы вчерашние
    * подвиги не засчитались. */
  def take(heroDao: HeroDao, hero: Hero, npc: DailyNpc, nowMs: Long): Task[DailyTask] =
    todays(heroDao, hero, npc, nowMs).flatMap { task =>
      if (task.taken) ZIO.succeed(task)
      else {
        val started = task.copy(taken = true, from = counterOf(task.kind, hero), count = 0L)
        update(heroDao, hero.userId, npc, started).as(started)
      }
    }

  /** Прибавить сделанное. Поручение не взято или уже сдано — прибавлять нечего:
    * так у героя не копится прогресс по делам, за которые он не брался. */
  def add(heroDao: HeroDao, userId: UserId, npc: DailyNpc, kind: DailyKind, n: Long): Task[Unit] =
    if (n <= 0L) ZIO.unit
    else
      load(heroDao, userId).flatMap { all =>
        all.of(npc) match {
          case Some(task) if task.taken && !task.done && task.kind == kind =>
            save(heroDao, userId, all.updated(npc, task.plus(n)))
          case _ => ZIO.unit
        }
      }

  /** Сдать: отмечаем выполненным. Награду выдаёт сама сцена — у каждого
    * горожанина она своя. */
  def complete(heroDao: HeroDao, userId: UserId, npc: DailyNpc, task: DailyTask): Task[Unit] =
    load(heroDao, userId).flatMap(all =>
      save(heroDao, userId, all.updated(npc, task.copy(done = true, count = task.kind.goal))))

  private def update(heroDao: HeroDao, userId: UserId, npc: DailyNpc, task: DailyTask): Task[Unit] =
    load(heroDao, userId).flatMap(all => save(heroDao, userId, all.updated(npc, task)))

  /** Счётчиковое задание подтягивает прогресс из самого героя. */
  private def refreshed(task: DailyTask, hero: Hero): DailyTask =
    if (task.kind.snap && task.taken) task.withCounter(counterOf(task.kind, hero)) else task

  /** По какому счётчику героя считается это поручение. */
  private def counterOf(kind: DailyKind, hero: Hero): Long = kind match {
    case DailyKind.HornKills      => hero.kills
    case DailyKind.HornReputation => hero.guildReputation
    case _                        => 0L
  }

  /** Уточнение к поручению «принеси»: чья реликвия, какой камень, какая трава.
    * Ставка своя, не та, что у выбора самого поручения, — иначе камень ходил бы
    * следом за днём одной и той же парой. */
  private def pickOf(kind: DailyKind, day: Long, hero: Hero): Option[String] = kind match {
    case b: DailyBring if b.picks.nonEmpty =>
      val opts = b.picks
      Some(opts(pickFor(day * 7919L + hero.id.value * 31L, opts.size)))
    case _ => None
  }

  /** Какое поручение пришлось на выпавшее число. Поручения не равны: у кого
    * вес больше, тот и занимает больше дней ([[DailyKind.weight]]). */
  @annotation.tailrec
  private def weighted(pool: List[DailyKind], roll: Int): DailyKind = pool match {
    case kind :: Nil                      => kind
    case kind :: _ if roll < kind.weight  => kind
    case kind :: rest                     => weighted(rest, roll - kind.weight)
    case Nil                              => DailyKind.BankLot // пул пуст не бывает
  }

  /** Какое число выпало сегодня. Не бросок, а счёт от дня и самого героя:
    * у каждого своё, назавтра другое, а бой и прочие сцены со своими бросками
    * при этом не сбиваются — заглянуть к горожанину можно когда угодно. */
  private def pickFor(seed: Long, bound: Int): Int = {
    val mixed = Rng(seed).nextLong._1
    (((mixed >>> 17) % bound.toLong).toInt + bound) % bound
  }
}
