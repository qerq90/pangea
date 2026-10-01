package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Choice, ChoiceColor, Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.quest.{DailyNpc, DailyRates, DailyTask}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Разговор о сегодняшнем поручении — одинаковый у всех четверых горожан.
  *
  * Кнопка в меню: «есть работа» — пока не взято, «как там моё дело» — пока
  * идёт, «сдать» — когда сделано, и ничего, когда сдано. Что именно просят и
  * чем платят, знает сама сцена: сюда она передаёт только награду.
  *
  * Поручения «принеси» сдаются своей кнопкой и по частям: сколько подходящего
  * нашлось в сумке, столько и уходит горожанину, остальное — в другой заход.
  * Сама собой сумка не пустеет: пока герой не нажал «отдать», вещи при нём.
  *
  * Кнопки именуются `<prefix>Daily`, `<prefix>DailyTake`, `<prefix>DailyGive`,
  * `<prefix>DailyHand`.
  */
final case class DailyDialog(
  heroDao:   HeroDao,
  content:   SceneContent,
  npc:       DailyNpc,
  prefix:    String,
  inventory: Option[InventoryRepository] = None
) {

  val openAction: String = s"${prefix}Daily"
  val takeAction: String = s"${prefix}DailyTake"
  val giveAction: String = s"${prefix}DailyGive"
  val handAction: String = s"${prefix}DailyHand"

  private def npcKey(field: String): String = s"daily.${npc.key}.$field"

  private def taskKey(task: DailyTask, field: String): String =
    s"daily.${npc.key}.tasks.${task.kind.key}.$field"

  def today(hero: Hero, nowMs: Long): Task[DailyTask] =
    DailyQuestLog.todays(heroDao, hero, npc, nowMs)

  /** Кнопка поручения для меню горожанина; сдано — кнопки нет до завтра. */
  def button(task: DailyTask): Option[Choice] =
    if (task.done) None
    else if (task.ready) Some(content.choice(openAction, npcKey("readyLabel")).copy(color = ChoiceColor.Positive))
    else if (task.taken) Some(content.choice(openAction, npcKey("activeLabel")))
    else Some(content.choice(openAction, npcKey("offerLabel")).copy(color = ChoiceColor.Positive))

  /** Экран поручения: рассказ, счёт сделанного и кнопки по состоянию дел. */
  def show(user: User, task: DailyTask, nowMs: Long, renderer: Renderer): Task[Unit] = {
    val body =
      if (task.done) content.text(npcKey("doneToday"))
      else if (task.ready) line(task, "ready")
      else if (task.taken) line(task, "active")
      else line(task, "offer")
    val left = content.format(npcKey("untilNext"), "left" -> hoursLeft(nowMs))
    renderer.show(user, Screen(s"$body\n\n$left", buttons(task) ++ List(back)))
  }

  private def buttons(task: DailyTask): List[Choice] =
    if (task.done) Nil
    else if (task.ready) List(content.choice(handAction, npcKey("hand")).copy(color = ChoiceColor.Positive))
    // «Принеси» сдаётся вручную: герой сам решает, когда расстаться с вещами.
    else if (task.taken) task.bring.map(_ => content.choice(giveAction, npcKey("give"))).toList
    else List(content.choice(takeAction, npcKey("take")).copy(color = ChoiceColor.Positive))

  /** Строка поручения. `{what}` — то, что просят назвать поимённо: чья
    * реликвия, какой камень, какая трава. */
  private def line(task: DailyTask, field: String): String =
    content.format(taskKey(task, field),
      "goal" -> task.kind.goal.toString,
      "have" -> task.progress.toString,
      "what" -> task.what)

  private def back: Choice =
    content.choice(s"${prefix}DailyBack", npcKey("back")).copy(color = ChoiceColor.Negative)

  def take(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[Unit] =
    DailyQuestLog.take(heroDao, hero, npc, nowMs).flatMap(show(user, _, nowMs, renderer))

  /** Отдать то, что просят: сколько нашлось в сумке, столько и уходит, но не
    * больше, чем не хватает. Пусто — так и говорим, вещи при герое. */
  def give(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[Unit] =
    today(hero, nowMs).flatMap { task =>
      if (!task.taken || task.done || task.bring.isEmpty) show(user, task, nowMs, renderer)
      else
        bag(hero).flatMap { items =>
          val goods = task.toGive(items)
          if (goods.isEmpty)
            renderer.show(user, Screen(content.text(npcKey("nothingToGive")), Nil)) *>
              show(user, task, nowMs, renderer)
          else
            surrender(hero, goods) *>
              DailyQuestLog.add(heroDao, user.userId, npc, task.kind, goods.size.toLong) *>
              renderer.show(user, Screen(content.format(npcKey("given"),
                "names" -> goods.map(_.displayTitle).mkString(", ")), Nil)) *>
              today(hero, nowMs).flatMap(show(user, _, nowMs, renderer))
        }
    }

  /** Сдать сделанное: опыт кладём здесь, остальное — на совести сцены.
    * Возвращает опыт и само поручение, чтобы сцена сказала своё слово. */
  def hand(user: User, hero: Hero, nowMs: Long): Task[Option[(DailyTask, Long)]] =
    today(hero, nowMs).flatMap { task =>
      if (!task.ready) ZIO.succeed(None)
      else {
        val exp = DailyRates.exp(hero.lvl)
        val up  = hero.gainExp(exp)
        DailyQuestLog.complete(heroDao, user.userId, npc, task) *>
          heroDao.updateExpAndLevel(user.userId, up.exp, up.lvl, up.upgradePoints)
            .as(Some(task -> exp))
      }
    }

  private def surrender(hero: Hero, goods: List[Item]): Task[Unit] =
    ZIO.foreachDiscard(inventory)(repo =>
      repo.removeItems(goods.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString)))

  private def bag(hero: Hero): Task[List[Item]] =
    inventory.fold[Task[List[Item]]](ZIO.succeed(Nil))(repo =>
      repo.get(hero.id).mapError(e => new Throwable(e.toString))
        .map(_.items.data.filter(_.id != 0L)))

  /** Строка «до новых поручений»: часы и минуты до полуночи по Москве. */
  private def hoursLeft(nowMs: Long): String = {
    val mins = (DailyRates.untilNextDay(nowMs) / 60000L).max(1L)
    if (mins >= 60L) s"${mins / 60L} ч ${mins % 60L} мин" else s"$mins мин"
  }
}
