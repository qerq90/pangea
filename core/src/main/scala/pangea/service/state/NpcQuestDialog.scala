package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Choice, ChoiceColor, Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.User
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Общая часть разговора о задании у любого горожанина: кнопка в его меню
  * (предложить, пока не взято; напомнить, пока идёт; ничего, когда выполнено),
  * экран завязки с «взять/не сейчас», принятие и награда опытом. Что именно
  * проверять на шагах и чем платить сверх опыта — у каждой сцены своё.
  *
  * Кнопки именуются `<prefix>Quest`, `<prefix>QuestAccept`,
  * `<prefix>QuestDecline` — сцена вешает их в свои маршруты. */
final case class NpcQuestDialog(heroDao: HeroDao, content: SceneContent, quest: NpcQuest, prefix: String) {

  val questAction: String  = s"${prefix}Quest"
  val acceptAction: String = s"${prefix}QuestAccept"
  val declineAction: String = s"${prefix}QuestDecline"

  def key(field: String): String = s"npcQuests.${quest.key}.$field"

  def text(field: String): String = content.text(key(field))

  def format(field: String, args: (String, String)*): String = content.format(key(field), args: _*)

  def load(user: User): Task[NpcQuests] = NpcQuestLog.load(heroDao, user.userId)

  /** Кнопка задания для меню NPC; None — задание уже выполнено. */
  def button(quests: NpcQuests): Option[Choice] = {
    val p = quests.of(quest)
    if (p.done) None
    else if (p.taken) Some(content.choice(questAction, key("activeLabel")))
    else Some(content.choice(questAction, key("offerLabel")).copy(color = ChoiceColor.Positive))
  }

  /** Завязка: текст (уже собранный сценой — у Горна он зависит от оружия) и две кнопки. */
  def offer(user: User, renderer: Renderer, intro: String): Task[Unit] =
    renderer.show(user, Screen(intro, List(
      content.choice(acceptAction, key("accept")).copy(color = ChoiceColor.Positive),
      content.choice(declineAction, key("decline")).copy(color = ChoiceColor.Negative)
    )))

  /** Взять задание (если ещё не взято) и ответить одной репликой. */
  def accept(user: User, renderer: Renderer): Task[Unit] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _   <- NpcQuestLog.modify(heroDao, user.userId)(q => if (q.of(quest).taken || q.isDone(quest)) q else q.take(quest, now))
      _   <- renderer.show(user, Screen(text("accepted"), Nil))
    } yield ()

  /** Закрыть задание и выдать опыт. Возвращает героя после опыта и строки про
    * опыт и (если случился) новый уровень — сцена вклеивает их в свою награду. */
  def complete(user: User, hero: Hero, extra: NpcQuests => NpcQuests): Task[(Hero, String)] =
    for {
      _       <- NpcQuestLog.modify(heroDao, user.userId)(q => extra(q.finish(quest)))
      leveled <- NpcQuestLog.grantExp(heroDao, hero)
      expLine  = content.format("npcQuests.common.exp", "exp" -> NpcQuestLog.expReward(hero).toString)
      lvlLine  = if (leveled.lvl > hero.lvl) "\n" + content.format("npcQuests.common.levelUp", "level" -> leveled.lvl.toString) else ""
    } yield (leveled, expLine + lvlLine)
}
