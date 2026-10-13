package pangea.service.activity

import pangea.dao.activity.{ActivityDao, ActivityScore}
import pangea.dao.hero.HeroDao
import pangea.engine.{Players, SceneContent}
import pangea.model.hero.{AzatState, Hero}
import pangea.model.user.{User, VkId}
import pangea.repository.user.UserRepository
import zio.{Task, ZIO, ZLayer}

/** Конкурс активности в группе ВК. Платы за каждый лайк нет — ВК не любит
  * «мотивированную активность», — зато лайки и комментарии под свежими постами
  * копятся баллами, а в конце недели лучшие получают дублоны.
  *
  * Неделя — московская, с понедельника 00:00. Баллы идут только игрокам с
  * героем и только за посты, вышедшие не раньше недели назад: старую ленту
  * пролайкать ради баллов не выйдет. Лайк и комментарий засчитываются по
  * одному на пост. */
final case class ActivityContest(
  dao:      ActivityDao,
  userRepo: UserRepository,
  heroDao:  HeroDao,
  players:  Players,
  content:  SceneContent
) {
  import ActivityContest._

  /** Событие группы: пост запоминаем, лайк и комментарий засчитываем. */
  def handle(event: GroupEvent, now: Long): Task[Unit] = event match {
    case GroupEvent.PostPublished(post, at) => dao.recordPost(post, at)
    case GroupEvent.Liked(vk, post)         => count(vk, post, Kind.Like, now)
    case GroupEvent.Commented(vk, post)     => count(vk, post, Kind.Comment, now)
  }

  private def count(vk: Long, post: Long, kind: Kind, now: Long): Task[Unit] =
    dao.postedAt(post).flatMap {
      case Some(at) if now - at <= PostWindowMs =>
        player(VkId(vk.toString)).flatMap {
          case Some((user, _)) => dao.recordAction(user.userId, post, kind.key, kind.points, weekStart(now), now).unit
          case None            => ZIO.unit
        }
      // Пост старый или мы его не видели (вышел до того, как бот стал их
      // запоминать) — баллов за него нет.
      case _ => ZIO.unit
    }

  /** Игрок с героем по его странице ВК. */
  private def player(vk: VkId): Task[Option[(User, Hero)]] =
    userRepo.getUserByVkId(vk).flatMap {
      case Some(user) => heroDao.getHeroByUserId(user.userId).map(_.map(user -> _))
      case None       => ZIO.none
    }

  /** Подвести итоги прошедшей недели, если их ещё не подводили: лучшим —
    * дублоны в отложенные выплаты (дойдут с ближайшим действием, где бы игрок ни
    * был), в беседу — итоги, каждому победителю — письмо. Зовётся раз в минуту:
    * подведённая неделя повторно ничего не платит. */
  def settle(now: Long): Task[Unit] = {
    val week = weekStart(now) - WeekMs
    dao.awarded(week).flatMap {
      case true  => ZIO.unit
      case false =>
        for {
          top     <- dao.top(week, Winners)
          found   <- ZIO.foreach(top)(score => byUserId(score).map(_.map { case (u, h) => (score, u, h) }))
          winners  = found.flatten
          paid    <- dao.award(week, winners.map { case (_, _, hero) => hero.id -> Prize }, now)
          _       <- ZIO.when(paid && winners.nonEmpty)(congratulate(winners))
        } yield ()
    }
  }

  private def byUserId(score: ActivityScore): Task[Option[(User, Hero)]] =
    for {
      user <- userRepo.getUserById(score.userId).option.map(_.flatten)
      hero <- heroDao.getHeroByUserId(score.userId)
    } yield for { u <- user; h <- hero } yield (u, h)

  private def congratulate(winners: List[(ActivityScore, User, Hero)]): Task[Unit] =
    for {
      named <- ZIO.foreach(winners) { case (score, user, _) => nameOf(user).map(name => (score, user, name)) }
      lines  = named.zipWithIndex.map { case ((score, user, name), i) =>
                 content.format("activity.resultLine", "place" -> (i + 1).toString,
                   "name" -> mention(user, name), "points" -> score.points.toString)
               }
      _     <- players.announce(content.format("activity.results",
                 "lines" -> lines.mkString("\n"), "prize" -> Prize.toString)).ignore
      _     <- ZIO.foreachDiscard(named.zipWithIndex) { case ((score, user, _), i) =>
                 players.notify(user, content.format("activity.youWon", "place" -> (i + 1).toString,
                   "points" -> score.points.toString, "prize" -> Prize.toString)).ignore
               }
    } yield ()

  /** Таблица текущей недели — для команды «Рейтинг» в беседе. Имена без
    * упоминаний: таблицу спрашивают часто, и звать пятерых лучших каждый раз
    * незачем. */
  def board(now: Long): Task[String] =
    for {
      top   <- dao.top(weekStart(now), Winners)
      named <- ZIO.foreach(top)(score => byUserId(score).flatMap {
                 case Some((user, _)) => nameOf(user).map(name => Some(score -> name))
                 case None            => ZIO.none
               })
      lines  = named.flatten.zipWithIndex.map { case ((score, name), i) =>
                 content.format("activity.resultLine", "place" -> (i + 1).toString,
                   "name" -> name, "points" -> score.points.toString)
               }
      left   = timeLeft(now)
    } yield
      if (lines.isEmpty) content.format("activity.boardEmpty", "left" -> left, "prize" -> Prize.toString)
      else content.format("activity.board", "lines" -> lines.mkString("\n"), "left" -> left, "prize" -> Prize.toString)

  private def nameOf(user: User): Task[String] =
    players.getDisplayName(user).orElse(ZIO.succeed(content.text("activity.someone")))

  /** Кликабельное упоминание игрока в ВК. */
  private def mention(user: User, name: String): String = s"[id${user.vkId.value}|$name]"
}

object ActivityContest {

  /** За что дают баллы и сколько. */
  sealed abstract class Kind(val key: String, val points: Long)
  object Kind {
    case object Like    extends Kind("like", 1L)
    case object Comment extends Kind("comment", 2L)
  }

  /** Сколько лучших награждаем по итогам недели и чем. */
  val Winners: Int = 5
  val Prize: Long  = 100L

  private val DayMs: Long = 24L * 60L * 60L * 1000L
  val WeekMs: Long        = 7L * DayMs

  /** Баллы идут за посты не старше этого: свежая лента, а не архив. */
  val PostWindowMs: Long = WeekMs

  /** Начало недели (мс): понедельник, 00:00 по Москве — как и прочие сутки игры
    * (см. [[AzatState.midnightsBetween]]). */
  def weekStart(nowMs: Long): Long = {
    val day    = Math.floorDiv(nowMs + AzatState.MoscowOffsetMs, DayMs)
    // 1 января 1970-го — четверг: понедельник той же недели на три дня раньше.
    val monday = day - Math.floorMod(day + 3L, 7L)
    monday * DayMs - AzatState.MoscowOffsetMs
  }

  /** Сколько осталось до конца недели: «2д 5ч». */
  def timeLeft(nowMs: Long): String = {
    val secs  = ((weekStart(nowMs) + WeekMs - nowMs) / 1000L).max(0L)
    s"${secs / 86400L}д ${(secs % 86400L) / 3600L}ч"
  }

  val live: ZLayer[ActivityDao with UserRepository with HeroDao with Players with SceneContent, Nothing, ActivityContest] =
    ZLayer.fromFunction(ActivityContest(_, _, _, _, _))
}
