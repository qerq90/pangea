package pangea.service.activity

import java.time.{ZoneId, ZonedDateTime}
import pangea.engine.SceneContent
import pangea.model.hero.HeroId
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.chat.ChatCommand
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler}
import pangea.test._
import zio.ZIO
import zio.test._

/** Конкурс активности: события группы, баллы недели, итоги и награды. */
object ActivityContestSpec extends ZIOSpecDefault {

  private val moscow = ZoneId.of("Europe/Moscow")
  private def msk(y: Int, mo: Int, d: Int, h: Int, mi: Int = 0): Long =
    ZonedDateTime.of(y, mo, d, h, mi, 0, 0, moscow).toInstant.toEpochMilli

  // Вторник, 13 октября 2026-го, 15:00 по Москве.
  private val now  = msk(2026, 10, 13, 15)
  private val hour = 60L * 60L * 1000L
  private val day  = 24L * hour

  private def user(n: Int): User = User(UserId(n.toLong), VkId((100 + n).toString), TelegramId(s"tg$n"))

  /** Конкурс с игроками 1..`players` (у каждого герой с тем же номером); у игрока
    * 9 аккаунт есть, а героя нет. */
  private def contest(players: Int = 3) =
    for {
      heroDao  <- TestHeroDao.make
      _        <- ZIO.foreachDiscard(1 to players)(n =>
                    heroDao.insertHero(TestFixtures.hero(UserId(n.toLong)).copy(id = HeroId(n.toLong))))
      userRepo <- TestUserRepository.withUsers(((1 to players).map(user) :+ user(9)): _*)
      content  <- ZIO.attempt(SceneContent.load())
      payouts   = TestPayoutDao.empty
      dao       = new TestActivityDao(payouts)
      players_  = new TestPlayers
    } yield (ActivityContest(dao, userRepo, heroDao, players_, content), dao, payouts, players_)

  private def json(s: String) = io.circe.parser.parse(s).toOption.get

  private def post(id: Long, at: Long)   = GroupEvent.PostPublished(id, at)
  private def like(n: Int, post: Long)    = GroupEvent.Liked(100L + n, post)
  private def comment(n: Int, post: Long) = GroupEvent.Commented(100L + n, post)

  override def spec = suite("Конкурс активности")(

    test("неделя — с понедельника 00:00 по Москве") {
      val monday = msk(2026, 10, 12, 0)
      assertTrue(ActivityContest.weekStart(now) == monday) &&
      assertTrue(ActivityContest.weekStart(monday) == monday) &&
      // воскресенье 23:59 — ещё прошлая неделя
      assertTrue(ActivityContest.weekStart(msk(2026, 10, 11, 23, 59)) == msk(2026, 10, 5, 0)) &&
      assertTrue(ActivityContest.timeLeft(now) == "5д 9ч")
    },

    test("разбор событий группы: пост, лайк на посте, комментарий игрока — и только они") {
      val published = json("""{"type":"wall_post_new","object":{"id":28,"date":1760000000,"post_type":"post","owner_id":-1},"secret":"s"}""")
      val postponed = json("""{"type":"wall_post_new","object":{"id":29,"date":1760000000,"post_type":"postpone"}}""")
      val liked     = json("""{"type":"like_add","object":{"liker_id":5,"object_type":"post","object_owner_id":-1,"object_id":28}}""")
      val onPhoto   = json("""{"type":"like_add","object":{"liker_id":5,"object_type":"photo","object_id":28}}""")
      val replied   = json("""{"type":"wall_reply_new","object":{"id":3,"from_id":7,"post_id":28,"owner_id":-1,"date":1}}""")
      val byGroup   = json("""{"type":"wall_reply_new","object":{"id":4,"from_id":-1,"post_id":28}}""")
      val other     = json("""{"type":"group_join","object":{"user_id":5}}""")
      assertTrue(GroupEvent.parse(published).contains(GroupEvent.PostPublished(28L, 1760000000000L))) &&
      assertTrue(GroupEvent.secretOf(published).contains("s")) &&
      assertTrue(GroupEvent.parse(postponed).isEmpty) &&
      assertTrue(GroupEvent.parse(liked).contains(GroupEvent.Liked(5L, 28L))) &&
      assertTrue(GroupEvent.parse(onPhoto).isEmpty) &&
      assertTrue(GroupEvent.parse(replied).contains(GroupEvent.Commented(7L, 28L))) &&
      assertTrue(GroupEvent.parse(byGroup).isEmpty) &&
      assertTrue(GroupEvent.parse(other).isEmpty && !GroupEvent.Types.contains("group_join"))
    },

    test("лайк — очко, комментарий — два, и по одному на пост, сколько ни повторяй") {
      for {
        t <- contest()
        (c, dao, _, _) = t
        _   <- c.handle(post(1L, now - hour), now)
        _   <- c.handle(like(1, 1L), now)
        _   <- c.handle(like(1, 1L), now + 1000L)      // сняла и поставила снова
        _   <- c.handle(comment(1, 1L), now + 2000L)
        _   <- c.handle(comment(1, 1L), now + 3000L)   // второй комментарий под тем же постом
        top <- dao.top(ActivityContest.weekStart(now), 5)
      } yield assertTrue(top.map(s => s.userId -> s.points) == List(UserId(1L) -> 3L))
    },

    test("старый пост, незнакомый пост, игрок без героя и чужак — без очков") {
      for {
        t <- contest()
        (c, dao, _, _) = t
        _   <- c.handle(post(1L, now - 8 * day), now)  // вышел раньше недели назад
        _   <- c.handle(like(1, 1L), now)
        _   <- c.handle(like(2, 77L), now)             // такого поста бот не видел
        _   <- c.handle(post(2L, now - hour), now)
        _   <- c.handle(like(9, 2L), now)              // аккаунт есть, героя нет
        _   <- c.handle(GroupEvent.Liked(555L, 2L), now) // не игрок вовсе
        top <- dao.top(ActivityContest.weekStart(now), 5)
      } yield assertTrue(top.isEmpty)
    },

    test("при равных очках выше тот, кто набрал их раньше") {
      for {
        t <- contest()
        (c, dao, _, _) = t
        _   <- c.handle(post(1L, now - hour), now)
        _   <- c.handle(like(2, 1L), now + 1000L)
        _   <- c.handle(like(1, 1L), now + 5000L)
        _   <- c.handle(like(3, 1L), now + 2000L)
        _   <- c.handle(comment(3, 1L), now + 3000L)
        top <- dao.top(ActivityContest.weekStart(now), 5)
      } yield assertTrue(top.map(_.userId) == List(UserId(3L), UserId(2L), UserId(1L)))
    },

    test("итоги: пятеро лучших прошлой недели получают по сотне дублонов — один раз") {
      for {
        t <- contest(players = 6)
        (c, _, payouts, players) = t
        _ <- c.handle(post(1L, now - hour), now)
        _ <- c.handle(post(2L, now - hour), now)
        // у шестерых разные очки: 6-й набрал меньше всех
        _ <- ZIO.foreachDiscard(1 to 5)(n => c.handle(comment(n, 1L), now + n * 1000L))
        _ <- ZIO.foreachDiscard(1 to 4)(n => c.handle(like(n, 2L), now + 10000L + n))
        _ <- c.handle(like(6, 1L), now + 20000L)
        // наступила следующая неделя
        nextWeek = ActivityContest.weekStart(now) + ActivityContest.WeekMs + hour
        _ <- c.settle(nextWeek)
        _ <- c.settle(nextWeek + 60000L)               // через минуту — уже подведено
      } yield assertTrue(payouts.snapshot.keySet == (1 to 5).map(n => HeroId(n.toLong)).toSet) &&
              assertTrue(payouts.snapshot.values.forall(_ == (0L, ActivityContest.Prize))) &&
              // итоги — в беседе, с упоминаниями победителей; каждому — письмо
              assertTrue(players.announced.size == 1 && players.announced.head.contains("Итоги недели")) &&
              assertTrue((1 to 5).forall(n => players.announced.head.contains(s"[id${100 + n}|"))) &&
              assertTrue(!players.announced.head.contains("[id106|")) &&
              assertTrue(players.sentLetters.map(_._1).toSet == (1 to 5).map(n => UserId(n.toLong)).toSet)
    },

    test("неделя без очков подводится молча: ни выплат, ни объявления") {
      for {
        t <- contest()
        (c, dao, payouts, players) = t
        _ <- c.settle(now)
      } yield assertTrue(payouts.snapshot.isEmpty && players.announced.isEmpty) &&
              assertTrue(dao.awardedSnapshot.contains(ActivityContest.weekStart(now) - ActivityContest.WeekMs))
    },

    test("«Рейтинг» в беседе: команду узнают, а таблица уходит в саму беседу") {
      for {
        t <- contest()
        (c, _, payoutDao, players) = t
        _        <- c.handle(post(1L, now - hour), now)
        _        <- c.handle(comment(1, 1L), now)
        heroDao  <- TestHeroDao.withHero(UserId(1L), TestFixtures.hero(UserId(1L)))
        userRepo <- TestUserRepository.withUsers(user(1))
        api      <- TestApi.make
        content  <- ZIO.attempt(SceneContent.load())
        lock     <- PlayerLock.make
        payouts   = Payouts(payoutDao, heroDao, content)
        parcels   = Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
        transfers = Transfers(TestInventoryRepository.accepting, userRepo, parcels, players, content,
                      heroDao, TestBankRepository.empty, payouts)
        handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                      parcels, transfers, Map.empty[StateType, State], AdminPanel.disabled, content, lock,
                      Some(c), None)
        _        <- TestClock.setTime(java.time.Instant.ofEpochMilli(now))
        _        <- handler.selfToChat(user(1).vkId, ChatCommand.Self.Rating, -1L)
        chat     <- api.chatMessages
      } yield assertTrue(ChatCommand.selfCommand("Рейтинг").contains(ChatCommand.Self.Rating)) &&
              assertTrue(ChatCommand.selfCommand("рейтинг активности!").contains(ChatCommand.Self.Rating)) &&
              assertTrue(ChatCommand.selfCommand("рейтинг друга").isEmpty) &&
              assertTrue(chat.size == 1 && chat.head.contains("1. Иван Иванов — 2"))
    },

    test("«Рейтинг»: таблица текущей недели и сколько до её конца") {
      for {
        t <- contest()
        (c, _, _, _) = t
        empty <- c.board(now)
        _     <- c.handle(post(1L, now - hour), now)
        _     <- c.handle(comment(2, 1L), now)
        board <- c.board(now)
      } yield assertTrue(empty.contains("очков пока нет") && empty.contains("5д 9ч")) &&
              assertTrue(board.contains("1. Иван Иванов — 2") && board.contains("🟡 100")) &&
              // без упоминаний: таблицу спрашивают часто, звать лучших незачем
              assertTrue(!board.contains("[id"))
    }
  )
}
