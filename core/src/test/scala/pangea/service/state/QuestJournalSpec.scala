package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.LoreData
import pangea.model.quest._
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.hero.HeroStatsState
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

/** «📜 Задания» из «Персонажа»: всё, что у героя в работе, одним сообщением. */
object QuestJournalSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String) = pangea.service.state.UserAction("", Some(s"""{"action":"$key"}"""))

  private val hero = TestFixtures.hero(userId).copy(lvl = 3L)

  // Понедельник, полдень по Москве: и сутки, и неделя доски — одни и те же.
  private val now: Long = 1760346000000L

  private def content = ZIO.attempt(SceneContent.load())

  /** Густаво на втором шаге (одна победа из трёх), Трактирщик сдан, письмо Марисе
    * вскрыто на первом шаге. */
  private val quests = NpcQuests.empty
    .take(NpcQuest.Gustavo, now).update(NpcQuest.Gustavo)(_.copy(step = 2, counter = 1L))
    .take(NpcQuest.Innkeeper, now).finish(NpcQuest.Innkeeper)
    .take(NpcQuest.Marisa, now).update(NpcQuest.Marisa)(_.copy(bonus = true))

  private val daily = DailyQuests.empty
    .updated(DailyNpc.Richelieu, DailyTask(DailyKind.SellItems, DailyRates.dayOf(now), count = 1L, goal = 3L, taken = true))
    // взятое вчера в журнал не идёт
    .updated(DailyNpc.Horn, DailyTask(DailyKind.HornKills, DailyRates.dayOf(now) - 1L, taken = true))

  private val board = BoardData(BoardRates.weekOf(now), BoardTier.of(hero.lvl).key, List(
    BoardSlot(BoardKind.CaveClear, taken = true, lvl = 5L),
    BoardSlot(BoardKind.Thieves, taken = true, done = true),
    BoardSlot(BoardKind.SewerRats)))

  override def spec = suite("Журнал заданий")(

    test("всё, что в работе: сюжет, горожане на своём шаге, сегодняшние поручения, взятое с доски") {
      for {
        c <- content
        text = QuestJournal.render(c, hero, LoreData(firstSteps = true), quests, daily, Some(board), now)
      } yield assertTrue(text.startsWith("📜 Ваши задания")) &&
              assertTrue(text.contains("📌 «Встать на ноги»") && text.contains("горожан (1/5)")) &&
              assertTrue(text.contains("🧭 «Подопытный» — победить троих, пока действует зелье Густаво (1/3)")) &&
              assertTrue(text.contains("🧭 «Письмо Марисе» — спросить о Марисе у Трактирщика Или сразу по карте")) &&
              // сданное не висит
              assertTrue(!text.contains("Плата за первую кружку")) &&
              assertTrue(text.contains("📅 Ришелье: «Продано 1 из 3.")) &&
              assertTrue(!text.contains("Мастер Горн")) &&
              assertTrue(text.contains("📋 Доска 1–25") && text.contains("— в работе") && text.contains("— сделано, можно сдавать")) &&
              // не взятое объявление в журнал не попадает
              assertTrue(text.linesIterator.count(_.startsWith("📋")) == 2)
    },

    test("доска прошлой недели и сюжет, которого герой не брал, не числятся; пусто — так и сказано") {
      for {
        c <- content
        stale = board.copy(week = board.week - 1L)
        text  = QuestJournal.render(c, hero, LoreData.empty, NpcQuests.empty, DailyQuests.empty, Some(stale), now)
      } yield assertTrue(text == c.text("journal.empty"))
    },

    test("у каждого задания горожан есть название и строка на каждый шаг") {
      for {
        c <- content
      } yield assertTrue(NpcQuest.values.forall(q =>
                c.text(s"journal.quests.${q.key}.title").nonEmpty && c.list(s"journal.quests.${q.key}.steps").nonEmpty)) &&
              assertTrue(DailyNpc.values.forall(n => c.text(s"journal.dailyNpc.${n.key}").nonEmpty))
    },

    test("в «Персонаже» есть кнопка «📜 Задания»: присылает журнал, а кнопки «Персонажа» остаются") {
      for {
        dao <- TestHeroDao.withHero(userId, hero)
        _   <- dao.writeLoreData(userId, LoreData(firstSteps = true).asJson)
        r   <- TestRenderer.make
        c   <- content
        state = HeroStatsState(dao, c)
        _      <- state.enter(testUser, r)
        main   <- r.sentScreens.map(_.last)
        result <- state.action(testUser, tap("OpenQuests"), r)
        shown  <- r.sentScreens.map(_.last)
      } yield assertTrue(main.choices.exists(ch => ch.id == "OpenQuests" && ch.label == "📜 Задания")) &&
              assertTrue(result == StateType.HeroStats) &&
              assertTrue(shown.text.contains("«Встать на ноги»") && shown.choices.map(_.id) == main.choices.map(_.id))
    }
  )
}
