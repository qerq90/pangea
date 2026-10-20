package pangea.service.state.states.hero

import pangea.engine.SceneContent
import pangea.model.hero.Achievement
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

/** «Персонаж» → «🏆 Достижения»: все достижения, что дают, и счёт полученных. */
object AchievementsScreenSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val total = Achievement.values.size

  override def spec = suite("Достижения в «Персонаже»")(

    test("кнопка «🏆 Достижения» — рядом с «Заданиями»; «Назад» — последним рядом") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        r    <- TestRenderer.make
        c    <- ZIO.attempt(SceneContent.load())
        _    <- HeroStatsState(dao, c).enter(testUser, r)
        main <- r.sentScreens.map(_.last)
        row   = (id: String) => main.choices.find(_.id == id).flatMap(_.row)
      } yield assertTrue(main.choices.exists(ch => ch.id == "OpenAchievements" && ch.label == "🏆 Достижения")) &&
              assertTrue(row("OpenAchievements") == row("OpenQuests")) &&
              assertTrue(row("Back").exists(b => main.choices.flatMap(_.row).forall(_ <= b)))
    },

    test("экран: «получено 2 из всех», полученные ✅ сверху, остальные 🔒, у каждого — что даёт; кнопки остаются") {
      val hero = TestFixtures.hero(userId)
                   .withAchievement(Achievement.MurlocFavorite)
                   .withAchievement(Achievement.Brewer1)
      for {
        dao    <- TestHeroDao.withHero(userId, hero)
        r      <- TestRenderer.make
        c      <- ZIO.attempt(SceneContent.load())
        state   = HeroStatsState(dao, c)
        _      <- state.enter(testUser, r)
        main   <- r.sentScreens.map(_.last)
        result <- state.action(testUser, tap("OpenAchievements"), r)
        shown  <- r.sentScreens.map(_.last)
        lines   = shown.text.split("\n").toList.filter(l => l.startsWith("✅") || l.startsWith("🔒"))
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(shown.text.startsWith(s"🏆 Достижения: получено 2 из $total")) &&
              assertTrue(lines.size == total) &&
              // полученные — первыми, в порядке, в каком заведены достижения
              assertTrue(lines.take(2) == List(
                s"✅ ${Achievement.Brewer1.title} — ${Achievement.Brewer1.bonusLine}",
                s"✅ ${Achievement.MurlocFavorite.title} — ${Achievement.MurlocFavorite.bonusLine}")) &&
              assertTrue(lines.drop(2).forall(_.startsWith("🔒"))) &&
              assertTrue(lines.exists(_ == s"🔒 ${Achievement.MarisaSavior.title} — ${Achievement.MarisaSavior.bonusLine}")) &&
              assertTrue(shown.choices.map(_.id) == main.choices.map(_.id))
    },

    test("ни одного достижения — «получено 0 из всех», все под замком") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        text = HeroStatsState.achievementsText(c, TestFixtures.hero(userId))
      } yield assertTrue(text.startsWith(s"🏆 Достижения: получено 0 из $total")) &&
              assertTrue(text.split("\n").count(_.startsWith("🔒")) == total && !text.contains("✅"))
    }
  )
}
