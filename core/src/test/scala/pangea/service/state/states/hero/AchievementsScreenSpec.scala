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

    test("экран: «получено 2 из всех», полученные ✅ с тем, что дают; неполученные скрыты; кнопки остаются") {
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
        lines   = shown.text.split("\n").toList.filter(_.startsWith("✅"))
        locked  = Achievement.values.filterNot(hero.hasAchievement)
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(shown.text.startsWith(s"🏆 Достижения: получено 2 из $total")) &&
              // полученные — в порядке, в каком заведены достижения, с тем, что дают
              assertTrue(lines == List(
                s"✅ ${Achievement.Brewer1.title} — ${Achievement.Brewer1.bonusLine}",
                s"✅ ${Achievement.MurlocFavorite.title} — ${Achievement.MurlocFavorite.bonusLine}")) &&
              // неполученные — только числом: ни названия, ни бонуса
              assertTrue(shown.text.contains(s"🔒 Ещё не получено: ${total - 2}.")) &&
              assertTrue(locked.forall(a => !shown.text.contains(a.title) && !shown.text.contains(a.bonusLine))) &&
              assertTrue(shown.choices.map(_.id) == main.choices.map(_.id))
    },

    test("ни одного достижения — «пока ни одного», все скрыты; все получены — строки «скрыто» нет") {
      val all = Achievement.values.foldLeft(TestFixtures.hero(userId))(_ withAchievement _)
      for {
        c    <- ZIO.attempt(SceneContent.load())
        none  = HeroStatsState.achievementsText(c, TestFixtures.hero(userId))
        full  = HeroStatsState.achievementsText(c, all)
      } yield assertTrue(none.startsWith(s"🏆 Достижения: получено 0 из $total")) &&
              assertTrue(none.contains("Пока ни одного.") && none.contains(s"🔒 Ещё не получено: $total.")) &&
              assertTrue(Achievement.values.forall(a => !none.contains(a.title))) &&
              assertTrue(full.startsWith(s"🏆 Достижения: получено $total из $total") && !full.contains("🔒")) &&
              assertTrue(full.split("\n").count(_.startsWith("✅")) == total)
    }
  )
}
