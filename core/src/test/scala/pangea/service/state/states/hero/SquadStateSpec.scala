package pangea.service.state.states.hero

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.squad.{AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

/** «Отряд» в меню персонажа: строй по позициям, карточка союзника,
  * перестановка (в том числе на место героя) и увольнение. */
object SquadStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def pick(key: String, kind: AllyKind, extra: (String, String)*): UserAction = {
    val fields = (("action" -> key) +: ("ally" -> kind.entryName) +: extra).map { case (k, v) => s""""$k":"$v"""" }
    UserAction("", Some(fields.mkString("{", ",", "}")))
  }

  private val lvl = 10L
  private val baseHero: Hero = TestFixtures.hero(userId, state = StateType.HeroStats).copy(lvl = lvl,
    squad = Squad.empty.hire(AllyKind.Human, lvl, 0L).hire(AllyKind.Gnome, lvl, 0L))

  private def makeState(hero: Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SquadState(heroDao, content), heroDao, renderer)

  override def spec = suite("SquadState")(

    test("кнопка «Отряд» в профиле есть только с союзниками") {
      for {
        withT <- (for {
                   dao      <- TestHeroDao.withHero(userId, baseHero)
                   renderer <- TestRenderer.make
                   content  <- ZIO.attempt(SceneContent.load())
                   _        <- HeroStatsState(dao, content).enter(testUser, renderer)
                   scr      <- renderer.sentScreens
                 } yield scr.last.choices.exists(_.id == "OpenSquad"))
        without <- (for {
                   dao      <- TestHeroDao.withHero(userId, baseHero.copy(squad = Squad.empty))
                   renderer <- TestRenderer.make
                   content  <- ZIO.attempt(SceneContent.load())
                   _        <- HeroStatsState(dao, content).enter(testUser, renderer)
                   scr      <- renderer.sentScreens
                 } yield scr.last.choices.exists(_.id == "OpenSquad"))
      } yield assertTrue(withT && !without)
    },

    test("enter → строй по позициям: герой, союзники, пустое место; кнопки — союзники и «Назад»") {
      for {
        t <- makeState(baseHero)
        (state, _, renderer) = t
        _   <- state.enter(testUser, renderer)
        scr <- renderer.sentScreens.map(_.last)
      } yield assertTrue(scr.text.contains("1. Вы")) &&
              assertTrue(scr.text.contains("2. Йорген Кремень — ❤ 1250/1250  🧥 1500/1500")) &&
              assertTrue(scr.text.contains("3. Брамбл Медноус — ❤ 800/800  🧥 1750/1750")) &&
              assertTrue(scr.text.contains("4. — пусто —")) &&
              assertTrue(scr.choices.map(_.label) == List("Йорген Кремень", "Брамбл Медноус", "↩ Назад")) &&
              assertTrue(scr.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
    },

    test("карточка: состояние и позиция, кнопки «На N (кто там)», «Выгнать», «К отряду»") {
      for {
        t <- makeState(baseHero)
        (state, _, renderer) = t
        _   <- state.action(testUser, pick("SquadAlly", AllyKind.Human), renderer)
        scr <- renderer.sentScreens.map(_.last)
        moves = scr.choices.filter(_.id == "SquadMove").map(c => c.label -> c.data("pos"))
      } yield assertTrue(scr.text.startsWith("Йорген Кремень\nЧеловек, стихия 🔥 — позиция 2\n")) &&
              assertTrue(scr.text.contains(" ❤ 1250/1250  🧥 Броня 1500/1500  ⚡ Энергия 1000/1000\n ⚔ Атк 200  🛡 Защ 400\n 🎯 Точн 1000  👁 Укл 1000")) &&
              assertTrue(moves == List("На 1 (вы)" -> "1", "На 3 (Брамбл Медноус)" -> "3", "На 4 (пусто)" -> "4")) &&
              assertTrue(scr.choices.exists(_.id == "SquadDismiss") && scr.choices.exists(_.id == "SquadList")) &&
              assertTrue(scr.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
    },

    test("перестановка на место героя: герой встаёт на прежнее; на союзника — меняются; сохраняется") {
      for {
        t <- makeState(baseHero)
        (state, dao, renderer) = t
        r1 <- state.action(testUser, pick("SquadMove", AllyKind.Human, "pos" -> "1"), renderer)
        h1 <- dao.getHeroByUserId(userId).map(_.get)
        _  <- state.action(testUser, pick("SquadMove", AllyKind.Gnome, "pos" -> "1"), renderer)
        h2 <- dao.getHeroByUserId(userId).map(_.get)
        scr <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(r1 == StateType.Squad) &&
              assertTrue(h1.squad.heroPos == 2 && h1.squad.allyAt(1).exists(_.kind == AllyKind.Human)) &&
              assertTrue(h2.squad.allyAt(1).exists(_.kind == AllyKind.Gnome) && h2.squad.allyAt(3).exists(_.kind == AllyKind.Human)) &&
              assertTrue(scr.contains("Йорген Кремень теперь на позиции 1."))
    },

    test("увольнение — с подтверждением; «Нет» оставляет, «Да» убирает и сохраняет") {
      for {
        t <- makeState(baseHero)
        (state, dao, renderer) = t
        _   <- state.action(testUser, pick("SquadDismiss", AllyKind.Human), renderer)
        ask <- renderer.sentScreens.map(_.last)
        _   <- state.action(testUser, pick("SquadAlly", AllyKind.Human), renderer)
        kept <- dao.getHeroByUserId(userId).map(_.get)
        _   <- state.action(testUser, pick("SquadDismissDo", AllyKind.Human), renderer)
        gone <- dao.getHeroByUserId(userId).map(_.get)
        back <- state.action(testUser, tap("BackFromSquad"), renderer)
      } yield assertTrue(ask.text.contains("Выгнать Йорген Кремень из отряда?")) &&
              assertTrue(ask.choices.map(_.id) == List("SquadDismissDo", "SquadAlly")) &&
              assertTrue(kept.squad.has(AllyKind.Human)) &&
              assertTrue(!gone.squad.has(AllyKind.Human) && gone.squad.has(AllyKind.Gnome)) &&
              assertTrue(back == StateType.HeroStats)
    }
  )
}
