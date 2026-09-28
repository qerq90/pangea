package pangea.service.admin

import pangea.dao.admin.{AdminDao, AdminStats}
import pangea.model.hero.Hero
import pangea.model.item.{GemKind, ItemSet, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test._
import zio.test._
import zio.{Ref, Task, ZIO}

/** Админ-панель: вход по паролю, выдача вещей, деньги и сводка по серверу.
  * Панель живёт поверх игры — ни состояние героя, ни сцена от неё не меняются. */
object AdminPanelSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def text(t: String): UserAction        = UserAction(t, None)
  private def tap(id: String): UserAction        = UserAction("", Some(s"""{"action":"$id"}"""))

  private val Password = "проверочный-пароль"

  private val stubStats = AdminStats(
    heroes = 7L, active24h = 3L, active7d = 5L,
    heroSilver = 1000L, vaultSilver = 500L, doubloons = 42L,
    sets = List(AdminStats.SetRow(ItemSet.Hunter, 6, 54L), AdminStats.SetRow(ItemSet.Ghoul, 8, 32L)))

  private val stubDao: AdminDao = new AdminDao {
    def stats: Task[AdminStats] = ZIO.succeed(stubStats)
  }

  private def panel(hero: Hero = TestFixtures.hero(userId), password: Option[String] = Some(Password)) =
    for {
      dao      <- TestHeroDao.withHero(userId, hero)
      inv       = TestInventoryRepository.accepting
      sessions <- Ref.make(Map.empty[UserId, AdminScreen])
      r        <- TestRenderer.make
    } yield (new AdminPanelLive(dao, inv, TestItemRepository.make, stubDao, Map.empty, AdminConfig(password), sessions),
             dao, inv, r)

  private def said(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  /** Войти в панель и оказаться на главном экране. */
  private def enter(p: AdminPanelLive, r: TestRenderer): Task[Unit] =
    p.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r) *>
      p.intercept(testUser, TestFixtures.hero(userId), text(Password), r).unit

  override def spec = suite("Админ-панель")(

    test("«/admin» просит пароль; верный пускает, неверный закрывает") {
      for {
        t <- panel()
        (p, _, _, r) = t
        opened <- p.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r)
        ask    <- r.sentScreens.map(_.last)
        ok     <- p.intercept(testUser, TestFixtures.hero(userId), text(Password), r)
        main   <- r.sentScreens.map(_.last)
        // после закрытия сессии обычный текст панели уже не достаётся
        t2 <- panel()
        (p2, _, _, r2) = t2
        _      <- p2.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r2)
        _      <- p2.intercept(testUser, TestFixtures.hero(userId), text("не тот"), r2)
        after  <- p2.intercept(testUser, TestFixtures.hero(userId), text("что-нибудь"), r2)
        wrong  <- said(r2)
      } yield assertTrue(opened && ok) &&
              assertTrue(ask.text.contains("Пришлите пароль")) &&
              assertTrue(main.choices.map(_.id) == List("AdminItems", "AdminMoney", "AdminStats", AdminPanel.CancelId)) &&
              assertTrue(wrong.contains("Неверно") && !after)
    },

    test("без ADMIN_PASSWORD панель не пускает никого") {
      for {
        t <- panel(password = None)
        (p, _, _, r) = t
        _    <- p.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r)
        _    <- p.intercept(testUser, TestFixtures.hero(userId), text("любой пароль"), r)
        says <- said(r)
        // сессии больше нет: панель отдала ввод игре
        more <- p.intercept(testUser, TestFixtures.hero(userId), text("дальше"), r)
      } yield assertTrue(says.contains(AdminConfig.EnvName) && !more)
    },

    test("«/home» сильнее панели: сессия закрывается, ввод уходит игре") {
      for {
        t <- panel()
        (p, _, _, r) = t
        _      <- enter(p, r)
        escape <- p.intercept(testUser, TestFixtures.hero(userId), text("/home"), r)
        after  <- p.intercept(testUser, TestFixtures.hero(userId), text("что-нибудь"), r)
      } yield assertTrue(!escape && !after)
    },

    test("каталог: раздел, страница и выдача — вещь ложится в сумку") {
      for {
        t <- panel()
        (p, _, inv, r) = t
        hero = TestFixtures.hero(userId)
        _     <- enter(p, r)
        _     <- p.intercept(testUser, hero, tap("AdminItems"), r)
        _     <- p.intercept(testUser, hero, tap("AdminSections"), r)
        list  <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, hero, tap(s"${AdminPanel.SectionPrefix}gem"), r)
        gems  <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, hero, tap(AdminPanel.NextId), r)
        page2 <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, hero, tap(s"${AdminPanel.GivePrefix}gem:${GemKind.Ruby.entryName}:3"), r)
        given <- said(r)
      } yield assertTrue(list.choices.exists(_.id == s"${AdminPanel.SectionPrefix}gem")) &&
              assertTrue(gems.text.contains("стр. 1/") && page2.text.contains("стр. 2/")) &&
              assertTrue(given.contains("Выдано")) &&
              assertTrue(inv.snapshot.exists(_.gem.exists(g => g.kind == GemKind.Ruby && g.grade == 3))) &&
              // все кнопки панели укладываются в клавиатуру ВК
              assertTrue(gems.choices.flatMap(_.row).groupBy(identity).forall(_._2.size <= 5))
    },

    test("поиск по куску названия находит вещь и выдаёт её") {
      for {
        t <- panel()
        (p, _, inv, r) = t
        hero = TestFixtures.hero(userId)
        _     <- enter(p, r)
        _     <- p.intercept(testUser, hero, tap("AdminItems"), r)
        _     <- p.intercept(testUser, hero, text("Живая вода"), r)
        found <- r.sentScreens.map(_.last)
        id     = found.choices.find(_.id.startsWith(AdminPanel.GivePrefix)).map(_.id).getOrElse("")
        _     <- p.intercept(testUser, hero, tap(id), r)
        // и по чепухе ничего не находится
        _     <- p.intercept(testUser, hero, text("абырвалг"), r)
        empty <- r.sentScreens.map(_.last)
      } yield assertTrue(found.text.contains("Найдено")) &&
              assertTrue(inv.snapshot.exists(_.brew.exists(_.label == "Живая вода"))) &&
              assertTrue(empty.text.contains("ничего не нашлось"))
    },

    test("экипировка собирается по слоту, редкости и уровню") {
      for {
        t <- panel()
        (p, _, inv, r) = t
        hero = TestFixtures.hero(userId)
        _    <- enter(p, r)
        _    <- p.intercept(testUser, hero, tap("AdminItems"), r)
        _    <- p.intercept(testUser, hero, tap("AdminEquip"), r)
        _    <- p.intercept(testUser, hero, tap(s"${AdminPanel.TypePrefix}${ItemType.Weapon.entryName}"), r)
        _    <- p.intercept(testUser, hero, tap(s"${AdminPanel.RarityPrefix}${Rarity.Orange.entryName}"), r)
        ask  <- r.sentScreens.map(_.last)
        // уровень пишем числом
        _    <- p.intercept(testUser, hero, text("75"), r)
        made  = inv.snapshot.headOption
        // чепуха и запредельный уровень не проходят, экран остаётся на месте
        _     <- p.intercept(testUser, hero, text("сто"), r)
        _     <- p.intercept(testUser, hero, text("500"), r)
        scold <- said(r)
      } yield assertTrue(ask.text.contains("пришлите уровень числом")) &&
              assertTrue(made.exists(i => i.itemType == ItemType.Weapon && i.rarity == Rarity.Orange && i.lvl == 75L)) &&
              assertTrue(scold.contains(AdminPanel.BadLevel)) &&
              // ни одна из двух неудачных попыток вещь не создала
              assertTrue(inv.snapshot.size == 1)
    },

    test("деньги начисляются кнопками номиналов") {
      val rich = TestFixtures.hero(userId).copy(silver = 100L, doubloons = 5L)
      for {
        t <- panel(rich)
        (p, dao, _, r) = t
        _     <- enter(p, r)
        _     <- p.intercept(testUser, rich, tap("AdminMoney"), r)
        money <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, rich, tap(s"${AdminPanel.SilverPrefix}10000"), r)
        _     <- p.intercept(testUser, rich, tap(s"${AdminPanel.DoubloonPrefix}100"), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(money.choices.count(_.id.startsWith(AdminPanel.SilverPrefix)) == 3) &&
              assertTrue(after.silver == 10100L && after.doubloons == 105L)
    },

    test("сводка по серверу показывает игроков, деньги и наборы") {
      for {
        t <- panel()
        (p, _, _, r) = t
        _     <- enter(p, r)
        _     <- p.intercept(testUser, TestFixtures.hero(userId), tap("AdminStats"), r)
        stats <- r.sentScreens.map(_.last)
      } yield assertTrue(stats.text.contains("всего героев: 7")) &&
              assertTrue(stats.text.contains("заходили за сутки: 3") && stats.text.contains("заходили за неделю: 5")) &&
              assertTrue(stats.text.contains("серебро всего: 1 500") && stats.text.contains("дублоны: 42")) &&
              assertTrue(stats.text.contains("Охотник на 6: 54") && stats.text.contains("Упырь на 8: 32"))
    },

    test("панель не трогает ни состояние героя, ни сцену") {
      val busy = TestFixtures.hero(userId, state = StateType.Battle)
      for {
        t <- panel(busy)
        (p, dao, _, r) = t
        _     <- dao.writeSceneData(userId, io.circe.Json.obj("важное" -> io.circe.Json.fromString("значение")))
        _     <- enter(p, r)
        _     <- p.intercept(testUser, busy, tap("AdminMoney"), r)
        _     <- p.intercept(testUser, busy, tap(s"${AdminPanel.SilverPrefix}1000"), r)
        _     <- p.intercept(testUser, busy, tap(AdminPanel.CancelId), r)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        scene <- dao.readSceneData(userId)
      } yield assertTrue(hero.state == StateType.Battle) &&
              assertTrue(scene.exists(_.hcursor.get[String]("важное").contains("значение")))
    },

    test("пароль не попадает в лог входящих сообщений") {
      val secret = "s3cret"
      assertTrue(AdminPanel.maskSecrets(secret, Some(secret)) == AdminPanel.Masked) &&
      // с пробелами по краям — тоже пароль
      assertTrue(AdminPanel.maskSecrets(s"  $secret ", Some(secret)) == AdminPanel.Masked) &&
      // прочие сообщения в логе остаются как есть
      assertTrue(AdminPanel.maskSecrets("/admin", Some(secret)) == "/admin") &&
      assertTrue(AdminPanel.maskSecrets("обычное сообщение", Some(secret)) == "обычное сообщение") &&
      assertTrue(AdminPanel.maskSecrets(secret, None) == secret)
    },

    test("наборы считаются по надетому, одиночный предмет не в счёт") {
      val six   = TestFixtures.wearingSet(ItemSet.Hunter, 6)
      val eight = TestFixtures.wearingSet(ItemSet.Ghoul, 8)
      val one   = TestFixtures.wearingSet(ItemSet.Hunter, 1)
      val rows  = AdminStats.setRows(List(six, six, eight, one), AdminDao.MinWorn)
      assertTrue(rows.exists(r => r.set == ItemSet.Hunter && r.worn == 6 && r.heroes == 2)) &&
      assertTrue(rows.exists(r => r.set == ItemSet.Ghoul && r.worn == 8 && r.heroes == 1)) &&
      assertTrue(!rows.exists(_.worn < AdminDao.MinWorn))
    }
  )
}
