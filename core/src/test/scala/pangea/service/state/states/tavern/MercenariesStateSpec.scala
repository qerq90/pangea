package pangea.service.state.states.tavern

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, Item}
import pangea.model.squad.{AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Наёмники в таверне: за столом только свободные; Йорген берёт серебром,
  * Плюх — Живой водой, Брамбл — шнапсом; вернувшийся из отлучки встречает
  * репликой и снова нанимается. */
object MercenariesStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def pick(key: String, kind: AllyKind): UserAction =
    UserAction("", Some(s"""{"action":"$key","kind":"${kind.entryName}"}"""))

  private def brew(kind: BrewKind, id: Long): Item = BrewKind.item(kind).copy(id = id)

  private val baseHero: Hero = TestFixtures.hero(userId, state = StateType.Tavern).copy(lvl = 10L, silver = 0L)

  private def makeState(hero: Hero, items: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (MercenariesState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("MercenariesState")(

    test("enter → трое за столом и «Назад»; карточка — история и реплика с ценой") {
      for {
        t <- makeState(baseHero)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, pick("MercCard", AllyKind.Murloc), renderer)
        screens <- renderer.sentScreens
        list     = screens.head
        card     = screens.last
      } yield assertTrue(list.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Human", "Murloc", "Gnome")) &&
              assertTrue(list.choices.exists(_.id == "BackFromMercs")) &&
              assertTrue(card.text.contains("«Плюх. Плюх тут кружки мыл")) &&
              assertTrue(card.text.contains("сегодня я возьму с тебя 2 флаконов!")) &&
              assertTrue(card.choices.exists(c => c.id == "MercHire" && c.data.get("kind").contains("Murloc")))
    },

    test("Йорген: 500 серебра за уровень — мало серебра → отказ, хватает → нанят, серебро списано") {
      for {
        poor <- makeState(baseHero.copy(silver = 4999L))
        (ps, pdao, _, pr) = poor
        _     <- ps.action(testUser, pick("MercHire", AllyKind.Human), pr)
        pHero <- pdao.getHeroByUserId(userId).map(_.get)
        pScr  <- pr.sentScreens.map(_.map(_.text).mkString("\n"))
        rich <- makeState(baseHero.copy(silver = 5000L))
        (rs, rdao, _, rr) = rich
        _     <- rs.action(testUser, pick("MercHire", AllyKind.Human), rr)
        rHero <- rdao.getHeroByUserId(userId).map(_.get)
        rScr  <- rr.sentScreens
      } yield assertTrue(pScr.contains("Не хватает серебра: нужно 5000, у вас 4999")) &&
              assertTrue(pHero.squad.isEmpty && pHero.silver == 4999L) &&
              assertTrue(rHero.silver == 0L && rHero.squad.allyAt(2).exists(_.kind == AllyKind.Human)) &&
              assertTrue(rScr.exists(_.text.contains("Йорген Кремень встаёт из-за стола"))) &&
              // за столом его больше нет
              assertTrue(rScr.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Murloc", "Gnome"))
    },

    test("Плюх берёт Живой водой (уровень 10 → 2 флакона): одного мало, два — уходят из сумки") {
      for {
        one <- makeState(baseHero, List(brew(BrewKind.LivingWater, 1L), brew(BrewKind.Schnapps, 2L)))
        (os, odao, _, or) = one
        _     <- os.action(testUser, pick("MercHire", AllyKind.Murloc), or)
        oHero <- odao.getHeroByUserId(userId).map(_.get)
        oScr  <- or.sentScreens.map(_.map(_.text).mkString("\n"))
        two <- makeState(baseHero, List(brew(BrewKind.LivingWater, 1L), brew(BrewKind.LivingWater, 2L), brew(BrewKind.Schnapps, 3L)))
        (ts, tdao, tinv, tr) = two
        _     <- ts.action(testUser, pick("MercHire", AllyKind.Murloc), tr)
        tHero <- tdao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(oScr.contains("нужно 2 — «Живая вода», у вас 1")) &&
              assertTrue(oHero.squad.isEmpty) &&
              assertTrue(tHero.squad.has(AllyKind.Murloc)) &&
              assertTrue(tinv.snapshot.map(_.id) == List(3L))
    },

    test("Брамбл берёт шнапсом; на первом уровне — одну бутылку") {
      val lowHero = baseHero.copy(lvl = 1L)
      for {
        t <- makeState(lowHero, List(brew(BrewKind.Schnapps, 1L)))
        (state, dao, inv, renderer) = t
        _    <- state.action(testUser, pick("MercCard", AllyKind.Gnome), renderer)
        card <- renderer.sentScreens.map(_.last.text)
        _    <- state.action(testUser, pick("MercHire", AllyKind.Gnome), renderer)
        hero <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(card.contains("бутылок: 1.")) &&
              assertTrue(hero.squad.has(AllyKind.Gnome) && inv.snapshot.isEmpty)
    },

    test("наёмник отработал 12 часов: у стола уходит с репликой, его нет; через 12 часов снова здесь — без реплики о свитке") {
      for {
        t <- makeState(baseHero.copy(silver = 5000L))
        (state, dao, _, renderer) = t
        _     <- state.action(testUser, pick("MercHire", AllyKind.Human), renderer)
        _     <- TestClock.adjust(zio.Duration.fromMillis(12L * 60L * 60L * 1000L))
        _     <- state.enter(testUser, renderer)
        scr   <- renderer.sentScreens
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        _     <- TestClock.adjust(zio.Duration.fromMillis(12L * 60L * 60L * 1000L))
        _     <- state.enter(testUser, renderer)
        all   <- renderer.sentScreens
      } yield assertTrue(scr.exists(_.text.contains("Йорген Кремень отработал свой день"))) &&
              assertTrue(!hero.squad.has(AllyKind.Human) && hero.squad.offDuty.contains("Human")) &&
              assertTrue(scr.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Murloc", "Gnome")) &&
              assertTrue(all.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Human", "Murloc", "Gnome")) &&
              assertTrue(!all.exists(_.text.contains("снова в таверне")))
    },

    test("в отлучке — за столом нет; вернулся — реплика о свитке один раз и снова нанимается") {
      val away = baseHero.copy(squad = Squad.empty.sentAway(AllyKind.Human, 0L))
      for {
        gone <- makeState(away)
        (gs, _, _, gr) = gone
        _      <- gs.enter(testUser, gr)
        gList  <- gr.sentScreens.map(_.head)
        _      <- TestClock.adjust(zio.Duration.fromMillis(24L * 60L * 60L * 1000L))
        back <- makeState(away)
        (bs, bdao, _, br) = back
        _      <- bs.enter(testUser, br)
        bScr   <- br.sentScreens
        bHero  <- bdao.getHeroByUserId(userId).map(_.get)
        _      <- bs.enter(testUser, br)
        again  <- br.sentScreens
      } yield assertTrue(gList.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Murloc", "Gnome")) &&
              assertTrue(bScr.head.text.contains("Йорген Кремень снова в таверне.")) &&
              assertTrue(bScr.head.text.contains("свиток вынес")) &&
              assertTrue(bScr.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Human", "Murloc", "Gnome")) &&
              assertTrue(bHero.squad.away.isEmpty) &&
              assertTrue(again.count(_.text.contains("снова в таверне")) == 1)
    }
  )
}
