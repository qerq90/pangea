package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.home.HomeData
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.rune.{Rune, RuneData, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Рунический алтарь: камни ложатся одной кнопкой и становятся счётом по рунам
  * (большая — пять, малая — одна); углубить понимание, как у Казимира; забрать
  * пятёрками — большими, остаток — малыми. */
object RuneAltarStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private val cunning = Rune.Active(Skill.CunningStrike)
  private val healer  = Rune.Passive(PassiveKind.Healer)

  private def stone(id: Long, rune: Rune, size: RuneStoneSize): Item = RuneStone.item(rune, size).copy(id = id)
  private def sword(id: Long): Item =
    Item(id, s"Меч $id", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0,
      evasion = 0, details = ItemDetails.Weapon(Skill.CunningStrike))

  private def hero(lvl: Long = 1L, runes: RuneData = RuneData.empty) =
    TestFixtures.hero(userId, StateType.RuneAltar).copy(lvl = lvl, runes = runes)

  private def make(h: pangea.model.hero.Hero, items: List[Item], home: HomeData) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (RuneAltarState(dao, inv, TestItemRepository.make, c), dao, inv, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def last(r: TestRenderer)  = r.sentScreens.map(_.last)
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private val cottage = HomeData(tier = 1, metJunona = true)

  override def spec = suite("Рунический алтарь")(

    test("«Положить все руны»: камни уходят из сумки, на алтаре — счёт по рунам; вещи с рунами остаются") {
      val items = List(stone(101L, cunning, RuneStoneSize.Big), stone(102L, cunning, RuneStoneSize.Small),
        stone(103L, cunning, RuneStoneSize.Small), stone(104L, healer, RuneStoneSize.Big), sword(105L))
      for {
        t <- make(hero(), items, cottage)
        (state, dao, inv, r) = t
        _    <- state.enter(testUser, r)
        menu <- last(r)
        _    <- state.action(testUser, tap("AltarPutAll"), r)
        home <- homeOf(dao)
        said <- texts(r)
        list <- last(r)
        // второй раз класть нечего
        _    <- state.action(testUser, tap("AltarPutAll"), r)
        none <- texts(r)
      } yield assertTrue(menu.text.contains("Алтарь пуст")) &&
              assertTrue(home.runes == Map(cunning.key -> 7L, healer.key -> 5L)) &&
              assertTrue(inv.snapshot.map(_.id) == List(105L)) &&      // меч с руной не взят
              assertTrue(said.contains("Алтарь вбирает узоры") && said.contains("• Хитрый удар: +7")) &&
              assertTrue(list.text.contains("Узоров на алтаре: 12")) &&
              // руны — по алфавиту
              assertTrue(list.choices.filter(_.id == "AltarRune").map(_.label) == List("Хитрый удар — 7", "Целитель — 5")) &&
              assertTrue(none.contains("В сумке нет рунных камней"))
    },

    test("экран руны и «углубить» — как у Казимира: до потолка по уровню, остаток ждёт на алтаре") {
      // потолок на 1-м уровне — 30; понимание уже 25, на алтаре 12
      val h = hero(lvl = 1L, runes = RuneData.empty.deepen(cunning, 25L, Rune.cap(1L))._1)
      for {
        t <- make(h, Nil, cottage.withRune(cunning.key, 12L))
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("AltarRune", "k" -> cunning.key, "rp" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("AltarDeepen", "k" -> cunning.key, "rp" -> "0"), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        home   <- homeOf(dao)
        said   <- texts(r)
        // у потолка — узоры остаются, понимание не растёт
        _      <- state.action(testUser, tap("AltarDeepen", "k" -> cunning.key, "rp" -> "0"), r)
        atCap  <- texts(r)
        home2  <- homeOf(dao)
      } yield assertTrue(screen.text.contains("На алтаре: 12. В сумку выйдет больших рун — 2, малых — 2.")) &&
              assertTrue(screen.text.contains("Понимание: 25/30")) &&
              assertTrue(screen.choices.map(_.id) == List("AltarDeepen", "AltarTake", "AltarMenu")) &&
              assertTrue(after.runes.understandingOf(cunning) == 30L && home.runeCount(cunning.key) == 7L) &&
              assertTrue(said.contains("стало глубже: 30 (+5). На алтаре осталось: 7.")) &&
              assertTrue(atCap.contains("насколько позволяет опыт (30)") && home2.runeCount(cunning.key) == 7L)
    },

    test("забрать: пятёрки — большими, остаток — малыми; места мало — берёт, что влезло, остальное на алтаре") {
      for {
        t <- make(hero(), Nil, cottage.withRune(cunning.key, 13L))
        (state, dao, inv, r) = t
        _    <- state.action(testUser, tap("AltarTake", "k" -> cunning.key, "rp" -> "0"), r)
        home <- homeOf(dao)
        got   = inv.snapshot.flatMap(_.runeStone)
        said <- texts(r)
        // сумка почти полна: под большие — одно место
        t2 <- make(hero(), (1L to 19L).map(sword).toList, cottage.withRune(cunning.key, 13L))
        (state2, dao2, inv2, r2) = t2
        _     <- state2.action(testUser, tap("AltarTake", "k" -> cunning.key, "rp" -> "0"), r2)
        home2 <- homeOf(dao2)
        got2   = inv2.snapshot.flatMap(_.runeStone)
        said2 <- texts(r2)
      } yield assertTrue(got.count(_.size == RuneStoneSize.Big) == 2 && got.count(_.size == RuneStoneSize.Small) == 3) &&
              assertTrue(got.forall(_.runeKey == cunning.key) && inv.snapshot.forall(_.id > 0L)) &&
              assertTrue(home.runeCount(cunning.key) == 0L && !home.runes.contains(cunning.key)) &&
              assertTrue(said.contains("В сумку легли: больших рун — 2, малых — 3.")) &&
              assertTrue(got2.count(_.size == RuneStoneSize.Big) == 1 && got2.count(_.size == RuneStoneSize.Small) == 3) &&
              assertTrue(home2.runeCount(cunning.key) == 5L && said2.contains("осталось на алтаре: 5"))
    },

    test("«Назад» с экрана руны — на ту страницу списка, с которой её выбрали") {
      for {
        t <- make(hero(), Nil, cottage.withRune(cunning.key, 3L))
        (state, _, _, r) = t
        _      <- state.action(testUser, tap("AltarRune", "k" -> cunning.key, "rp" -> "2"), r)
        screen <- last(r)
      } yield assertTrue(screen.choices.find(_.id == "AltarMenu").exists(_.data == Map("p" -> "2"))) &&
              assertTrue(screen.choices.filter(_.id != "AltarMenu").forall(_.data.get("rp").contains("2")))
    }
  )
}
