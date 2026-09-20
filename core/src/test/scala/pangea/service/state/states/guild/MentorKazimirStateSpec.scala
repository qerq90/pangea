package pangea.service.state.states.guild

import pangea.engine.{ChoiceColor, SceneContent}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.rune.{Rune, RuneData}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Казимир: клеймо (нанести, проверки, переклеймовать), понимание (по одной
  * вещи и «Сдать всё» с настройкой редкостей), экран рун. */
object MentorKazimirStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def weapon(id: Long, skill: Skill, rarity: Rarity = Rarity.Gray): Item =
    Item(id, s"Меч $id", 1L, rarity, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Weapon(skill))
  private def chest(id: Long, skill: Skill, rarity: Rarity = Rarity.Gray): Item =
    weapon(id, skill, rarity).copy(itemType = ItemType.ChestPlate, details = ItemDetails.Armor(skill))
  private def helmet(id: Long, kind: PassiveKind, rarity: Rarity = Rarity.Gray): Item =
    Item(id, s"Шлем $id", 1L, rarity, ItemType.Helmet, attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0,
      details = ItemDetails.Passive(kind))

  private val cunning = Rune.Active(Skill.CunningStrike)
  private val ram     = Rune.Active(Skill.Ram)
  private val healer  = Rune.Passive(PassiveKind.Healer)

  private def hero(rep: Long = 5000L, runes: RuneData = RuneData.empty, lvl: Long = 1L): Hero =
    TestFixtures.hero(userId).copy(guildReputation = rep, runes = runes, lvl = lvl)

  private def make(h: Hero, items: List[Item], inv: Option[TestInventoryRepository] = None) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      repo = inv.getOrElse(TestInventoryRepository.withItems(items))
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (MentorKazimirState(dao, repo, c), dao, repo, r)

  private def heroOf(dao: TestHeroDao): Task[Hero] = dao.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Казимир")(

    test("вход: четыре кнопки; меню клейма показывает места и цену") {
      for {
        t <- make(hero(), Nil)
        (state, _, _, r) = t
        _    <- state.enter(testUser, r)
        menu <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("Brand"), r)
        brand <- r.sentScreens.map(_.last)
      } yield assertTrue(menu.text.contains("Ха-ха-ха") && menu.choices.map(_.id) == List("Brand", "Deepen", "Runes", "LeaveMentorKazimir")) &&
              assertTrue(brand.text.contains("Боевые руны на теле: 0/2") && brand.text.contains("Пассивные руны на теле: 0/4") &&
                brand.text.contains("500 репутации")) &&
              assertTrue(brand.choices.map(_.id) == List("BrandList", "BrandList", "Rebrand", "KazimirMenu"))
    },

    test("список боевых рун — только те, что на вещах в сумке и ещё не на теле; выбор руны — её вещи с ценой") {
      val items = List(weapon(1L, Skill.CunningStrike), weapon(2L, Skill.CunningStrike, Rarity.Blue), chest(3L, Skill.Ram), helmet(4L, PassiveKind.Healer))
      for {
        t <- make(hero(runes = RuneData.empty.brand(ram)), items)
        (state, _, _, r) = t
        _    <- state.action(testUser, tap("BrandList", "kind" -> "a", "p" -> "0"), r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("BrandRune", "k" -> cunning.key, "p" -> "0"), r)
        pick <- r.sentScreens.map(_.last)
      } yield assertTrue(list.choices.filter(_.id == "BrandRune").map(_.label) == List("Хитрый удар")) &&
              assertTrue(pick.text.contains("Хитрый удар") && pick.text.contains("стоить 1000 репутации")) &&
              assertTrue(pick.choices.filter(_.id == "BrandItem").map(_.data("id")) == List("1", "2"))
    },

    test("клеймо: вещь сгорает, репутация уходит, руна на теле, в бою — своя кнопка; повторно — «уже на тебе»") {
      for {
        t <- make(hero(rep = 600L), List(weapon(1L, Skill.CunningStrike), weapon(2L, Skill.CunningStrike)))
        (state, dao, inv, r) = t
        res   <- state.action(testUser, tap("BrandItem", "k" -> cunning.key, "id" -> "1"), r)
        after <- heroOf(dao)
        all   <- texts(r)
        _     <- state.action(testUser, tap("BrandItem", "k" -> cunning.key, "id" -> "2"), r)
        again <- r.sentScreens.map(_.last)
      } yield assertTrue(res == StateType.MentorKazimir && all.contains("Узор лёг ровно")) &&
              assertTrue(inv.snapshot.map(_.id) == List(2L) && after.guildReputation == 100L) &&
              assertTrue(after.runes.isBranded(cunning) && after.runes.nextCost == 1000L) &&
              assertTrue(after.activeSkillSlots.map(_.skill) == List(Skill.CunningStrike) && after.activeSkillSlots.forall(s => Rune.isBodySlot(s.itemId))) &&
              assertTrue(again.text.contains("уже на тебе") && inv.snapshot.map(_.id) == List(2L))
    },

    test("проверки: мало репутации — отказ; нет места под третью боевую — отказ; вещь без руны — «уже нет»") {
      val two = RuneData.empty.brand(ram).brand(Rune.Active(Skill.Bleeding))
      for {
        t <- make(hero(rep = 100L), List(weapon(1L, Skill.CunningStrike)))
        (state, _, inv, r) = t
        _    <- state.action(testUser, tap("BrandItem", "k" -> cunning.key, "id" -> "1"), r)
        poor <- r.sentScreens.map(_.last)
        t2 <- make(hero(runes = two), List(weapon(1L, Skill.CunningStrike)))
        (state2, _, inv2, r2) = t2
        _    <- state2.action(testUser, tap("BrandItem", "k" -> cunning.key, "id" -> "1"), r2)
        full <- r2.sentScreens.map(_.last)
        _    <- state2.action(testUser, tap("BrandItem", "k" -> cunning.key, "id" -> "77"), r2)
        gone <- r2.sentScreens.map(_.last)
      } yield assertTrue(poor.text.contains("Нужно 500 репутации") && inv.snapshot.size == 1) &&
              assertTrue(full.text.contains("нет места") && inv2.snapshot.size == 1) &&
              assertTrue(gone.text.contains("уже нет"))
    },

    test("пассивное клеймо: руна действует без вещи; Тайник на теле расширяет сумку") {
      for {
        t <- make(hero(), List(helmet(1L, PassiveKind.Healer), helmet(2L, PassiveKind.Stash)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, tap("BrandItem", "k" -> healer.key, "id" -> "1"), r)
        _     <- state.action(testUser, tap("BrandItem", "k" -> Rune.Passive(PassiveKind.Stash).key, "id" -> "2"), r)
        after <- heroOf(dao)
        bag   <- inv.get(after.id)
      } yield assertTrue(after.passives.kinds == Set[PassiveKind](PassiveKind.Healer, PassiveKind.Stash) && after.passives.healMult == 1.1) &&
              assertTrue(after.runes.passive == List("Healer", "Stash") && bag.maxItems == 30L)
    },

    test("переклеймовать: список (боевые красные, пассивные синие), подтверждение, снятие — понимание остаётся, цена падает") {
      val data = RuneData.empty.brand(cunning).brand(healer).copy(understanding = Map(cunning.key -> 12L))
      for {
        t <- make(hero(runes = data), Nil)
        (state, dao, _, r) = t
        _    <- state.action(testUser, tap("Rebrand"), r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("RebrandRune", "k" -> cunning.key), r)
        ask  <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("RebrandYes", "k" -> cunning.key), r)
        after <- heroOf(dao)
        all  <- texts(r)
      } yield assertTrue(list.choices.filter(_.id == "RebrandRune").map(c => (c.label, c.color)) ==
                List(("Хитрый удар", ChoiceColor.Negative), ("Целитель", ChoiceColor.Primary))) &&
              assertTrue(ask.text.contains("переклеймовать руну «Хитрый удар»") && ask.choices.map(_.id) == List("RebrandYes", "Rebrand")) &&
              assertTrue(all.contains("Кожа чистая")) &&
              assertTrue(!after.runes.isBranded(cunning) && after.runes.isBranded(healer)) &&
              assertTrue(after.runes.understandingOf(cunning) == 12L && after.runes.nextCost == 1000L)
    },

    test("углубить: любая руна с вещами в сумке; очки по редкости; потолок 30·уровень — вещь остаётся") {
      val items = List(weapon(1L, Skill.CunningStrike, Rarity.Blue), weapon(2L, Skill.CunningStrike, Rarity.Orange),
                       weapon(3L, Skill.CunningStrike, Rarity.Orange), weapon(4L, Skill.CunningStrike, Rarity.Orange), helmet(5L, PassiveKind.Healer))
      for {
        t <- make(hero(), items) // клейма нет — понимание всё равно растёт
        (state, dao, inv, r) = t
        _     <- state.action(testUser, tap("DeepenList", "p" -> "0"), r)
        list  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("DeepenRune", "k" -> cunning.key, "p" -> "0"), r)
        rune  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("DeepenItem", "k" -> cunning.key, "id" -> "1"), r)
        h1    <- heroOf(dao)
        _     <- state.action(testUser, tap("DeepenItem", "k" -> cunning.key, "id" -> "2"), r)
        _     <- state.action(testUser, tap("DeepenItem", "k" -> cunning.key, "id" -> "3"), r)
        _     <- state.action(testUser, tap("DeepenItem", "k" -> cunning.key, "id" -> "4"), r)
        h2    <- heroOf(dao)
        all   <- texts(r)
      } yield assertTrue(list.choices.filter(_.id == "DeepenRune").map(_.label) == List("Хитрый удар", "Целитель")) &&
              assertTrue(rune.text.contains("Понимание: 0/30")) &&
              assertTrue(h1.runes.understandingOf(cunning) == 2L && all.contains("стало глубже: 2 (+2)")) &&
              // 2 + 5 + 5 = 12, ещё 5 → 17: все влезли в потолок 30; ничего не осталось лишним
              assertTrue(h2.runes.understandingOf(cunning) == 17L && inv.snapshot.map(_.id) == List(5L))
    },

    test("у потолка вещь не сгорает и понимание не растёт") {
      val data = RuneData.empty.copy(understanding = Map(cunning.key -> 30L))
      for {
        t <- make(hero(runes = data), List(weapon(1L, Skill.CunningStrike)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, tap("DeepenItem", "k" -> cunning.key, "id" -> "1"), r)
        after <- heroOf(dao)
        last  <- r.sentScreens.map(_.last)
      } yield assertTrue(last.text.contains("Подрасти") && inv.snapshot.size == 1 && after.runes.understandingOf(cunning) == 30L)
    },

    test("сдать всё: по настройке редкостей (серые и белые по умолчанию), с потолком, итог по рунам; настройка переключается") {
      val items = List(weapon(1L, Skill.CunningStrike), weapon(2L, Skill.CunningStrike, Rarity.White), weapon(3L, Skill.CunningStrike, Rarity.Blue),
                       chest(4L, Skill.Ram), helmet(5L, PassiveKind.Healer, Rarity.Orange), weapon(6L, Skill.Bleeding, Rarity.Green))
      for {
        t <- make(hero(), items)
        (state, dao, inv, r) = t
        _     <- state.action(testUser, tap("BurnAll"), r)
        ask   <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("BurnAllYes"), r)
        after <- heroOf(dao)
        all   <- texts(r)
        _     <- state.action(testUser, tap("BurnSettings"), r)
        set   <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("BurnRarity", "g" -> "Blue"), r)
        set2  <- r.sentScreens.map(_.last)
        later <- heroOf(dao)
      } yield assertTrue(ask.text.contains("В огонь пойдёт вещей: 3")) &&
              assertTrue(inv.snapshot.map(_.id).sorted == List(3L, 5L, 6L)) &&
              assertTrue(after.runes.understandingOf(cunning) == 2L && after.runes.understandingOf(ram) == 1L) &&
              assertTrue(all.contains("«Хитрый удар»: +2, понимание 2.") && all.contains("«Таран»: +1, понимание 1.")) &&
              assertTrue(set.choices.find(_.data.get("g").contains("Gray")).exists(_.color == ChoiceColor.Positive)) &&
              assertTrue(set.choices.find(_.data.get("g").contains("Blue")).exists(_.color == ChoiceColor.Negative)) &&
              assertTrue(set2.choices.find(_.data.get("g").contains("Blue")).exists(_.color == ChoiceColor.Positive) && later.runes.burns(Rarity.Blue))
    },

    test("сдать всё: у потолка лишнее остаётся в сумке, сжигать нечего — сообщение") {
      val data = RuneData.empty.copy(understanding = Map(cunning.key -> 29L))
      for {
        t <- make(hero(runes = data), List(weapon(1L, Skill.CunningStrike), weapon(2L, Skill.CunningStrike)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, tap("BurnAllYes"), r)
        after <- heroOf(dao)
        all   <- texts(r)
        _     <- state.action(testUser, tap("BurnAll"), r)
        none  <- r.sentScreens.map(_.last)
      } yield assertTrue(inv.snapshot.size == 1 && after.runes.understandingOf(cunning) == 30L) &&
              assertTrue(all.contains("оставил тебе") && none.text.contains("Сжигать нечего"))
    },

    test("экран «Руны»: боевые первыми, пометки «на теле» / «на вещи» / обе, понимание и сила") {
      val data = RuneData.empty.brand(cunning).brand(healer).copy(understanding = Map(cunning.key -> 1000L))
      val eq   = TestFixtures.emptyEquipment.copy(weapon = weapon(9L, Skill.CunningStrike), helmet = helmet(8L, PassiveKind.Stealthy))
      for {
        t <- make(hero(runes = data, lvl = 40L).copy(equipment = eq), Nil)
        (state, _, _, r) = t
        _    <- state.action(testUser, tap("Runes", "p" -> "0"), r)
        view <- r.sentScreens.map(_.last)
        i1    = view.text.indexOf("⚔ Хитрый удар (на теле и на вещи)")
        i2    = view.text.indexOf("🔹 Целитель (на теле)")
        i3    = view.text.indexOf("🔹 Скрытность (на вещи)")
      } yield assertTrue(i1 >= 0 && i2 > i1 && i3 > i2) &&
              assertTrue(view.text.contains("Понимание: 1000/1200, сила +100,0%.")) &&
              assertTrue(view.choices.map(_.id) == List("KazimirMenu"))
    }
  )
}
