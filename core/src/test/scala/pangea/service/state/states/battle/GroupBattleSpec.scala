package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.{MonsterEnergy, Skill}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Групповой бой: герой под номером 1 против строя мобов. В паре — активный,
  * сосед под номером 2 достаёт героя сбоку, дальние копят и лечат. */
object GroupBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  private def hero(atk: Long = 20L, hp: Long = 500000L): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = lvl,
      fightStats = FightStats(atk = atk, hp = hp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000))

  private def monster(hp: Long, atk: Long = 20L): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 9999,
                 energy = MonsterEnergy.maxEnergy(lvl)))

  /** Герой с умением на оружии (id 101) и на кирасе (id 202), с запасом энергии. */
  private def heroWithSkills(weaponSkill: Skill, chestSkill: Skill): Hero = {
    val weapon = Item(101L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Weapon(weaponSkill))
    val chest  = Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(chestSkill))
    val h = hero()
    h.copy(
      fightStats = h.fightStats.copy(energy = 10_000L),
      baseStats  = h.baseStats.copy(str = 50, int = 50, vit = 50, agi = 50),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon, chestPlate = chest))
  }

  /** Строй: активный и остальные, все с нулевой стартовой энергией (умений нет). */
  private def group(hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster(_)), hero(), Nil)

  /** Тот же строй, но слоты умений — от героя с умениями. */
  private def groupFor(h: Hero, hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster(_)), h, Nil)

  /** Умения готовы сразу — без стартового кд. */
  private def ready(b: SoloPveBattle): SoloPveBattle = b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0)))

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Раунд без сюрпризов: герой попал (60), моб в паре бьёт (99 — герою с его
    * запасом HP всё равно), дальше `extra`: обычно броски соседей и подкрепления
    * (99 — сосед бьёт, подкрепление не пришло). Промах — это бросок не выше 5. */
  private def quietRound(extra: Int*) =
    TestRandom.feedInts(60 +: 99 +: extra: _*) *> TestRandom.feedLongs(100L, 100L, 100L)

  override def spec = suite("Групповой бой")(

    test("экран группы: активный в паре с героем, остальные — одной строкой каждый") {
      for {
        t <- makeState(hero(), group(1000L, 1000L, 1000L))
        (state, _, r) = t
        _       <- quietRound(99, 99)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        screens <- r.sentScreens.map(_.map(_.text))
        summary  = screens.find(_.contains("VS")).getOrElse("")
      } yield assertTrue(summary.linesIterator.count(_.contains("🔴")) == 3) &&
              assertTrue(summary.contains("🟢 Вы VS 🔴")) &&
              assertTrue(summary.contains("❤")) && assertTrue(summary.contains("🧥"))
    },

    test("вход в групповой бой показывает строй перед экраном боя") {
      for {
        t <- makeState(hero(), group(1000L, 1000L))
        (state, _, r) = t
        _       <- state.enter(testUser, r)
        screens <- r.sentScreens.map(_.map(_.text))
      } yield assertTrue(screens.size == 2) &&
              assertTrue(screens.head.contains("🟢 Вы VS 🔴")) &&
              assertTrue(screens.head.linesIterator.count(_.contains("🔴")) == 2)
    },

    test("сосед под номером 2 бьёт героя сбоку, дальний под номером 3 — нет") {
      val h = hero(hp = 100000L)
      for {
        t <- makeState(h, group(1000L, 1000L, 1000L))
        (state, dao, r) = t
        // герой попал, активный промахнулся (1), сосед попал (10), подкрепление нет
        _       <- TestRandom.feedInts(60, 1, 10, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(updated.fightStats.hp < 100000L) &&
              assertTrue(screens.contains("атаковал вас сбоку")) &&
              // третий не достаёт: ни удара, ни промаха с его стороны
              assertTrue(screens.linesIterator.count(_.contains("сбоку")) == 1)
    },

    test("в бою 1 на 1 ничего из этого нет: ни ударов сбоку, ни сводки") {
      for {
        t <- makeState(hero(), SoloPveBattle.from(monster(1000L), hero()))
        (state, _, r) = t
        _       <- quietRound()
        _       <- state.action(testUser, tap("Attack"), r)
        screens <- r.sentScreens.map(_.map(_.text))
      } yield assertTrue(!screens.exists(_.contains("VS"))) &&
              assertTrue(!screens.exists(_.contains("сбоку")))
    },

    test("активный пал — в пару встаёт следующий, бой продолжается, павший ждёт добычи") {
      for {
        t <- makeState(hero(atk = 100000L), group(10L, 1000L))
        (state, dao, r) = t
        _       <- quietRound(99)
        result  <- state.action(testUser, aimed("Attack", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(after.monsterCurrentHp == 1000L) &&
              assertTrue(after.group.others.isEmpty) &&
              assertTrue(after.group.slain.size == 1) &&
              assertTrue(screens.contains("На его место встаёт"))
    },

    test("добивание ударом восстанавливает энергию за раунд — и когда в пару встаёт следующий") {
      // Интеллект 10, ловкость 10 → +15 энергии за раунд; макс. энергии у героя хватает.
      val h = hero(atk = 100000L).copy(fightStats = hero().fightStats.copy(energy = 0L, atk = 100000L))
      for {
        t <- makeState(h, group(10L, 1000L))
        (state, dao, r) = t
        _       <- quietRound(99)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        t2 <- makeState(h, group(10L))
        (state2, dao2, r2) = t2
        _       <- quietRound()
        result  <- state2.action(testUser, tap("Attack"), r2)
        solo    <- dao2.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(after.fightStats.energy == 15L) &&
              assertTrue(result == StateType.Loot && solo.fightStats.energy == 15L)
    },

    test("глоток фляги раунд не завершает: сосед сбоку не бьёт, подкрепление не идёт, раунд не растёт") {
      val flask = Item(1L, "Фляга", 1L, ItemRarity.Gray, ItemType.Flask,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        details = ItemDetails.Flask(pangea.model.item.FlaskEffect.HealPercent(25), charges = 1, maxCharges = 1))
      val h = hero().copy(equipment = TestFixtures.emptyEquipment.copy(flask = flask))
      for {
        t <- makeState(h, group(1000L, 1000L))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(99, 1) // если бы сосед ходил: попадание и подкрепление (1 ≤ 2 — пришло бы)
        _       <- state.action(testUser, tap("UseFlask"), r)
        after   <- battleOf(dao)
        u       <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // пустая фляга — тоже не ход
        _       <- state.action(testUser, tap("UseFlask"), r)
        again   <- battleOf(dao)
      } yield assertTrue(u.fightStats.hp == h.effectiveMaxHp(0L) && after.consumableUsedThisRound) && // полон, никто не ударил
              assertTrue(!screens.contains("атаковал вас сбоку") && !screens.contains("прибежал сородич")) &&
              assertTrue(after.group.round == 0 && after.group.others.size == 1) &&
              assertTrue(again.group.round == 0 && again.group.others.size == 1)
    },

    test("последний моб пал — это победа, добыча за всех") {
      for {
        t <- makeState(hero(atk = 100000L), group(10L))
        (state, _, r) = t
        _      <- quietRound()
        result <- state.action(testUser, tap("Attack"), r)
      } yield assertTrue(result == StateType.Loot)
    },

    test("дальний моб с фляжкой лечит раненого соседа, а не себя") {
      // Третий цел и с энергией на фляжку; второй ранен наполовину.
      val trio = group(1000L, 1000L, 1000L)
      val hurtSecond = trio.group.others.head.copy(currentHp = 500L)
      val richThird  = trio.group.others(1).copy(currentEnergy = MonsterEnergy.maxEnergy(lvl))
      val b = trio.copy(group = trio.group.copy(others = List(hurtSecond, richThird)))
      for {
        t <- makeState(hero(), b)
        (state, dao, r) = t
        _       <- quietRound(99)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(screens.contains("исцелил вашего врага")) &&
              assertTrue(after.group.others.head.currentHp > 500L) &&
              // лекарь заплатил энергией
              assertTrue(after.group.others(1).currentEnergy < MonsterEnergy.maxEnergy(lvl))
    },

    test("подкрепление: на 2% в конце раунда приходит сородич той же расы") {
      for {
        t <- makeState(hero(), group(1000L)) // один моб без энергии — умений не будет
        (state, dao, r) = t
        // герой, моб, подкрепление = 1 (≤ 2 — пришёл)
        _       <- TestRandom.feedInts(60, 99, 1) *> TestRandom.feedLongs(100L, 100L, 7L, 10L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.isGroup) &&
              assertTrue(after.group.others.head.race == Race.Orc.entryName) &&
              assertTrue(screens.contains("прибежал сородич"))
    },

    test("выше десяти мобов подкрепление не приходит") {
      val ten = group(List.fill(10)(1000L): _*)
      for {
        t <- makeState(hero(), ten)
        (state, dao, r) = t
        _     <- quietRound(99, 1)
        _     <- state.action(testUser, aimed("Attack", 1), r)
        after <- battleOf(dao)
      } yield assertTrue(after.group.aliveCount == 10) &&
              assertTrue(after.group.others.size == 9)
    },

    test("после Тарана герой стоит между двумя мобами — бьют сбоку оба, а не только сосед справа") {
      // герой на месте 2 против мобов 1-2-3: соседи — места 1 и 3
      val b = group(1000L, 1000L, 1000L).moveHeroTo(2)
      for {
        t <- makeState(hero(hp = 100000L), b)
        (state, dao, r) = t
        // герой попал, активный мимо (1), сосед слева попал (10), сосед справа попал (10), подкрепления нет
        _       <- TestRandom.feedInts(60, 1, 10, 10, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Attack", 2), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(screens.linesIterator.count(_.contains("атаковал вас сбоку")) == 2) &&
              assertTrue(after.group.heroPos == 2) &&
              assertTrue(screens.contains("2. 🟢 Вы VS 🔴"))
    },

    test("каждый четвёртый раунд ряды перемешиваются — никто не пропадает и не двоится") {
      // Три раунда без перемен, на четвёртом — перемешивание. Три моба с разным
      // запасом HP: по нему видно, что после перемешивания это те же трое.
      // Раненый сосед (броня 18 из 100) должен остаться ровно одним и с той же
      // раной — из-за этого и пропал следопыт у игрока.
      // Порядок после перемешивания зависит от сида — гоняем несколько, чтобы
      // проверить и случай, когда в пару встал не первый.
      val trio    = group(1000L, 2000L, 3000L)
      val wounded = trio.group.others.head.copy(currentArmor = 18L, currentHp = 1940L)
      val b = trio.copy(group = trio.group.copy(others = wounded :: trio.group.others.tail, round = 3))
      def run(seed: Long) =
        for {
          t <- makeState(hero(), b)
          (state, dao, r) = t
          _       <- TestRandom.setSeed(seed) *> quietRound(99, 99, 99)
          _       <- state.action(testUser, aimed("Attack", 1), r)
          after   <- battleOf(dao)
          screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
          lineUp   = after.monstersInOrder
          // удар героя этим ходом получил тот, кто стоял в паре ДО перемешивания
          // (1000 HP); раненый сосед (2000) остался с той же раной, дальний (3000) цел
          byMax    = lineUp.map(m => m.stats.hp -> m).toMap
        } yield assertTrue(after.group.round == 4) &&
                assertTrue(screens.contains("Ряды смешались")) &&
                assertTrue(lineUp.map(_.stats.hp).sorted == List(1000L, 2000L, 3000L)) &&
                assertTrue(byMax(1000L).currentHp < 1000L) &&
                assertTrue(byMax(2000L).currentHp == 1940L && byMax(2000L).currentArmor == 18L) &&
                assertTrue(byMax(3000L).currentHp == 3000L)
      ZIO.foreach((1L to 6L).toList)(run).map(_.reduce(_ && _))
    },

    test("бегство: свободные мобы могут окружить — по 5% за каждого") {
      for {
        t <- makeState(hero(), group(1000L, 1000L, 1000L)) // двое свободных → 10%
        (state, dao, r) = t
        // окружение (5 ≤ 10 — окружили)
        _       <- TestRandom.feedInts(5, 99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        result  <- state.action(testUser, tap("ConfirmFlee"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.contains("окружает вас")) &&
              assertTrue(after.isGroup)
    },

    test("бегство 1 на 1 — окружать некому, бросок на это не тратится") {
      for {
        t <- makeState(hero(), SoloPveBattle.from(monster(1000L), hero()))
        (state, _, r) = t
        _      <- TestRandom.feedInts(99) *> TestRandom.feedLongs(100L)
        result <- state.action(testUser, tap("ConfirmFlee"), r)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("умение с уроном в группе сначала спрашивает цель — ход не тратится") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.MinorHeal)
      for {
        t <- makeState(h, groupFor(h, 1000L, 1000L))
        (state, dao, r) = t
        result  <- state.action(testUser, tap("Skill_101"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens
        ask      = screens.last
        targets  = ask.choices.filter(_.id == "Skill_101").map(_.data.get("target"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(ask.text.contains("в кого?")) &&
              assertTrue(targets == List(Some("1"), Some("2"))) &&
              assertTrue(ask.choices.exists(_.id == "CancelTarget")) &&
              assertTrue(after.monsterCurrentHp == 1000L && after.group.others.head.currentHp == 1000L) &&
              assertTrue(after.group.round == 0)
    },

    test("кнопки выбора цели влезают в лимит подписи даже у отмеченного тьмой с длинным именем") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.MinorHeal)
      // Самое длинное имя в игре, да ещё с меткой тьмы — полностью не влезет.
      val chief = Monster(0L, lvl, Race.Goblin, Rarity.Mythical,
        FightStats(atk = 20, hp = 1000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0),
        marked = true)
      val b = SoloPveBattle.fromGroup(List(chief, chief), h, Nil)
      for {
        t <- makeState(h, b)
        (state, _, r) = t
        _       <- state.action(testUser, tap("Skill_101"), r)
        ask     <- r.sentScreens.map(_.last)
        labels   = ask.choices.filter(_.id == "Skill_101").map(_.label)
      } yield assertTrue(labels.size == 2) &&
              assertTrue(labels.forall(_.length <= pangea.engine.Choice.MaxLabelLength)) &&
              assertTrue(labels.head == "1. Отмеченный тьмой Гоблин ❤ 100%") &&
              assertTrue(chief.name == "Отмеченный тьмой Хобгоблин — предводитель банды")
    },

    test("легендарный в конце первого раунда зовёт 2–3 сородичей 3–4 ранга") {
      val boss = Monster(0L, lvl, Race.Orc, Rarity.Legendary,
        FightStats(atk = 20, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))
      for {
        t <- makeState(hero(), SoloPveBattle.from(boss, hero()))
        (state, dao, r) = t
        // герой попал, моб ответил; подкрепления нет; зов: трое — мифический, редкий, мифический; стартовая энергия 5%
        _       <- TestRandom.feedInts(60, 99, 99, 3, 1, 0, 1) *> TestRandom.feedLongs(100L, 100L, 5L, 5L, 5L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(screens.contains("трубит — на зов сбегаются сородичи")) &&
              assertTrue(after.group.others.map(_.rarity) == List(Rarity.Mythical, Rarity.Rare, Rarity.Mythical).map(_.entryName)) &&
              assertTrue(after.group.others.forall(m => m.race == Race.Orc.entryName && m.lvl == hero().dungeonLevel.toLong)) &&
              assertTrue(after.group.places == List(2, 3, 4) && after.group.queue.isEmpty && after.group.round == 1)
    },

    test("павший в первом же раунде вожак сородичей не зовёт") {
      val chief = Monster(0L, lvl, Race.Orc, Rarity.Mythical,
        FightStats(atk = 20, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))
      val eight = List.fill(8)(monster(1000L))
      val h     = hero(atk = 100000L)
      for {
        t <- makeState(h, SoloPveBattle.fromGroup(chief :: eight, h, Nil))
        (state, dao, r) = t
        // герой добил вожака (ответа нет), в пару шагнул сосед; дальние сбоку не достают; подкрепления нет.
        // Зовут только те, кто стоит в строю в конце раунда, — павший вожак не трубит
        _       <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        noCall  <- battleOf(dao)
      } yield assertTrue(noCall.group.slain.size == 1 && noCall.group.others.size == 7 && noCall.group.queue.isEmpty)
    },

    test("мифический выжил первый раунд — зовёт; одиннадцатому и дальше места нет, они ждут за строем") {
      val chief = Monster(0L, lvl, Race.Orc, Rarity.Mythical,
        FightStats(atk = 20, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))
      val eight = List.fill(8)(monster(1000L))
      for {
        t <- makeState(hero(), SoloPveBattle.fromGroup(chief :: eight, hero(), Nil))
        (state, dao, r) = t
        // герой попал, вожак ответил; сосед на месте 2 бьёт сбоку; подкрепления нет (9 в строю);
        // зов: трое 1–3 ранга — обычный, необычный, редкий; место есть только одному
        _       <- TestRandom.feedInts(60, 99, 99, 99, 3, 0, 1, 2) *> TestRandom.feedLongs(100L, 100L, 100L, 5L, 5L, 5L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.group.aliveCount == 10 && after.group.others.size == 9) &&
              assertTrue(after.group.others.last.rarity == Rarity.Common.entryName &&
                         after.group.queue.map(_.rarity) == List(Rarity.Uncommon, Rarity.Rare).map(_.entryName)) &&
              assertTrue(screens.contains("Ещё 2 не находят места и ждут за строем")) &&
              assertTrue(screens.contains("… и ещё 2 ждут за строем."))
    },

    test("очередь: место освободилось — следующий выходит из-за спин; строй пуст, а очередь нет — победы нет") {
      val slot = SoloPveBattle.fromGroup(List(monster(500L)), hero(), Nil).activeSlot
      val full = (1 to 9).foldLeft(group(1000L))((b, _) => b.withReinforcement(slot))   // 10 в строю
      val withQueue = full.admit(slot.copy(currentHp = 777L)).admit(slot.copy(currentHp = 888L))
      val freed = withQueue.sideFallen(0).admitQueued
      val lone  = group(10L).copy(group = group(10L).group.copy(queue = List(slot.copy(currentHp = 777L)))).copy(monsterCurrentHp = 0L)
      val next  = lone.promoteNext
      assertTrue(full.group.aliveCount == 10 && !full.hasRoom) &&
      assertTrue(withQueue.group.others.size == 9 && withQueue.group.queue.map(_.currentHp) == List(777L, 888L)) &&
      assertTrue(freed._2.map(_.currentHp) == List(777L) && freed._1.group.queue.map(_.currentHp) == List(888L)) &&
      assertTrue(freed._1.group.others.exists(_.currentHp == 777L)) &&
      assertTrue(next.exists(b => b.monsterCurrentHp == 777L && b.group.queue.isEmpty && b.group.slain.size == 1))
    },

    test("лечащее умение цели не спрашивает — ход идёт сразу") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.MinorHeal)
      for {
        t <- makeState(h, groupFor(h, 1000L, 1000L))
        (state, dao, r) = t
        _       <- quietRound(99)
        _       <- state.action(testUser, tap("Skill_202"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(!screens.contains("в кого?")) &&
              assertTrue(after.group.round == 1)
    },

    test("умение по соседу: урон уходит мобу под номером 2, базовая атака — мобу в паре") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.MinorHeal)
      for {
        t <- makeState(h, groupFor(h, 1000L, 1000L))
        (state, dao, r) = t
        // бросок умения (long), базовая атака попала (60), моб мимо, сосед мимо, подкрепления нет
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Skill_101", 2), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.group.others.head.currentHp < 1000L) &&
              assertTrue(after.monsterCurrentHp < 1000L) &&
              assertTrue(after.monsterStats.hp == 1000L && after.group.others.size == 1) &&
              assertTrue(screens.contains("урона")) &&
              assertTrue(!screens.contains("в кого?"))
    },

    test("умение добило соседа — он в павших, бой продолжается с мобом в паре") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.MinorHeal)
      for {
        t <- makeState(h, groupFor(h, 1000L, 5L))
        (state, dao, r) = t
        _       <- quietRound(99) *> TestRandom.feedLongs(100L)
        result  <- state.action(testUser, aimed("Skill_101", 2), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(after.group.others.isEmpty) &&
              assertTrue(after.group.slain.size == 1) &&
              assertTrue(after.monsterStats.hp == 1000L) &&
              assertTrue(screens.contains("пал."))
    },

    test("Размашистый удар по мобу в паре: соседу цели — половина урона, дальний под номером 3 цел") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.MinorHeal)
      for {
        t <- makeState(h, groupFor(h, 1000L, 1000L, 1000L))
        (state, dao, r) = t
        // умение (1·50 + 4·50 + 0.4·20 = 258 по цели при разбросе 100, половина — 129 — её соседу);
        // базовая атака; ответ пары; сосед сбоку; подкрепления нет
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Skill_101", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        byPos    = after.group.entries.toMap
      } yield assertTrue(after.monsterCurrentHp < 742L) &&
              assertTrue(byPos(2).currentHp == 871L) &&
              assertTrue(byPos(3).currentHp == 1000L) &&
              assertTrue(screens.contains("Дуга удара задевает")) &&
              assertTrue(screens.linesIterator.count(_.contains("Дуга удара задевает")) == 1)
    },

    test("Размашистый удар по соседу № 2: дуга задевает обоих его соседей — добила моба в паре и достала № 3, до которого герою не дотянуться") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.MinorHeal)
      for {
        t <- makeState(h, groupFor(h, 100L, 1000L, 1000L))
        (state, dao, r) = t
        // умение по № 2 (258), дуга по № 1 (129 > 100 — пал) и по № 3 (129); базовая атака по № 2;
        // № 2 бьёт сбоку; подкрепления нет
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        result  <- state.action(testUser, aimed("Skill_101", 2), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        byPos    = after.group.entries.toMap
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(after.group.slain.size == 1) &&
              assertTrue(after.monsterStats.hp == 1000L && after.monsterCurrentHp < 742L) &&
              // цель освободилась и шагнула к герою; № 3 остался на своём месте с дугой в боку
              assertTrue(after.group.paired && after.group.heroPos == 1) &&
              assertTrue(byPos.keySet == Set(3) && byPos(3).currentHp == 871L) &&
              assertTrue(screens.linesIterator.count(_.contains("Дуга удара задевает")) == 2 && screens.contains("пал."))
    },

    test("Вихрь клинка: цель и каждый в досягаемости героя получают свой бросок, дальний № 3 цел") {
      val h = heroWithSkills(Skill.BladeWhirl, Skill.MinorHeal)
      for {
        t <- makeState(h, ready(groupFor(h, 1000L, 1000L, 1000L)))
        (state, dao, r) = t
        // умение (1·50 + 3·50 + 0.3·20 = 206 при разбросе 100, защита мобов 0); базовая атака; ответ; сосед; подкрепления нет
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Skill_101", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        byPos    = after.group.entries.toMap
      } yield assertTrue(after.monsterCurrentHp < 794L) &&
              assertTrue(byPos(2).currentHp == 794L) &&
              assertTrue(byPos(3).currentHp == 1000L) &&
              assertTrue(screens.contains("вихре стали") && screens.contains("Вихрь настигает"))
    },

    test("Веерный порез: урон и кровь цели и соседу героя") {
      val h = heroWithSkills(Skill.FanCut, Skill.MinorHeal)
      for {
        t <- makeState(h, ready(groupFor(h, 10000L, 10000L)))
        (state, dao, r) = t
        // умение (2·50 + 3·50 + 0.2·9999 = 2249 при разбросе 100); базовая атака; ответ; сосед; подкрепления нет
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Skill_101", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        side     = after.group.others.head
      } yield assertTrue(after.monsterCurrentHp < 10000L - 2249L) &&
              assertTrue(after.effects.monsterBleed.exists(_.pct == 2)) &&
              // порез 2249 и первый тик крови (2% от 10000) в фазе мобов вне пары
              assertTrue(side.currentHp == 10000L - 2249L - 200L) &&
              assertTrue(side.effects.monsterBleed.exists(_.pct == 2)) &&
              assertTrue(screens.contains("пошла кровь") && screens.contains("Порез задевает"))
    },

    test("Боевой клич: цели не спрашивает, всем в досягаемости — дебаф защиты и запертое умение, потом базовая атака") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.BattleCry)
      for {
        t <- makeState(h, ready(groupFor(h, 1000L, 1000L, 1000L)))
        (state, dao, r) = t
        // клич (0.2·50 + 0.1·0 = 10% при разбросе 100); базовая атака; ответ; сосед; подкрепления нет
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, tap("Skill_202"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        byPos    = after.group.entries.toMap
      } yield assertTrue(!screens.contains("в кого?")) &&
              assertTrue(screens.contains("слабеет на 10% на 2 хода")) &&
              // к концу раунда дебафы оттикали по разу: 2 → 1 хода, замок 2 → 1
              assertTrue(after.effects.monsterDefenceDebuff.contains(pangea.model.battle.TimedDefenceDebuff(10, 1))) &&
              assertTrue(after.effects.monsterSkillBlockedTurns == 1) &&
              assertTrue(byPos(2).effects.monsterDefenceDebuff.contains(pangea.model.battle.TimedDefenceDebuff(10, 1))) &&
              assertTrue(byPos(2).effects.monsterSkillBlockedTurns == 1) &&
              // № 3 не в досягаемости — его клич не задел
              assertTrue(byPos(3).effects.monsterDefenceDebuff.isEmpty && byPos(3).effects.monsterSkillBlockedTurns == 0) &&
              assertTrue(after.monsterCurrentHp < 1000L)
    },

    test("Отбросить: выживший отлетает в конец строя, последний выходит на его место и встаёт в пару") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.Shove)
      for {
        t <- makeState(h, groupFor(h, 1000L, 2000L, 3000L))
        (state, dao, r) = t
        // умение по № 1 (0.5·0 + 0.5·50 + 2·50 = 125); базовая атака уже по вышедшему № 3; ответ; сосед; подкрепления нет
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Skill_202", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        byPos    = after.group.entries.toMap
      } yield assertTrue(after.group.paired && after.monsterStats.hp == 3000L && after.monsterCurrentHp < 3000L) &&
              assertTrue(byPos(3).stats.hp == 1000L && byPos(3).currentHp == 875L) &&
              assertTrue(byPos(2).currentHp == 2000L) &&
              assertTrue(screens.contains("отшвыриваете") && screens.contains("отлетает в конец строя"))
    },

    test("Отбросить последнего (или единственного) — просто удар, строй не меняется") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.Shove)
      for {
        t <- makeState(h, groupFor(h, 1000L))
        (state, dao, r) = t
        _       <- quietRound(99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, tap("Skill_202"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.group.paired && after.monsterCurrentHp < 875L) &&
              assertTrue(screens.contains("отшвыриваете") && !screens.contains("отлетает в конец строя"))
    },

    test("Таран по соседу: урон сразу, а в конце раунда он встаёт в пару") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.Ram)
      for {
        t <- makeState(h, groupFor(h, 1000L, 2000L))
        (state, dao, r) = t
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Skill_202", 2), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.monsterStats.hp == 2000L) &&
              assertTrue(after.monsterCurrentHp < 2000L) &&
              assertTrue(after.group.others.head.stats.hp == 1000L) &&
              assertTrue(after.group.pendingMove.isEmpty) && assertTrue(after.group.heroPos == 2) &&
              assertTrue(screens.contains("Таран сработал"))
    },

    test("Таран по мобу в паре — обычный удар, пары не меняет") {
      val h = heroWithSkills(Skill.SweepingStrike, Skill.Ram)
      for {
        t <- makeState(h, groupFor(h, 1000L, 2000L))
        (state, dao, r) = t
        _       <- quietRound(99, 99) *> TestRandom.feedLongs(100L)
        _       <- state.action(testUser, aimed("Skill_202", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.monsterStats.hp == 1000L) &&
              assertTrue(after.monsterCurrentHp < 1000L) &&
              assertTrue(after.group.others.head.currentHp == 2000L) &&
              assertTrue(!screens.contains("Таран сработал"))
    },

    test("победа над группой: опыт одной суммой за всех, добыча — по каждому павшему по очереди") {
      val h = hero(atk = 100000L)
      for {
        t <- makeState(h, group(10L, 10L))
        (state, dao, r) = t
        // раунд 1: №1 пал, №2 встал в пару, подкрепления нет; раунд 2: №2 пал
        _       <- TestRandom.feedInts(60, 99, 60) *> TestRandom.feedLongs(100L, 100L)
        first   <- state.action(testUser, aimed("Attack", 1), r)
        second  <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        loot    <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        perMob   = (h.dungeonLevel.toLong * Rarity.Common.factor).toLong.max(1L)
      } yield assertTrue(first == StateType.Battle && second == StateType.Loot) &&
              assertTrue(updated.exp == perMob * 2L) &&
              assertTrue(screens.contains("Все повержены")) &&
              assertTrue(loot.monsterName.isDefined) &&
              assertTrue(loot.queue.size == 1)
    },

    test("победа 1 на 1: добыча без очереди и без имени над ней") {
      for {
        t <- makeState(hero(atk = 100000L), SoloPveBattle.from(monster(10L), hero()))
        (state, dao, r) = t
        _      <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        result <- state.action(testUser, tap("Attack"), r)
        loot   <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(loot.monsterName.isEmpty && loot.queue.isEmpty)
    },

    test("удар сбоку, добивший героя, виден в логе смерти") {
      val h = hero(hp = 1L)
      for {
        t <- makeState(h, group(1000L, 1000L))
        (state, _, r) = t
        // герой попал, активный мимо (1), сосед попал (10) и добил
        _       <- TestRandom.feedInts(60, 1, 10) *> TestRandom.feedLongs(100L, 100L)
        result  <- state.action(testUser, aimed("Attack", 1), r)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Death) &&
              assertTrue(screens.contains("атаковал вас сбоку"))
    }
  )
}
