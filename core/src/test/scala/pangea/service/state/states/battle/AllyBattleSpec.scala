package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Бой с отрядом: союзник на позиции N стоит против врага на месте N, бьёт его
  * (или моба в паре с героем, если тот по соседству), моб напротив бьёт его, а
  * не героя; умения — случайные из доступных; обнулённый уходит по свитку и
  * после боя сутки в отлучке. */
object AllyBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  /** Йорген 10-го уровня: HP 1250, броня 1500, атака 200, энергия 1000. */
  private def ally(kind: AllyKind = AllyKind.Human, pos: Int = 2, energy: Long = 0L,
                   hp: Option[Long] = None, armor: Option[Long] = None): Ally = {
    val s = kind.stats(lvl)
    Ally(kind, pos, hp.getOrElse(s.hp), armor.getOrElse(s.armor), energy)
  }

  private def hero(atk: Long = 20L, hp: Long = 500000L, allies: List[Ally] = List(ally()), heroPos: Int = 1): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = lvl,
      fightStats = FightStats(atk = atk, hp = hp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      squad      = Squad(heroPos = heroPos, allies = allies))

  private def monster(hp: Long, atk: Long = 20L): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  private def heroWithSkills(h: Hero, weaponSkill: Skill, chestSkill: Skill): Hero = {
    val weapon = Item(101L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Weapon(weaponSkill))
    val chest  = Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(chestSkill))
    h.copy(
      fightStats = h.fightStats.copy(energy = 10_000L),
      baseStats  = h.baseStats.copy(str = 50, int = 50, vit = 50, agi = 50),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon, chestPlate = chest))
  }

  private def group(h: Hero, hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster(_)), h, Nil)

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  override def spec = suite("Бой с отрядом")(

    test("строй двухрядный: герой и союзник слева по позициям, мобы справа по местам") {
      for {
        t <- makeState(hero(), group(hero(), 1000L, 1000L))
        (state, _, r) = t
        _       <- state.enter(testUser, r)
        screens <- r.sentScreens
        first    = screens.head.text
        buttons  = screens.last.choices.map(_.label)
      } yield assertTrue(first.contains("1. 🟢 Вы VS 🔴")) &&
              assertTrue(first.contains("2. 🔵 Йорген Кремень ❤ 100% 🧥 100% VS 🔴")) &&
              assertTrue(buttons.contains("⚔ Место 2"))
    },

    test("союзник бьёт врага напротив и добивает его — павший ждёт добычи, бой идёт") {
      for {
        t <- makeState(hero(), group(hero(), 1000L, 10L))
        (state, dao, r) = t
        // герой попал, моб в паре ответил; союзник попал (добил — прока нет); подкрепления нет
        _       <- TestRandom.feedInts(60, 99, 60, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        result  <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.contains("⚔ Йорген Кремень бьёт ")) &&
              assertTrue(screens.contains("Йорген Кремень добивает")) &&
              assertTrue(after.group.slain.size == 1 && after.group.others.isEmpty) &&
              assertTrue(after.monsterCurrentHp < 1000L && after.monsterCurrentHp > 0L) &&
              assertTrue(after.group.allies.exists(_.kind == AllyKind.Human))
    },

    test("союзник добивает моба в паре с героем — это победа героя: опыт и добыча ему") {
      val h = hero(atk = 1L)
      for {
        t <- makeState(h, SoloPveBattle.from(monster(10L), h))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 99, 60) *> TestRandom.feedLongs(100L, 100L, 100L)
        result  <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(updated.exp > 0L) &&
              assertTrue(updated.squad.has(AllyKind.Human))
    },

    test("умение — случайное из доступных: целому и с энергией достаются только удары, второй — дробящий прямо в HP") {
      val h = hero(allies = List(ally(energy = 1000L)))
      for {
        t <- makeState(h, SoloPveBattle.from(monster(100000L), h))
        (state, dao, r) = t
        // герой; моб; удар союзника + прок; выбор умения (1 из [быстрый, дробящий]); дробящий + прок; подкрепление
        _       <- TestRandom.feedInts(60, 99, 60, 99, 1, 60, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        a        = after.group.allies.head
      } yield assertTrue(screens.contains("бьёт плашмя прямо по голове, нанеся 80 урона")) &&
              assertTrue(a.energy == 1000L - 160L + 70L)
    },

    test("раненому доступны фляга и починка: второй вариант — фляга на 30% HP") {
      val h = hero(allies = List(ally(energy = 1000L, hp = Some(500L), armor = Some(500L))))
      for {
        t <- makeState(h, SoloPveBattle.from(monster(100000L), h))
        (state, dao, r) = t
        // выбор 1 из [быстрый, фляга, починка, дробящий] — фляга
        _       <- TestRandom.feedInts(60, 99, 60, 99, 1, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        a        = after.group.allies.head
      } yield assertTrue(screens.contains("выпивает из своей фляги и восстанавливает 375 хп")) &&
              assertTrue(a.hp == 875L) &&
              assertTrue(a.energy == 1000L - 80L + 70L)
    },

    test("моб напротив союзника бьёт его, а не героя сбоку") {
      for {
        t <- makeState(hero(), group(hero(), 1000L, 1000L))
        (state, dao, r) = t
        // герой; моб в паре; союзник + прок; моб № 2 бьёт союзника; подкрепление
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        a        = after.group.allies.head
      } yield assertTrue(screens.contains("бьёт Йорген Кремень на")) &&
              assertTrue(!screens.contains("сбоку")) &&
              assertTrue(a.armor < 1500L && a.hp == 1250L) &&
              // герой получил только от моба в паре
              assertTrue(updated.fightStats.hp < 500000L)
    },

    test("обнулённый союзник уходит по свитку; после боя его нет в отряде и он сутки в отлучке") {
      val h = hero(allies = List(ally(hp = Some(1L), armor = Some(0L))))
      for {
        t <- makeState(h, group(h, 1000L, 1000L))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // бежим: окружения нет (99 > 5%)
        _       <- TestRandom.feedInts(99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        fled    <- state.action(testUser, tap("ConfirmFlee"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        now     <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      } yield assertTrue(screens.contains("Йорген Кремень воспользовался свитком и телепортировался с боя")) &&
              assertTrue(after.group.allies.isEmpty && after.group.alliesGone == List("Human")) &&
              assertTrue(fled == StateType.Dungeon) &&
              assertTrue(!updated.squad.has(AllyKind.Human)) &&
              assertTrue(updated.squad.isAway(AllyKind.Human, now)) &&
              assertTrue(!updated.squad.isAway(AllyKind.Human, now + 24L * 60L * 60L * 1000L))
    },

    test("состояние союзника после боя сохраняется в отряде") {
      for {
        t <- makeState(hero(atk = 100000L), group(hero(), 1000L, 1000L))
        (state, dao, r) = t
        // раунд: герой добил активного (ответа нет), к нему шагнул № 2 (союзнику напротив —
        // никого); союзник бьёт активного как соседа + прок; подкрепления нет. Раунд 2: герой добивает.
        _       <- TestRandom.feedInts(60, 60, 99, 99, 60) *> TestRandom.feedLongs(100L, 100L, 100L)
        first   <- state.action(testUser, tap("Attack"), r)
        second  <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(first == StateType.Battle && second == StateType.Loot) &&
              assertTrue(updated.squad.allyAt(2).exists(a => a.kind == AllyKind.Human && a.energy == 70L))
    },

    test("герой бьёт соседа кнопкой «⚔ Место 2»: урон мобу на месте 2, отвечает моб в паре") {
      for {
        t <- makeState(hero(), group(hero(), 1000L, 1000L))
        (state, dao, r) = t
        // удар по соседу; ответ моба в паре; союзник + прок; моб № 2 бьёт союзника; подкрепление
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Attack", 2), r)
        after   <- battleOf(dao)
      } yield assertTrue(after.monsterCurrentHp == 1000L) &&
              assertTrue(after.group.others.head.currentHp < 1000L) &&
              assertTrue(after.group.heroPos == 1)
    },

    test("лечение в отряде спрашивает цель: «Себе» и союзники; по союзнику — лечит его") {
      val wounded = ally(hp = Some(100L))
      val h = heroWithSkills(hero(allies = List(wounded)), Skill.SweepingStrike, Skill.MinorHeal)
      for {
        t <- makeState(h, SoloPveBattle.from(monster(100000L), h))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("Skill_202"), r)
        ask     <- r.sentScreens.map(_.last)
        targets  = ask.choices.filter(_.id == "Skill_202").map(c => c.label -> c.data.get("target"))
        // умение + базовая атака; моб; союзник + прок; подкрепление
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Skill_202", 2), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        a        = after.group.allies.head
      } yield assertTrue(targets.head == ("Себе" -> Some("1"))) &&
              assertTrue(targets(1)._2.contains("2") && targets(1)._1.startsWith("2. Йорген Кремень")) &&
              assertTrue(a.hp > 100L) &&
              assertTrue(screens.contains("«Малое исцеление» — Йорген Кремень: +"))
    },

    test("Таран на позицию союзника: герой встаёт туда, союзник — на прежнее место героя") {
      val h = heroWithSkills(hero(), Skill.SweepingStrike, Skill.Ram)
      for {
        t <- makeState(h, group(h, 1000L, 2000L))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Skill_202", 2), r)
        after   <- battleOf(dao)
      } yield assertTrue(after.group.heroPos == 2) &&
              assertTrue(after.group.allies.head.position == 1) &&
              assertTrue(after.monsterStats.hp == 2000L)
    },

    test("минибосс: союзник на позиции 2 бьёт босса как соседа") {
      val wolf    = MiniBoss.WhiteWolf
      val bossLvl = 2L
      val h = hero(atk = 100L).copy(lvl = 9L)
      val monsterW = Monster(0L, bossLvl, wolf.race, Rarity.Legendary, wolf.stats(bossLvl))
      val b = SoloPveBattle.from(monsterW, h).copy(bossKind = Some(wolf.entryName), bossTurn = 0, monsterCurrentEnergy = 0L)
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        // герой; волк попал; прок холода нет; союзник попал + прок нет
        _       <- TestRandom.feedInts(60, 90, 50, 60, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(screens.contains("Йорген Кремень бьёт Белый Волк")) &&
              assertTrue(after.monsterCurrentArmor < wolf.stats(bossLvl).armor)
    }
  )
}
