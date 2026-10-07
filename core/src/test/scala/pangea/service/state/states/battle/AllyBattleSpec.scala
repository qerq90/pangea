package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.squad.{Ally, AllyKind, Squad, UndeadForm}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.model.schedule.TaskKind
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
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

  /** Йорген у своего потолка (5-й уровень): HP 625, броня 750, атака 100, энергия 500. */
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

    test("строй двухрядный: герой и союзник слева по позициям, мобы справа по местам; «Атака» с двумя целями спрашивает, в кого") {
      for {
        t <- makeState(hero(), group(hero(), 1000L, 1000L))
        (state, dao, r) = t
        _       <- state.enter(testUser, r)
        screens <- r.sentScreens
        first    = screens.head.text
        buttons  = screens.last.choices.map(_.label)
        _       <- state.action(testUser, tap("Attack"), r)
        ask     <- r.sentScreens.map(_.last)
        targets  = ask.choices.filter(_.id == "Attack").map(_.data("target"))
        same    <- battleOf(dao)
      } yield assertTrue(first.contains("1. 🟢 Вы VS 🔴")) &&
              assertTrue(first.contains("2. 🔵 Йорген Кремень ❤ 100% 🧥 100% VS 🔴")) &&
              assertTrue(buttons.contains("Атака") && !buttons.exists(_.startsWith("⚔ Место"))) &&
              assertTrue(ask.text == "🎯 Атака — в кого?" && targets == List("1", "2")) &&
              assertTrue(ask.choices.exists(_.id == "CancelTarget")) &&
              assertTrue(same.monsterCurrentHp == 1000L && same.group.round == 0)   // ход не потрачен
    },

    test("обычная встреча: моб появляется на месте 1, где бы ни стоял герой; герой на 2 бьёт его как соседа, дерётся с ним союзник на 1") {
      val h = hero(heroPos = 2, allies = List(ally(pos = 1)))
      val b = SoloPveBattle.from(monster(1000L), h)
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _       <- state.enter(testUser, r)
        entry   <- r.sentScreens
        // удар героя по месту 1 (единственная цель — без вопроса); ответа пары нет;
        // союзник на 1 бьёт моба напротив + прок; моб бьёт союзника; подкрепление
        _       <- TestRandom.feedInts(60, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        a        = after.group.allies.head
      } yield assertTrue(b.group.activePos == 1 && !b.group.paired && b.group.attackTargets == List(1)) &&
              assertTrue(entry.head.text.contains("1. 🔵 Йорген Кремень ❤ 100% 🧥 100% VS 🔴")) &&
              assertTrue(entry.head.text.contains("2. 🟢 Вы VS ⚪")) &&
              assertTrue(entry.last.text.contains("Напротив вас никого нет")) &&
              assertTrue(entry.last.choices.exists(_.label == "Атака")) &&
              assertTrue(screens.contains("Вы наносите") && !screens.contains("атаковал вас сбоку")) &&
              assertTrue(after.monsterCurrentHp < 1000L && !after.group.paired && after.group.activePos == 1) &&
              assertTrue(updated.fightStats.hp == 500000L && a.armor < 750L)
    },

    test("герой убил своего моба: занятый союзником сосед к нему не шагает — напротив пусто, соседа бьёт по кнопке и добивает") {
      val h = hero(atk = 100000L)
      for {
        t <- makeState(h, group(h, 10L, 1000L))
        (state, dao, r) = t
        // герой добил активного (ответа нет); союзник бьёт моба напротив + прок; моб бьёт союзника; подкрепление
        _       <- TestRandom.feedInts(60, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        first   <- state.action(testUser, aimed("Attack", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens
        log      = screens.map(_.text).mkString("\n")
        _       <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        second  <- state.action(testUser, tap("Attack"), r)
      } yield assertTrue(first == StateType.Battle) &&
              assertTrue(log.contains("К вам никто не шагает")) &&
              assertTrue(!after.group.paired && after.group.activePos == 2 && after.group.slain.size == 1) &&
              assertTrue(after.group.attackTargets == List(2)) &&
              assertTrue(screens.last.text.contains("Напротив вас никого нет") && screens.last.text.contains("против последней цели")) &&
              assertTrue(screens.last.choices.exists(_.label == "Атака")) &&
              assertTrue(second == StateType.Loot)
    },

    test("освободившийся моб (союзник напротив ушёл по свитку) в конце раунда шагает к герою") {
      val h = hero(heroPos = 2, allies = List(ally(pos = 1, hp = Some(1L), armor = Some(0L))))
      for {
        t <- makeState(h, SoloPveBattle.from(monster(100000L), h))
        (state, dao, r) = t
        // удар по месту 1; союзник на 1 бьёт моба напротив + прок; моб обнуляет союзника (свиток); подкрепление
        _       <- TestRandom.feedInts(60, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(screens.contains("воспользовался свитком") && screens.contains("шагает к вам")) &&
              assertTrue(after.group.paired && after.group.activePos == 2 && after.group.heroPos == 2 && after.group.allies.isEmpty)
    },

    test("поднятый с алтаря свитков не носит: обнулённый — рассыпается, и в отряд не вернётся") {
      // Поднятый стоит напротив моба (место 1), герой — за ним: бьют его.
      val risen = Ally(AllyKind.Undead, 1, 1L, 0L, 0L,
        undead = Some(UndeadForm("Эльф капитан", 12L, AllyKind.Human.stats(3L))))
      val h     = hero(heroPos = 2, allies = List(risen))
      for {
        t <- makeState(h, SoloPveBattle.from(monster(100000L), h))
        (state, dao, r) = t
        // удар героя; поднятый бьёт; моб его обнуляет; подкрепления нет
        _       <- TestRandom.feedInts(60, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after   <- battleOf(dao)
      } yield assertTrue(screens.contains("Эльф капитан") && screens.contains("осыпаются грудой")) &&
              // про свиток ему сказать нечего: он не наёмник
              assertTrue(!screens.contains("воспользовался свитком")) &&
              // с песка он убран, а из отряда уйдёт насовсем по концу боя
              assertTrue(after.group.allies.isEmpty) &&
              assertTrue(after.group.alliesGone.exists(_.undead.isDefined))
    },

    test("союзника уносит свитком тем же ходом, каким кончается бой, — и об этом сказано") {
      // Герой добивает моба в паре; второй, стоящий сбоку, успевает добить
      // союзника и тут же издыхает от яда — бой кончается победой. Раньше
      // строка про свиток пропадала: после победы сводку не показывали.
      val doomed = ally(pos = 2, hp = Some(1L), armor = Some(0L))
      val h      = hero(atk = 100000L, allies = List(doomed))
      val group  = SoloPveBattle.fromGroup(List(monster(1L), monster(1000L, atk = 100000L)), h, List(0L, 0L))
      val poisoned = group.copy(group = group.group.copy(
        others = group.group.others.map(o => o.copy(effects = o.effects.copy(monsterPoison = Some(Poison(100)))))))
      for {
        t <- makeState(h, poisoned)
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 99, 99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        result  <- state.action(testUser, aimed("Attack", 1), r)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        updated <- dao.getHeroByUserId(userId).map(_.get)
        now     <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(screens.contains("воспользовался свитком")) &&
              // и в отряде его после боя нет — ушёл в отлучку
              assertTrue(!updated.squad.has(AllyKind.Human) && updated.squad.isAway(AllyKind.Human, now))
    },

    test("шаг ровно один: дальний союзник в списке не появляется, к соседнему герой встаёт меняясь") {
      // Герой на 3, свои на 1 и 2, моб на месте 1 — дотянуться до него разом нельзя.
      val h = hero(heroPos = 3, allies = List(ally(pos = 1), ally(AllyKind.Gnome, pos = 2)))
      for {
        t <- makeState(h, SoloPveBattle.from(monster(100000L), h))
        (state, dao, r) = t
        _       <- state.enter(testUser, r)
        entry   <- r.sentScreens.map(_.last)
        _       <- state.action(testUser, tap("Move"), r)
        ask     <- r.sentScreens.map(_.last)
        // шаг через голову не проходит: место 1 от героя через одно
        far     <- state.action(testUser, aimed("MoveTo", 1), r)
        stayed  <- battleOf(dao)
        // а на соседнее встаёт, меняясь местами с тем, кто там стоял
        _       <- TestRandom.feedInts(99, 60, 99, 99) *> TestRandom.feedLongs(100L, 100L)
        result  <- state.action(testUser, aimed("MoveTo", 2), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(entry.choices.exists(_.label == "Переместиться") && !entry.choices.exists(_.label == "Атака")) &&
              // выжидание никуда не делось: шаг его не подменяет
              assertTrue(entry.choices.exists(_.label == "Ждать")) &&
              assertTrue(ask.choices.filter(_.id == "MoveTo").map(_.label) == List("2. Брамбл Медноус")) &&
              assertTrue(far == StateType.Battle && stayed.group.heroPos == 3) &&
              assertTrue(result == StateType.Battle && screens.contains("Вы меняетесь местами: Брамбл Медноус")) &&
              // поменялись: герой на 2, Брамбл на 3, а Йорген как стоял на 1, так и стоит
              assertTrue(after.group.heroPos == 2) &&
              assertTrue(after.group.allyAt(3).exists(_.kind == AllyKind.Gnome)) &&
              assertTrue(after.group.allyAt(1).exists(_.kind == AllyKind.Human))
    },

    test("шаг на пустое соседнее место: союзник для этого не нужен") {
      // Герой на 3, моб на месте 1 — не достать, своих рядом нет: остаётся шагнуть.
      // Позицию задаём бою напрямую: одиночного героя отряд ставит на первое место.
      val h    = hero(heroPos = 3, allies = Nil)
      val solo = SoloPveBattle.from(monster(100000L), h)
      for {
        t <- makeState(h, solo.copy(group = solo.group.copy(heroPos = 3)))
        (state, dao, r) = t
        _      <- state.enter(testUser, r)
        entry  <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("Move"), r)
        ask    <- r.sentScreens.map(_.last)
        _      <- TestRandom.feedInts(99, 99) *> TestRandom.feedLongs(100L, 100L)
        result <- state.action(testUser, aimed("MoveTo", 2), r)
        after  <- battleOf(dao)
        says   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(entry.choices.exists(_.label == "Переместиться")) &&
              // за край строя ходить некуда: сосед всего один, и тот пустой
              assertTrue(ask.choices.filter(_.id == "MoveTo").map(_.label) == List("2. — пусто —")) &&
              assertTrue(result == StateType.Battle && says.contains("Вы переходите на позицию 2")) &&
              // шаг сделан — и моб на месте 1 к концу раунда сам подтянулся к герою
              assertTrue(after.group.heroPos == 2 && after.group.paired)
    },

    test("бить некого и союзников уже нет (ушли по свиткам) — «Ждать»: герой пропускает удар, раунд идёт") {
      // Герой стоял третьим за двумя союзниками; оба ушли по свиткам — один против моба на месте 1.
      val h = hero(heroPos = 3, allies = List(ally(pos = 1), ally(AllyKind.Gnome, pos = 2)))
      val b = SoloPveBattle.from(monster(100000L), h)
      val gone = List(ally(pos = 1), ally(AllyKind.Gnome, pos = 2)).map(BattleAlly.of(_, lvl))
      for {
        t    <- makeState(h, b.copy(group = b.group.copy(allies = Nil, alliesGone = gone)))
        (state, dao, r) = t
        _       <- state.enter(testUser, r)
        entry   <- r.sentScreens.map(_.last)
        // моб с места 1 героя на 3 не достаёт; подкрепления нет; моб свободен — в конце раунда шагает к герою
        _       <- TestRandom.feedInts(99) *> TestRandom.feedLongs(100L)
        result  <- state.action(testUser, tap("Wait"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(entry.choices.exists(_.label == "Ждать") && !entry.choices.exists(_.label == "Атака")) &&
              assertTrue(result == StateType.Battle && screens.contains("Вы выжидаете") && screens.contains("шагает к вам")) &&
              assertTrue(after.group.paired && after.group.heroPos == 3)
    },

    test("союзник бьёт врага напротив и добивает его — павший ждёт добычи, бой идёт") {
      for {
        t <- makeState(hero(), group(hero(), 1000L, 10L))
        (state, dao, r) = t
        // герой попал, моб в паре ответил; союзник попал (добил — прока нет); подкрепления нет
        _       <- TestRandom.feedInts(60, 99, 60, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        result  <- state.action(testUser, aimed("Attack", 1), r)
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
        // герой; моб; удар союзника + прок; выбор умения (1 из [быстрый, дробящий]); дробящий (без броска
        // на попадание — умения не мажут) + прок; подкрепление
        _       <- TestRandom.feedInts(60, 99, 60, 99, 1, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        a        = after.group.allies.head
      } yield assertTrue(screens.contains("⚔ Йорген Кремень бьёт Орк раб на 150 урона.")) &&   // 100 × 1,1 (огонь по HP) + 40 плашмя
              assertTrue(!screens.contains("плашмя") && screens.linesIterator.count(_.startsWith("⚔ Йорген")) == 1) &&
              // энергия и цены умений — по уровню наёмника, а не героя
              assertTrue(a.energy == 500L - 80L + 35L)
    },

    test("герой обнулён при живом отряде — не смерть: бой идёт без него по таймеру, отряд добивает — герой приходит в себя с 1 HP") {
      val h = hero(hp = 1L, allies = List(ally(pos = 2)))
      for {
        dao      <- TestHeroDao.withHero(userId, h)
        _        <- dao.writeActiveBattle(userId, group(h, 1000L, 10L).asJson)
        r        <- TestRenderer.make
        sch      <- TestScheduler.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content, sch)
        // герой попал; моб в паре попал и обнулил его (травмы нет — герой уже на нуле)
        _        <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        down     <- state.action(testUser, aimed("Attack", 1), r)
        b1       <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        sched    <- sch.scheduled
        scr1     <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        lying    <- r.sentScreens.map(_.last)
        // кнопки лежащего героя ничего не делают — только экран
        _        <- state.action(testUser, aimed("Attack", 1), r)
        same     <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        // тик 1: союзник бьёт соседа (10 HP) — добил, прока нет; моб в паре лежащего героя не трогает; подкрепление
        _        <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L)
        t1       <- state.action(testUser, tap("SquadTick"), r)
        b2       <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        // тик 2: союзнику напротив никого — бьёт моба в паре (сосед) + прок; накопил 70 энергии —
        // быстрый удар (единственный по карману, индекс 0) + прок; подкрепление
        _        <- TestRandom.feedInts(60, 99, 0, 99, 99) *> TestRandom.feedLongs(100L, 100L)
        t2       <- state.action(testUser, tap("SquadTick"), r)
        b3       <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        // моба почти нет — тик 3 добивает: победа, герой приходит в себя
        _        <- dao.writeActiveBattle(userId, b3.copy(monsterCurrentHp = 1L).asJson)
        _        <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        t3       <- state.action(testUser, tap("SquadTick"), r)
        updated  <- dao.getHeroByUserId(userId).map(_.get)
        scr      <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(down == StateType.Battle && b1.group.heroDown) &&
              // лежачему жать нечего: кнопок нет, и клавиатура убрана
              assertTrue(lying.choices.isEmpty && lying.hideKeyboard) &&
              assertTrue(sched.exists(s => s.kind == TaskKind.SquadFight && s.expectedState == StateType.Battle && s.fireAt == 30000L)) &&
              assertTrue(scr1.contains("оседаете на землю") && scr1.contains("Отряд дерётся за вас")) &&
              assertTrue(same == b1) &&
              assertTrue(t1 == StateType.Battle && b2.group.slain.size == 1 && b2.group.others.isEmpty) &&
              assertTrue(t2 == StateType.Battle && b3.monsterCurrentHp < 1000L && b3.group.heroDown) &&
              assertTrue(t3 == StateType.Loot && updated.fightStats.hp == 1L) &&
              assertTrue(scr.contains("отряд отбился, и вы приходите в себя"))
    },

    test("«Упырь» 12: пир только за своих убитых — добил союзник, герой не лечится") {
      // Набор «Упырь» на двенадцать предметов целиком.
      val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
        ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
        ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)
      val ghoulEq = slots.zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
        val it = Item(400L + i, "Предмет", 1L, ItemRarity.Blue, t,
          attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
          set = Some(pangea.model.item.ItemSet.Ghoul))
        i match {
          case 0  => eq.copy(helmet = it)
          case 1  => eq.copy(shoulderPads = it)
          case 2  => eq.copy(chestPlate = it)
          case 3  => eq.copy(bracelets = it)
          case 4  => eq.copy(gloves = it)
          case 5  => eq.copy(pants = it)
          case 6  => eq.copy(boots = it)
          case 7  => eq.copy(amulet = it)
          case 8  => eq.copy(firstRing = it)
          case 9  => eq.copy(secondRing = it)
          case 10 => eq.copy(belt = it)
          case _  => eq.copy(weapon = it)
        }
      }
      // Герой ранен и бьёт слишком слабо, чтобы кого-то добить; союзник напротив
      // соседа добивает его сам.
      val h = hero(atk = 1L, allies = List(ally(pos = 2))).copy(equipment = ghoulEq)
      val wounded = h.copy(fightStats = h.fightStats.copy(hp = 100L, armor = 0L))
      for {
        t <- makeState(wounded, group(wounded, 100000L, 1L))
        (state, dao, r) = t
        _     <- TestRandom.feedInts(60, 3, 90, 90)
        _     <- TestRandom.feedLongs(100L, 100L)
        _     <- state.action(testUser, aimed("Attack", 1), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        b1    <- battleOf(dao)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(wounded.sets.feastsOnKill) &&
              // союзник кого-то уложил
              assertTrue(b1.group.slain.nonEmpty) &&
              // а пира нет: герой никого не убивал. (HP при этом растёт — у
              // двенадцати предметов работает и порог 10, лечащий чужой кровью.)
              assertTrue(!said.contains("жуткий пир")) &&
              assertTrue(after.fightStats.hp > 0L)
    },

    test("герой лежит, союзники добили охрану — башням некого прикрывать, и это победа") {
      // Караван: охранник в паре с героем и башня в хвосте. Герой лежит,
      // союзник добивает охранника — башня остаётся одна, и бой кончен.
      val h     = hero(allies = List(ally(pos = 2)))
      val tower = Monster(0L, lvl, Race.Construct, Rarity.Rare,
                    FightStats(atk = 1, hp = 9999L, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      val b0    = SoloPveBattle.fromGroup(List(monster(1L), tower), h, Nil)
      // Башня стоит в хвосте, напротив союзника никого — он бьёт охранника в паре.
      val down  = b0.copy(group = b0.group.copy(heroDown = true, places = List(3)))
      for {
        t <- makeState(h.copy(fightStats = h.fightStats.copy(hp = 0L)), down)
        (state, dao, r) = t
        _    <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        out  <- state.action(testUser, tap("SquadTick"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        up   <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(out == StateType.Loot) &&
              assertTrue(said.contains("приходите в себя")) &&
              // герой поднимается, как и при обычной победе отряда
              assertTrue(up.fightStats.hp == BattleState.DownReviveHp)
    },

    test("моб напротив лежачего уходит к союзнику, если есть куда") {
      val h    = hero(allies = List(ally(pos = 2)))
      val up   = group(h, 1000L)
      val down = up.copy(group = up.group.copy(heroDown = true))
      val (moved, to) = down.leaveDownedHero
      // место союзника свободно — моб переходит туда и разрывает пару с героем
      assertTrue(down.group.paired && to.contains(2)) &&
      assertTrue(moved.group.activePos == 2 && !moved.group.paired) &&
      // пока герой на ногах — никто никуда не идёт
      assertTrue(up.leaveDownedHero._2.isEmpty) &&
      // место союзника занято другим мобом — остаётся где стоял
      assertTrue {
        val two = group(h, 1000L, 10L)
        two.copy(group = two.group.copy(heroDown = true)).leaveDownedHero._2.isEmpty
      } &&
      // союзник пал — идти не к кому
      assertTrue(down.copy(group = down.group.updateAlly(2)(_.copy(hp = 0L))).leaveDownedHero._2.isEmpty) &&
      // сооружение с места не сходит
      assertTrue(down.copy(monsterRace = Race.Construct.entryName).leaveDownedHero._2.isEmpty)
    },

    test("герой лежит, а последний союзник уходит по свитку — смерть, отложенная до этого момента") {
      val h = hero(hp = 1L, allies = List(ally(pos = 2, hp = Some(1L), armor = Some(0L))))
      for {
        t <- makeState(h, group(h, 1000L, 1000L))
        (state, dao, r) = t
        // герой попал; моб обнулил героя — герой падает, отряд жив
        _       <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        down    <- state.action(testUser, aimed("Attack", 1), r)
        // тик: союзник бьёт соседа + прок; сосед обнуляет союзника (свиток); подкрепление
        _       <- TestRandom.feedInts(60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L)
        result  <- state.action(testUser, tap("SquadTick"), r)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(down == StateType.Battle && result == StateType.Death) &&
              assertTrue(screens.contains("оседаете на землю") && screens.contains("Последний из отряда"))
    },

    test("сюжетный бой (squad = false) идёт без отряда: герой один, на месте 1") {
      val h = hero(heroPos = 2, allies = List(ally(pos = 1)))
      val b = SoloPveBattle.from(monster(1000L), h, squad = false)
      val g = SoloPveBattle.fromGroup(List(monster(1000L), monster(1000L)), h, Nil, squad = false)
      assertTrue(b.group.allies.isEmpty && b.group.heroPos == 1 && b.group.paired) &&
      assertTrue(g.group.allies.isEmpty && g.group.heroPos == 1 && g.group.paired && g.group.places == List(2))
    },

    test("позиции после боя сохраняются в отряде вместе с позицией героя, найм не сбрасывается") {
      val until = 777_777L
      val h = hero(atk = 100000L, allies = List(ally(pos = 2).copy(hiredUntil = until)))
      for {
        t <- makeState(h, group(h, 10L, 1000L))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("Move"), r)
        // герой на 2 (в пару с мобом 2), Йорген на 1 (напротив моб 1); моб 2 отвечает; Йорген добивает моба 1
        // (10 HP, прока нет); подкрепление
        _       <- TestRandom.feedInts(99, 60, 99) *> TestRandom.feedLongs(100L, 100L)
        _       <- state.action(testUser, aimed("MoveTo", 2), r)
        // бежим: окружать некому, моб в паре бьёт в спину
        _       <- TestRandom.feedInts(99) *> TestRandom.feedLongs(100L)
        fled    <- state.action(testUser, tap("ConfirmFlee"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(fled == StateType.Dungeon) &&
              assertTrue(updated.squad.heroPos == 2 && updated.squad.allyAt(1).exists(a => a.kind == AllyKind.Human && a.hiredUntil == until))
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
      } yield assertTrue(screens.contains("выпивает из своей фляги и восстанавливает 125 хп")) &&
              // лечит до потолка и не выше: 500 было, 625 максимум
              assertTrue(a.hp == 625L) &&
              assertTrue(a.energy == 500L - 40L + 35L)
    },

    test("моб напротив союзника бьёт его, а не героя сбоку") {
      for {
        t <- makeState(hero(), group(hero(), 1000L, 1000L))
        (state, dao, r) = t
        // герой; моб в паре; союзник + прок; моб № 2 бьёт союзника; подкрепление
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        after   <- battleOf(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        a        = after.group.allies.head
      } yield assertTrue(screens.contains("бьёт Йорген Кремень на")) &&
              assertTrue(!screens.contains("сбоку")) &&
              assertTrue(a.armor < 750L && a.hp == 625L) &&
              // герой получил только от моба в паре
              assertTrue(updated.fightStats.hp < 500000L)
    },

    test("обнулённый союзник уходит по свитку; после боя его нет в отряде и он сутки в отлучке") {
      val h = hero(allies = List(ally(hp = Some(1L), armor = Some(0L))))
      for {
        t <- makeState(h, group(h, 1000L, 1000L))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // бежим: окружения нет (99 > 5%)
        _       <- TestRandom.feedInts(99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        fled    <- state.action(testUser, tap("ConfirmFlee"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        now     <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      } yield assertTrue(screens.contains("Йорген Кремень воспользовался свитком и телепортировался с боя")) &&
              assertTrue(after.group.allies.isEmpty && after.group.alliesGone.map(_.kind) == List(AllyKind.Human)) &&
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
        first   <- state.action(testUser, aimed("Attack", 1), r)
        second  <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(first == StateType.Battle && second == StateType.Loot) &&
              assertTrue(updated.squad.allyAt(2).exists(a => a.kind == AllyKind.Human && a.energy == 35L))
    },

    test("герой бьёт соседа, выбрав место 2: урон мобу на месте 2, отвечает моб в паре") {
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

    test("Таран по мобу на месте 2 (напротив него — союзник): герой встаёт на 2, союзник — на 1; целью Таран берёт только мобов") {
      val h = heroWithSkills(hero(), Skill.SweepingStrike, Skill.Ram)
      for {
        t <- makeState(h, group(h, 1000L, 2000L))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("Skill_202"), r)
        ask     <- r.sentScreens.map(_.last)
        targets  = ask.choices.filter(_.id == "Skill_202").map(_.data("target"))
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Skill_202", 2), r)
        after   <- battleOf(dao)
      } yield assertTrue(targets == List("1", "2")) &&
              assertTrue(after.group.heroPos == 2 && after.monsterStats.hp == 2000L) &&
              assertTrue(after.group.allies.head.position == 1) &&
              assertTrue(after.group.others.head.stats.hp == 1000L && after.group.places == List(1))
    },

    test("Таран по мобу на месте 3, когда союзника напротив него нет: герой на 3, союзник остаётся на 1") {
      val h = heroWithSkills(hero(heroPos = 2, allies = List(ally(pos = 1))), Skill.SweepingStrike, Skill.Ram)
      for {
        t <- makeState(h, group(h, 1000L, 2000L, 3000L))
        (state, dao, r) = t
        // таран (разброс); базовая атака по паре; ответ пары; союзник бьёт моба № 1 + прок; моб № 1 бьёт
        // союзника; моб № 3 бьёт героя сбоку; подкрепление
        _       <- TestRandom.feedInts(60, 99, 60, 99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Skill_202", 3), r)
        after   <- battleOf(dao)
      } yield assertTrue(after.group.heroPos == 3 && after.group.paired && after.monsterStats.hp == 3000L) &&
              assertTrue(after.group.allies.head.position == 1)
    },

    test("моб, занятый союзником напротив, к герою не подтягивается; свободный дальний — подтягивается") {
      // герой на 1, союзники на 2 и 3 (отряд без дыр); мобы на 1 (пара), 3 (против гнома) и 5; места 2 и 4 пусты
      val h  = hero(allies = List(ally(pos = 2), ally(AllyKind.Gnome, pos = 3)))
      val b0 = group(h, 1000L, 1000L, 1000L, 1000L, 1000L)
      val b  = b0.sideFallen(b0.group.idxOf(2)).sideFallen(b0.sideFallen(b0.group.idxOf(2)).group.idxOf(4))
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _     <- TestRandom.feedInts(60, 99, 60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
        _     <- state.action(testUser, tap("Attack"), r)
        after <- battleOf(dao)
      } yield assertTrue(b.group.places.sorted == List(3, 5)) &&
              // № 3 остался против союзника, № 5 шагнул на 4 (место 3 занято)
              assertTrue(after.group.places.sorted == List(3, 4))
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
