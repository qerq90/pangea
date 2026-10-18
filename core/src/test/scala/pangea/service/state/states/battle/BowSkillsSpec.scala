package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.rune.{Rune, RuneData}
import pangea.model.skill.Skill
import pangea.model.squad.AllyKind
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Умения лука и арбалета: свои, на всю дальность оружия, и держат поле боя —
  * «Пригвоздить», «Заградительный залп», «Отскок» и «Отбрасывающий выстрел».
  * Умения стрелка — только с луком или арбалетом в руках, прочие умения оружия
  * — только без них. */
object BowSkillsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  private def weapon(grip: Grip, skill: Skill): Item =
    Item(101L, "Оружие", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(grip),
      details = ItemDetails.Weapon(skill))

  /** Герой с оружием в правой руке, Тараном на кирасе и клеймами `brands`. */
  private def hero(right: Item, brands: List[Skill] = Nil): Hero = {
    val base  = TestFixtures.hero(userId)
    val chest = Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(Skill.Ram))
    base.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 100L, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = base.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = right, chestPlate = chest),
      runes      = brands.foldLeft(RuneData.empty)((r, s) => r.brand(Rune.Active(s))))
  }

  private val full = 100000L

  private def monster: Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = full, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  /** Строй: первый моб в паре с героем на первом месте, остальные — на `places`. */
  private def battle(h: Hero, places: List[Int]): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List.fill(places.size + 1)(monster), h, Nil)
    b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0)), group = b.group.copy(places = places))
  }

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def skills(h: Hero): Set[Skill] = h.activeSkillSlots.map(_.skill).toSet

  private def generated(seed: Long, grip: Grip): Item =
    ItemGenerator.createItemOfType(ItemType.Weapon, 30L, ItemRarity.Blue, Rng(seed), Some(grip))._1

  override def spec = suite("Умения лука и арбалета")(

    test("на лук и арбалет выпадают только умения стрелка — все четыре; на прочее оружие — никогда") {
      val ranged = (1L to 400L).flatMap(s => List(generated(s, Grip.Bow), generated(s, Grip.Crossbow))).flatMap(_.activeSkill)
      val melee  = (1L to 400L).flatMap(s => List(generated(s, Grip.TwoHanded), generated(s, Grip.OneHanded))).flatMap(_.activeSkill)
      val meleeOk = melee.nonEmpty && melee.forall(s => Skill.weaponSkills.contains(s))
      assertTrue(ranged.toSet == Skill.rangedSkills.toSet) &&
      assertTrue(meleeOk)
    },

    test("с луком — умения стрелка, без лука — прочие умения оружия; и с вещи, и с клейма; кираса — всегда") {
      val bowBranded   = hero(weapon(Grip.Bow, Skill.PinningShot), brands = List(Skill.QuickStrike, Skill.Barrage))
      val swordBranded = hero(weapon(Grip.TwoHanded, Skill.QuickStrike), brands = List(Skill.PinningShot, Skill.FanCut))
      // лук, выпавший до этого обновления, — с умением меча: оно молчит
      val oldBow       = hero(weapon(Grip.Bow, Skill.BladeWhirl))
      assertTrue(skills(bowBranded) == Set[Skill](Skill.PinningShot, Skill.Barrage, Skill.Ram)) &&
      assertTrue(skills(swordBranded) == Set[Skill](Skill.QuickStrike, Skill.FanCut, Skill.Ram)) &&
      assertTrue(skills(oldBow) == Set[Skill](Skill.Ram))
    },

    test("в описании сказано, с чем умение работает") {
      val h = hero(weapon(Grip.Bow, Skill.PinningShot))
      assertTrue(Skill.PinningShot.describe(h).contains("Только с луком или арбалетом.")) &&
      assertTrue(Skill.Barrage.describe(h).contains("Только с луком или арбалетом.")) &&
      assertTrue(Skill.QuickStrike.describe(h).contains("С луком и арбалетом не работает.")) &&
      assertTrue(!Skill.Ram.describe(h).contains("луком")) &&
      // дальность не зашита в текст: «на всю дальность оружия»
      assertTrue(Skill.rangedSkills.forall(s => !s.description.contains("три")))
    },

    test("«Пригвоздить» бьёт на всю дальность: пригвождённый два раунда стоит, на третий идёт") {
      val h = hero(weapon(Grip.Bow, Skill.PinningShot))
      for {
        t <- makeState(h, battle(h, List(4)))
        (state, dao, r) = t
        _      <- state.action(testUser, aimed("Skill_101", 4), r)
        first  <- battleOf(dao)
        screen <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        _      <- state.action(testUser, tap("Wait"), r)
        second <- battleOf(dao)
        _      <- state.action(testUser, tap("Wait"), r)
        third  <- battleOf(dao)
      } yield assertTrue(BattleState.skillReach(h, Skill.PinningShot) == BattleState.heroReach(h)) &&
              // попал, остался на месте и помечен в строю
              assertTrue(first.monsterAt(4).exists(m => m.currentHp < full && m.effects.monsterRooted)) &&
              assertTrue(!first.group.hasMonster(3) && screen.linesIterator.exists(l => l.startsWith("4.") && l.contains("📌"))) &&
              assertTrue(second.monsterAt(4).exists(_.effects.monsterRooted) && !second.group.hasMonster(3)) &&
              // путы сошли — шагнул к герою
              assertTrue(third.group.hasMonster(3) && third.monsterAt(3).exists(!_.effects.monsterRooted))
    },

    test("«Заградительный залп» задевает всех в пределах дальности: задетые раунд стоят, дальний идёт") {
      val h = hero(weapon(Grip.Bow, Skill.Barrage))
      for {
        t <- makeState(h, battle(h, List(3, 5)))
        (state, dao, r) = t
        _      <- state.action(testUser, aimed("Skill_101", 1), r)
        first  <- battleOf(dao)
        _      <- state.action(testUser, tap("Wait"), r)
        second <- battleOf(dao)
      } yield assertTrue(first.monsterCurrentHp < full) &&
              // третий задет и стоит; пятый вне дальности — цел и шагнул на четвёртое
              // (на пятое за ним может прибежать подкрепление)
              assertTrue(first.monsterAt(3).exists(m => m.currentHp < full && m.effects.monsterRooted)) &&
              assertTrue(first.monsterAt(4).exists(_.currentHp == full)) &&
              // через раунд третий тоже пошёл
              assertTrue(second.group.hasMonster(2))
    },

    test("«Отскок» прыгает туда, где напротив пусто и рядом меньше врагов, и цель остаётся на выстреле") {
      val h  = hero(weapon(Grip.Bow, Skill.Disengage))
      val g  = battle(h, List(2)).group
      // с первого места: на втором напротив моб — на третье
      val first = g.leapSpot(target = 1, dist = 2, reach = 3)
      // с пятого, строй 5 (пара), 6, 7: налево, на третье — там рядом никого
      val fifth = g.copy(heroPos = 5, activePos = 5, places = List(6, 7)).leapSpot(target = 5, dist = 2, reach = 3)
      // на третьем союзник — прыгать некуда
      val rat     = BattleAlly(AllyKind.Rat, 3, 10L, 0L, 0L, FightStats(1, 10, 0, 0, 0, 1, 0), 1L)
      val blocked = g.copy(allies = List(rat)).leapSpot(target = 1, dist = 2, reach = 3)
      // цель на шестом, герой на третьем: на первое нельзя — оттуда до неё не достать
      val far = g.copy(heroPos = 3, activePos = 6, places = List(7)).leapSpot(target = 6, dist = 2, reach = 3)
      assertTrue(first == Some(3) && fifth == Some(3) && blocked.isEmpty) &&
      assertTrue(far.exists(p => math.abs(p - 6) <= 3) && !far.contains(1)) &&
      // прыжок — на одно место меньше дальности оружия
      assertTrue(BattleState.displacement(h) == BattleState.heroReach(h) - 1)
    },

    test("«Отбрасывающий выстрел» уносит цель от героя, пока места свободны; минибосса и сооружение — нет") {
      val h       = hero(weapon(Grip.Bow, Skill.PushShot))
      val open    = battle(h, List(4)).pushActive(2).map(_._2)
      val stopped = battle(h, List(3)).pushActive(2).map(_._2)
      val wall    = battle(h, List(2)).pushActive(2)
      val boss    = battle(h, List(4)).copy(bossKind = Some(MiniBoss.WhiteWolf.entryName)).pushActive(2)
      val tower   = battle(h, List(4)).copy(monsterRace = Race.Construct.entryName).pushActive(2)
      // сосед справа летит дальше вправо
      val side    = battle(h, List(2)).engage(2).pushActive(2).map(_._2)
      assertTrue(open == Some(3) && stopped == Some(2) && wall.isEmpty) &&
      assertTrue(boss.isEmpty && tower.isEmpty && side == Some(4))
    },

    test("«Отскок» в бою: прыжок, потом выстрел издалека; пара рвётся") {
      val h = hero(weapon(Grip.Bow, Skill.Disengage))
      for {
        t <- makeState(h, battle(h, List(2)))
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_101", 1), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("Вы отпрыгиваете на позицию 3") && log.contains("Выстрел в прыжке")) &&
              // бывшая пара ранена и осталась на первом — не напротив героя (а на
              // третье, напротив него, может прибежать подкрепление)
              assertTrue(after.group.heroPos == 3) &&
              assertTrue(after.monsterAt(1).exists(_.currentHp < full))
    },

    test("«Отскок» в бою: прыгать некуда — выстрел с места") {
      val h = hero(weapon(Grip.Bow, Skill.Disengage))
      for {
        t <- makeState(h, battle(h, List(2, 3)))
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_101", 1), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("Отпрыгнуть некуда") && after.group.heroPos == 1)
    },

    test("«Отбрасывающий выстрел» в бою: пара отлетает, удар по ней — уже издалека, и она идёт обратно") {
      val h = hero(weapon(Grip.Bow, Skill.PushShot))
      for {
        t <- makeState(h, battle(h, List(5)))
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_101", 1), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("отлетает на место 3")) &&
              // к концу раунда шагнула обратно — на второе, сбоку от героя
              assertTrue(!after.group.paired && after.group.activePos == 2) &&
              assertTrue(after.monsterAt(2).exists(_.currentHp < full))
    },

    test("«Отбрасывающий выстрел» по соседу: тот отлетает, а базовый удар — по паре") {
      val h = hero(weapon(Grip.Bow, Skill.PushShot))
      for {
        t <- makeState(h, battle(h, List(2)))
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_101", 2), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("отлетает на место 4")) &&
              // с четвёртого он шагнул на третье; пара на месте и ранена
              assertTrue(after.group.hasMonster(3) && !after.group.hasMonster(2)) &&
              assertTrue(after.group.paired && after.monsterCurrentHp < full)
    },

    test("отброшенный дальше, чем бьёт лук: в этом раунде базовый удар всё равно достаёт его") {
      val h  = hero(weapon(Grip.Bow, Skill.PushShot))
      // пары нет: напротив героя пусто, сосед на втором, цель на третьем
      val b0 = battle(h, List(3))
      val b  = b0.copy(group = b0.group.copy(activePos = 2))
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_101", 3), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("отлетает на место 5")) &&
              // удар ушёл за отброшенным (он уже шагнул на четвёртое), сосед цел
              assertTrue(after.monsterAt(4).exists(_.currentHp < full)) &&
              assertTrue(after.monsterAt(2).exists(_.currentHp == full))
    },

    test("пригвождённый в перемешивании стоит на своём месте, а сквозь него не проходят") {
      val h  = hero(weapon(Grip.Bow, Skill.PinningShot))
      val b0 = battle(h, List(3, 4))
      val pinnedIdx = b0.group.idxOf(3)
      val b = b0.copy(group = b0.group.copy(others = b0.group.others.updated(pinnedIdx,
                b0.group.others(pinnedIdx).copy(effects = b0.group.others(pinnedIdx).effects.copy(monsterRootedTurns = 2)))))
      val movers = b.movableInOrder
      val mixed  = b.reshuffle(movers.indices.reverse.toList)
      assertTrue(movers.size == 2 && movers.forall(!_.effects.monsterRooted)) &&
      assertTrue(mixed.monsterAt(3).exists(_.effects.monsterRooted)) &&
      // моб с четвёртого к герою не идёт: путь закрыт пригвождённым
      assertTrue(b.stepTowardsHero(4).isEmpty)
    }
  )
}
