package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.MonsterEnergy
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Комбо Молния + Холод: срезает броню, запирает умение моба и НЕ отменяет
  * одиночные проки обеих стихий. */
object ElementComboSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  /** Оружие с топазом (Молния) и сапфиром (Холод) — обе стихии сразу. */
  private def stormBlade: Item =
    Item(2L, "Грозовой клинок", 1L, ItemRarity.Blue, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = List(Some(Gem(GemKind.Topaz, 1)), Some(Gem(GemKind.Sapphire, 1))))

  private def hero: Hero = TestFixtures.hero(userId).copy(
    lvl        = lvl,
    fightStats = FightStats(atk = 20, hp = 500000L, armor = 0, defence = 0,
                            evasion = 0, accuracy = 9999, energy = 0),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
    equipment  = TestFixtures.emptyEquipment.copy(weapon = stormBlade))

  private def battle(armor: Long = 10000L): SoloPveBattle = SoloPveBattle(
    monsterLvl           = lvl,
    monsterRace          = Race.Human.entryName,
    monsterRarity        = Rarity.Common.entryName,
    monsterStats         = FightStats(atk = 20, hp = 1000000L, armor = armor, defence = 0,
                                      evasion = 0, accuracy = 9999,
                                      energy = MonsterEnergy.maxEnergy(lvl)),
    monsterCurrentHp     = 1000000L,
    monsterCurrentArmor  = armor,
    monsterCurrentEnergy = MonsterEnergy.maxEnergy(lvl))

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  /** Ход с заданными бросками. Порядок: удар героя, проки по Element.values
    * (Холод, Огонь, Молния, Воздух — у нас только Холод и Молния), удар моба. */
  private def turn(b: SoloPveBattle, rolls: Int*) =
    for {
      t <- makeState(hero, b)
      (state, dao, r) = t
      _     <- TestRandom.feedInts(rolls: _*) *> TestRandom.feedLongs(100L, 100L)
      _     <- state.action(testUser, tap("Attack"), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after, log)

  override def spec = suite("Комбо стихий")(

    test("комбо срезает десятую часть брони, а не сорок процентов") {
      val b = battle(armor = 10000L)
      for {
        // удар героя, прок Холода, прок Молнии, удар моба (мимо)
        r <- turn(b, 60, 1, 1, 99)
        (after, log) = r
        armorLost = b.monsterCurrentArmor - after.monsterCurrentArmor
      } yield assertTrue(log.contains("сковали и душу")) &&
              assertTrue(BattleState.ComboArmorCutPct == 10L) &&
              // 10% потолка брони плюс обычный урон по броне от удара
              assertTrue(armorLost >= 1000L) && assertTrue(armorLost < 2000L)
    },

    test("оба одиночных прока срабатывают вместе с комбо") {
      for {
        r <- turn(battle(), 60, 1, 1, 99)
        (after, log) = r
      } yield // Холод: накопительный срез снижения урона цели
              assertTrue(after.effects.monsterColdDefenceCut > 0) &&
              // Молния: выжженная энергия — и ровно одной строкой
              assertTrue(after.monsterCurrentEnergy < MonsterEnergy.maxEnergy(lvl)) &&
              assertTrue(log.contains("выжгла")) &&
              // о выжиге сообщает ровно одна строка, а не две подряд
              assertTrue(log.linesIterator.count(_.contains("выжгла")) == 1)
    },

    test("после комбо моб не применяет умение в свой ближайший ход") {
      for {
        r <- turn(battle(), 60, 1, 1, 99)
        (after, log) = r
      } yield assertTrue(log.contains("не смог применить умение")) &&
              // ни быстрых атак, ни удара плашмя — умение застряло
              assertTrue(!log.contains("делает быстрые атаки")) &&
              assertTrue(!log.contains("бьёт плашмя")) &&
              // тик в начале хода моба съел один заряд, остался последний
              assertTrue(after.effects.monsterSkillBlockedTurns == 1)
    },

    test("следующий ход моб кастует снова — блок держится ровно один каст") {
      for {
        first <- turn(battle(), 60, 1, 1, 99)
        (afterCombo, _) = first
        t <- makeState(hero, afterCombo)
        (state, dao, r) = t
        // второй ход: проки мимо (99), удар моба мимо, выбор умения
        _      <- TestRandom.feedInts(60, 99, 99, 99, 0) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        log    <- r.sentScreens.map(_.map(_.text).mkString)
        after  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(!log.contains("не смог применить умение")) &&
              assertTrue(after.effects.monsterSkillBlockedTurns == 0)
    },

    test("без комбо одиночный прок Холода работает как прежде") {
      for {
        r <- turn(battle(), 60, 1, 99, 99) // Холод прошёл, Молния мимо
        (after, log) = r
      } yield assertTrue(after.effects.monsterColdDefenceCut > 0) &&
              assertTrue(!log.contains("сковали и душу")) &&
              assertTrue(after.effects.monsterSkillBlockedTurns == 0)
    }
  )
}
