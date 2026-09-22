package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity => ItemRarity, DivineKind, DivineRates}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Божественное оружие: удар по всему полю боя, раз в раунд, с гранью своего
  * вида; заряды скрыты от игрока, последний удар рассыпает оружие. В доп. слоте
  * может лежать и обычная вещь — тогда кнопки удара нет. */
object DivineBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  private def blade(kind: DivineKind, rarity: ItemRarity, lvl: Long = lvl): Item =
    DivineKind.item(kind, lvl, rarity).copy(id = 7L)

  private def hero(kind: DivineKind, hp: Long = 500000L, rarity: ItemRarity = ItemRarity.Gray): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 20, hp = hp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = h.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(additionalWeapon = blade(kind, rarity)))
  }

  private def monster(hp: Long, atk: Long = 20L): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 100, defence = 100, evasion = 0, accuracy = 9999, energy = 0))

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

  /** Урон божественного оружия 10-го уровня по одному врагу. */
  private val blow = DivineRates.DamagePerLvl * lvl

  /** Броня мобов: её удар сносит первой, а в HP уходит остаток. */
  private val mobArmor = 100L

  override def spec = suite("Божественное оружие")(

    test("кнопка божественного оружия стоит над «Сбежать» и подписана её именем") {
      for {
        t <- makeState(hero(DivineKind.DarkLordSword), group(hero(DivineKind.DarkLordSword), 100000L, 100000L))
        (state, _, r) = t
        _   <- state.enter(testUser, r)
        scr <- r.sentScreens.map(_.last)
        divineBtn = scr.choices.find(_.id == "UseDivine")
        fleeBtn  = scr.choices.find(_.id == "Flee")
      } yield assertTrue(divineBtn.exists(_.label == "⚫ Меч Тёмного Владыки")) &&
              assertTrue(divineBtn.flatMap(_.row).exists(row => fleeBtn.flatMap(_.row).exists(_ > row))) &&
              assertTrue(divineBtn.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
    },

    test("удар бьёт всех на поле — сперва броня, остатком по HP; ход не заканчивается: мобы не отвечают") {
      val h = hero(DivineKind.ThunderGodTrident)
      for {
        t <- makeState(h, group(h, 100000L, 100000L, 100000L))
        (state, dao, r) = t
        result  <- state.action(testUser, tap("UseDivine"), r)
        after   <- battleOf(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.contains(s"Все получили $blow урона")) &&
              assertTrue(after.monsterCurrentHp == 100000L - (blow - mobArmor)) &&
              assertTrue(after.group.others.forall(_.currentHp == 100000L - (blow - mobArmor))) &&
              assertTrue(after.monsterCurrentArmor == 0L) &&                // броню снесло первой
              assertTrue(after.divineUsedThisRound && updated.fightStats.hp == 500000L) &&
              assertTrue(after.group.round == 0)                            // раунд не кончился
    },

    test("за раунд удар один — «быстрые руки» не помогают; после атаки раунд новый") {
      val h = hero(DivineKind.ThunderGodTrident)
      for {
        t <- makeState(h, group(h, 100000L, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("UseDivine"), r)
        _       <- state.action(testUser, tap("UseDivine"), r)
        twice   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // ход героя: удар по паре, ответ моба, сосед бьёт сбоку, подкрепления нет
        _       <- TestRandom.feedInts(60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        _       <- state.action(testUser, tap("UseDivine"), r)
        third   <- battleOf(dao)
      } yield assertTrue(screens.contains("второй раз за раунд его не поднять")) &&
              assertTrue(twice.monsterCurrentHp == 100000L - (blow - mobArmor)) &&  // второй удар не прошёл
              // второй удар пришёлся уже по голому HP: брони на соседе не осталось
              assertTrue(third.group.others.head.currentHp == 100000L - (blow - mobArmor) - blow)
    },

    test("Меч Тёмного Владыки возвращает половину всего отнятого в HP") {
      val h = hero(DivineKind.DarkLordSword, hp = 1000L)
      for {
        t <- makeState(h, group(h, 100000L, 100000L, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("UseDivine"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        drained  = blow * 3 * DivineRates.DrainPct / 100L   // броня тоже идёт в счёт отнятого
      } yield assertTrue(updated.fightStats.hp == 1000L + drained) &&
              assertTrue(screens.contains(s"вы восстановили $drained хп"))
    },

    test("стихийные божественного оружия кладут свою стихию на каждого врага без броска: огонь жжёт, холод режет защиту") {
      val fire = hero(DivineKind.FireLordAxe)
      val cold = hero(DivineKind.WhiteDeathMace)
      for {
        tf <- makeState(fire, group(fire, 100000L, 100000L))
        (fs, fdao, fr) = tf
        _  <- fs.action(testUser, tap("UseDivine"), fr)
        fb <- battleOf(fdao)
        tc <- makeState(cold, group(cold, 100000L, 100000L))
        (cs, cdao, cr) = tc
        _  <- cs.action(testUser, tap("UseDivine"), cr)
        cb <- battleOf(cdao)
      } yield assertTrue(fb.effects.monsterBurn.isDefined && fb.group.others.head.effects.monsterBurn.isDefined) &&
              assertTrue(cb.effects.monsterColdDefenceCut > 0 && cb.group.others.head.effects.monsterColdDefenceCut > 0)
    },

    test("Клинок Бога Змеи травит всех, кого задел") {
      val h = hero(DivineKind.SnakeGodBlade)
      for {
        t <- makeState(h, group(h, 100000L, 100000L))
        (state, dao, r) = t
        _     <- state.action(testUser, tap("UseDivine"), r)
        after <- battleOf(dao)
      } yield assertTrue(after.effects.monsterPoison.isDefined) &&
              assertTrue(after.group.others.head.effects.monsterPoison.isDefined)
    },

    test("Частица Аметистовой Богини даёт 95% попадания на 5 раундов") {
      val h = hero(DivineKind.AmethystGoddess)
      // Уклонение моба заоблачное: без грани аметиста герой мажет почти всегда.
      val dodgy = Monster(0L, lvl, Race.Orc, Rarity.Common,
        FightStats(atk = 20, hp = 100000, armor = 0, defence = 0, evasion = 100000, accuracy = 9999, energy = 0))
      for {
        t <- makeState(h, SoloPveBattle.from(dodgy, h))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("UseDivine"), r)
        after   <- battleOf(dao)
        // бросок 6 выше 5% уклонения — герой попадает; ответ моба и подкрепление
        _       <- TestRandom.feedInts(6, 99, 99) *> TestRandom.feedLongs(100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        hit     <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.effects.heroTrueStrikeTurns == DivineRates.BuffRounds) &&
              assertTrue(screens.contains("Вы наносите")) &&
              assertTrue(hit.monsterCurrentHp < after.monsterCurrentHp) &&
              assertTrue(hit.effects.heroTrueStrikeTurns == DivineRates.BuffRounds - 1)
    },

    test("Посох Владыки Ветров ускоряет героя за каждого задетого на 5 раундов") {
      val h = hero(DivineKind.WindLordStaff)
      for {
        t <- makeState(h, group(h, 100000L, 100000L, 100000L))
        (state, dao, r) = t
        _     <- state.action(testUser, tap("UseDivine"), r)
        after <- battleOf(dao)
        buff   = after.heroBattleState.buffs.head
      } yield assertTrue(after.heroBattleState.buffs.size == 1) &&
              assertTrue(buff.dodgePct == DivineRates.HastePctPerFoe * 3) &&
              assertTrue(buff.turnsLeft.contains(DivineRates.BuffRounds))
    },

    test("зарядов столько, сколько грейдов редкости: чёрная — 6 ударов, и на шестом божественное оружие рассыпается") {
      val h = hero(DivineKind.ThunderGodTrident)
      for {
        t <- makeState(h, group(h, 100000L, 100000L))
        (state, dao, r) = t
        // шесть ударов, между ними — ход героя, чтобы раунд сменился
        _ <- ZIO.foreachDiscard(1 to 6) { _ =>
               state.action(testUser, tap("UseDivine"), r) *>
                 TestRandom.feedInts(60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L) *>
                 state.action(testUser, aimed("Attack", 1), r)
             }
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        _       <- state.action(testUser, tap("UseDivine"), r)
        last    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(DivineKind.chargesFor(ItemRarity.Gray) == 6 && DivineKind.chargesFor(ItemRarity.Orange) == 12) &&
              assertTrue(screens.contains("рассыпается прямо в ваших руках")) &&
              assertTrue(updated.equipment.additionalWeapon.itemType == ItemType.NoItem) &&
              assertTrue(last.contains("В доп. слоте нет божественного оружия"))
    },

    test("удар добивает поле — это победа и добыча за всех") {
      val h = hero(DivineKind.ThunderGodTrident)
      for {
        t <- makeState(h, group(h, 10L, 10L))
        (state, dao, r) = t
        result  <- state.action(testUser, tap("UseDivine"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(result == StateType.Loot) && assertTrue(updated.exp > 0L)
    },

    test("божественное оружие не даёт статов, а её описание честно предупреждает о нестабильности") {
      val item = blade(DivineKind.DarkLordSword, ItemRarity.Orange, lvl = 42L)
      assertTrue(item.itemType == ItemType.AdditionalWeapon && item.lvl == 42L) &&
      assertTrue(List(item.attack, item.accuracy, item.energy, item.armor, item.defence, item.evasion, item.hp).forall(_ == 0L)) &&
      assertTrue(item.divine.exists(r => r.charges == 12 && r.maxCharges == 12)) &&
      assertTrue(item.statsLines == List(DivineKind.DarkLordSword.description)) &&
      assertTrue(item.statsLines.head.contains("развалится прямо в ладонях")) &&
      assertTrue(item.displayTitle == "🟠 [Ур.42] Меч Тёмного Владыки") &&
      // предмет переживает сериализацию: заряды не теряются
      assertTrue(item.asJson.as[Item].toOption.contains(item))
    }
  )
}
