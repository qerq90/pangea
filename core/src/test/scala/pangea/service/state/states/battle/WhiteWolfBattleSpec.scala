package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{Bleed, Element, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{FlaskEffect, Item, ItemDetails, ItemSet, ItemType, MaterialKind, Rarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.stats.FightStats
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Белый волк: растёт раз в четыре уровня, бьёт холодом и рвёт до крови, круг из
 *  пяти умений с двойным первым и критом дальше, а сам истекает ядом и кровью
 *  сильнее прочих. */
object WhiteWolfBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val wolf    = MiniBoss.WhiteWolf
  private val bossLvl = 2L // герой 9 уровня → BossLvL = (9−1)/4 = 2

  private def weapon: Item =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def healFlask: Item =
    Item(1L, "Фляга", 1L, Rarity.Gray, ItemType.Flask,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 1, maxCharges = 1))

  /** Герой бьёт без промаха и переживает удары волка. `armor` — его броня. */
  private def hero(atk: Long = 100L, hp: Long = 500000L, armor: Long = 0L): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 9L,
      dungeonLevel = 13,
      fightStats = FightStats(atk = atk, hp = hp, armor = armor, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon)
    )

  private def wolfBattle(
      h: Hero,
      turn: Int,
      energy: Long = 400L,  // максимум волка: 200 × BossLvL
      firstSpent: Boolean = false,
      hp: Option[Long] = None
  ): SoloPveBattle = {
    val stats   = wolf.stats(bossLvl)
    val monster = Monster(0L, bossLvl, wolf.race, MobRarity.Legendary, stats)
    val base    = SoloPveBattle.from(monster, h).copy(
      bossKind             = Some(wolf.entryName),
      bossTurn             = turn,
      bossFirstSkillSpent  = firstSpent,
      monsterCurrentEnergy = energy
    )
    hp.fold(base)(v => base.copy(monsterCurrentHp = v))
  }

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  /** Броски хода: удар героя, попадание моба, прок холода у моба (50 — нет), затем `extra`.
    * Удары волка по герою с его запасом HP мелкие — броска травмы нет. */
  private def seedTurn(extra: Int*) =
    TestRandom.feedInts(60 +: 90 +: 50 +: extra: _*) *> TestRandom.feedLongs(100L, 100L)

  private def strike(h: Hero, battle: SoloPveBattle, seed: ZIO[Any, Nothing, Unit]) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- seed
      out     <- state.action(testUser, tap("Attack"), r)
      updated <- dao.getHeroByUserId(userId).map(_.get)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens <- r.sentScreens
    } yield (updated, after, screens.map(_.text).mkString("\n"), out)

  private val MobHit = """наносит (\d+) урона""".r

  private def mobHitDamage(log: String): Long =
    MobHit.findAllMatchIn(log).map(_.group(1).toLong).toList.headOption.getOrElse(-1L)

  override def spec = suite("Белый волк")(

    // ── Статы и особенности ───────────────────────────────────────────────────
    test("статы считаются от BossLvL, а BossLvL — раз в четыре уровня героя") {
      val s = wolf.stats(bossLvl)
      assertTrue(wolf.bossLvl(9L) == 2L && wolf.bossLvl(4L) == 1L && wolf.bossLvl(5L) == 1L && wolf.bossLvl(1L) == 1L) &&
      assertTrue(wolf.bossLvl(13L) == 3L && MiniBoss.bossLvl(13L) == 2L) && // у элементалей по-прежнему раз в пять
      assertTrue(s.hp == 1000L * bossLvl && s.armor == 650L * bossLvl && s.atk == 300L * bossLvl) &&
      assertTrue(s.energy == 200L * bossLvl && s.accuracy == 450L * bossLvl) &&
      assertTrue(s.defence == 50L * bossLvl && s.evasion == 200L * bossLvl) &&
      assertTrue(wolf.energyRegen(bossLvl) == 14L * bossLvl && wolf.expReward(bossLvl) == 150L * bossLvl) &&
      assertTrue(wolf.monsterName == "Белый Волк" && wolf.abilities == 5)
    },

    test("зверь: яд и кровь его берут (на 20% сильнее), стихии оружия безразличны, бьёт холодом") {
      assertTrue(wolf.race == Race.Animal) &&
      assertTrue(!Race.immuneToDots(Race.Animal)) &&
      assertTrue(Race.bossRaces.contains(Race.Animal) && !Race.mortals.contains(Race.Animal)) &&
      assertTrue(wolf.dotDamageTakenMult == 1.2) &&
      assertTrue(Element.values.forall(e => wolf.damageTakenMult(e) == 1.0) && wolf.plainDamageTakenMult == 1.0) &&
      assertTrue(wolf.attackElement.contains(Element.Cold) && !wolf.immuneToBurn) &&
      assertTrue(wolf.ingredient == MaterialKind.WhiteWolfHide && wolf.set == ItemSet.Hunter)
    },

    test("экран боя: имя «Белый Волк» и раса «Животное»") {
      val h = hero()
      for {
        t <- makeState(h, wolfBattle(h, turn = 3))
        (state, _, r) = t
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.text.contains("Белый Волк") && screen.text.contains("Раса: Животное"))
    },

    // ── Обычная атака: холод ──────────────────────────────────────────────────
    test("обычная атака бьёт холодом: по броне на 10% больше, а прок сковывает защиту героя") {
      val h = hero(armor = 10000L)
      for {
        calm <- strike(h, wolfBattle(h, turn = 3), seedTurn())
        (calmHero, calmAfter, calmLog, _) = calm
        cold <- strike(h, wolfBattle(h, turn = 3), TestRandom.feedInts(60, 90, 10) *> TestRandom.feedLongs(100L, 100L))
        (_, coldAfter, coldLog, _) = cold
        dealt = mobHitDamage(calmLog)
      } yield assertTrue(dealt > 0L) &&
              // броня приняла удар с гранью холода (+10%), HP не тронуто
              assertTrue(10000L - calmHero.fightStats.armor == dealt * 110L / 100L) &&
              assertTrue(calmHero.fightStats.hp == 500000L) &&
              assertTrue(calmAfter.get.effects.heroColdDefenceCut == 0 && !calmLog.contains("Морозный удар")) &&
              assertTrue(coldLog.contains("Морозный удар сковывает вас")) &&
              assertTrue(coldAfter.get.effects.heroColdDefenceCut == Element.Cold.DefenceReductionCut)
    },

    // ── Способности по кругу ──────────────────────────────────────────────────
    test("яростная пасть: первое умение боя бьёт вдвое (половина атаки × 2) и пускает кровь") {
      val h = hero()
      for {
        r <- strike(h, wolfBattle(h, turn = 0), seedTurn())
        (u, after, log, _) = r
      } yield assertTrue(log.contains("Яростный укус наносит 600 урона!")) && // 600 × 0,5 × 2
              assertTrue(log.contains("критический удар")) &&
              assertTrue(after.get.bossFirstSkillSpent) &&
              assertTrue(after.get.effects.heroBleed.contains(Bleed(wolf.FangsBleedPct))) &&
              assertTrue(log.contains("Рана кровоточит") && log.contains("Кровотечение снимает с вас")) &&
              assertTrue(after.get.bossTurn == 1) &&
              // 400 − 14 + 28 упирается в потолок 400: реген 14 × BossLvL перекрывает цену умения
              assertTrue(after.get.monsterCurrentEnergy == (400L - wolf.FangsCostPerLvl * bossLvl + wolf.energyRegen(bossLvl)).min(400L)) &&
              assertTrue(u.fightStats.hp < 500000L - 600L)
    },

    test("дальше двойной урон — только по шансу 5%: когти 240 или 480 с репликой о крите") {
      val h = hero()
      for {
        plain <- strike(h, wolfBattle(h, turn = 1, firstSpent = true), seedTurn(50))
        crit  <- strike(h, wolfBattle(h, turn = 1, firstSpent = true), seedTurn(5))
      } yield assertTrue(plain._3.contains("разодрать вам живот. Вы получили 240 урона!")) && // 600 × 0,4
              assertTrue(!plain._3.contains("критический удар")) &&
              assertTrue(crit._3.contains("Вы получили 480 урона!") && crit._3.contains("Белый Волк наносит критический удар!")) &&
              assertTrue(plain._2.get.effects.heroBleed.contains(Bleed(wolf.ClawsBleedPct)))
    },

    test("кровь только при уроне по HP: за бронёй когти её не пускают; повторная рана стакает") {
      val armored = hero(armor = 10000L)
      val bleeding = hero()
      for {
        safe  <- strike(armored, wolfBattle(armored, turn = 1, firstSpent = true), seedTurn(50))
        base   = wolfBattle(bleeding, turn = 1, firstSpent = true)
        again <- strike(bleeding, base.copy(effects = base.effects.copy(heroBleed = Some(Bleed(2)))), seedTurn(50))
      } yield assertTrue(safe._2.get.effects.heroBleed.isEmpty && !safe._3.contains("Рана кровоточит")) &&
              assertTrue(again._2.get.effects.heroBleed.contains(Bleed(4)))
    },

    test("животный инстинкт: 4 хода атака и уклонение выше на 5%") {
      val h = hero()
      for {
        cast  <- strike(h, wolfBattle(h, turn = 2), seedTurn())
        plain <- strike(h, wolfBattle(h, turn = 3), seedTurn())
        base   = wolfBattle(h, turn = 3)
        keen  <- strike(h, base.copy(effects = base.effects.copy(mobInstinctTurns = 2)), seedTurn())
        plainDmg = mobHitDamage(plain._3)
        keenDmg  = mobHitDamage(keen._3)
      } yield assertTrue(cast._3.contains("Волк подстраивается под атаки своей добычи")) &&
              assertTrue(cast._2.get.effects.mobInstinctTurns == wolf.InstinctTurns) &&
              assertTrue(plainDmg > 0L && keenDmg > plainDmg && keenDmg <= plainDmg * 106L / 100L) &&
              assertTrue(keen._2.get.effects.mobInstinctTurns == 1) // тик в начале хода моба
    },

    test("смыкание пасти: четверть недостающего HP плюс пятая часть атаки, бесплатно") {
      val full    = hero()
      val maxHp   = full.effectiveMaxHp(0L)
      val wounded = full.copy(fightStats = full.fightStats.copy(hp = maxHp - 40000L))
      for {
        // моб промахивается обычной атакой (1 ≤ 5% уклонения), чтобы недостача была ровно 40 000
        r <- strike(wounded, wolfBattle(wounded, turn = 4, firstSpent = true, energy = 0L),
               TestRandom.feedInts(60, 1, 50, 50) *> TestRandom.feedLongs(100L)) // крит нет, травма от приёма нет
        (u, after, log, _) = r
      } yield assertTrue(log.contains("вгрызться в вашу шею! Вы получили 10120 урона!")) && // 40 000 × 25% + 600 × 0,2
              assertTrue(u.fightStats.hp == maxHp - 40000L - 10120L) &&
              assertTrue(after.get.bossTurn == 0) && // круг замкнулся
              assertTrue(after.get.effects.heroBleed.isEmpty) // пасть не режет
    },

    test("без энергии способность не применяется, но очередь едет дальше") {
      val h = hero()
      for {
        r <- strike(h, wolfBattle(h, turn = 0, energy = 0L), seedTurn())
        (_, after, log, _) = r
      } yield assertTrue(!log.contains("Яростный укус")) &&
              assertTrue(after.get.effects.heroBleed.isEmpty && !after.get.bossFirstSkillSpent) &&
              assertTrue(after.get.bossTurn == 1)
    },

    // ── Кровотечение на герое ─────────────────────────────────────────────────
    test("кровь на герое тикает процентом от макс.HP и не затухает; фляга её снимает") {
      val h    = hero(armor = 10000L).copy(equipment = TestFixtures.emptyEquipment.copy(weapon = weapon, flask = healFlask))
      val base = wolfBattle(h, turn = 3)
      val bled = base.copy(effects = base.effects.copy(heroBleed = Some(Bleed(2))))
      for {
        tick <- strike(h, bled, seedTurn())
        (u, after, log, _) = tick
        maxHp = h.effectiveMaxHp(0L)
        t    <- makeState(h, bled)
        (state, dao, r) = t
        _    <- TestRandom.feedInts(90, 50) *> TestRandom.feedLongs(100L) // попадание моба и прок холода
        _    <- state.action(testUser, tap("UseFlask"), r)
        healed <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      } yield assertTrue(500000L - u.fightStats.hp == maxHp * 2L / 100L) &&
              assertTrue(log.contains(s"Кровотечение снимает с вас ${maxHp * 2L / 100L} HP")) &&
              assertTrue(after.get.effects.heroBleed.contains(Bleed(2))) &&
              assertTrue(healed.exists(_.effects.heroBleed.isEmpty))
    },

    // ── Яд и кровь по волку ───────────────────────────────────────────────────
    test("яд и кровотечение снимают с волка на 20% больше") {
      val h    = hero()
      val base = wolfBattle(h, turn = 3)
      val dotted = base.copy(effects = base.effects.copy(monsterPoison = Some(Poison(10)), monsterBleed = Some(Bleed(10))))
      for {
        r <- strike(h, dotted, seedTurn())
        (_, after, log, _) = r
        maxHp = wolf.stats(bossLvl).hp
        expected = maxHp * 10L / 100L * 120L / 100L
      } yield assertTrue(log.contains(s"Яд снимает $expected HP") && log.contains(s"🔴 -$expected")) &&
              assertTrue(after.get.monsterCurrentHp == maxHp - 2L * expected) // удар героя ушёл в броню
    },

    // ── Победа ────────────────────────────────────────────────────────────────
    test("победа: 150 × BossLvL опыта, своя реплика, добыча ведёт обратно на поляну, клык — этажом встречи") {
      val h = hero(atk = 100000L)
      val routing = LootState.LootData(Nil, Nil, returnState = Some(StateType.FlowerMeadow))
      for {
        t <- makeState(h, wolfBattle(h, turn = 3, hp = Some(1L)))
        (state, dao, r) = t
        _       <- dao.writeSceneData(userId, routing.asJson)
        _       <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L, 7L)
        out     <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        log     <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        loot    <- dao.readSceneData(userId).map(_.flatMap(_.as[LootState.LootData].toOption).get)
      } yield assertTrue(out == StateType.Loot) &&
              assertTrue(updated.exp == 300L) && assertTrue(log.contains("Получено 300 опыта")) &&
              assertTrue(log.contains("Волк затих") && !log.contains("Надо посмотреть вокруг")) &&
              assertTrue(loot.returnState.contains(StateType.FlowerMeadow)) &&
              assertTrue(loot.items.nonEmpty) &&
              assertTrue(loot.items.count(_.material.contains(MaterialKind.WhiteWolfHide)) <= 1) &&
              // клык — этажом встречи и с коэффициентом 6 × BossLvL
              assertTrue(loot.items.filter(_.itemType == ItemType.Trophy).forall(f => f.lvl == 13L && f.name == "Клык Белого волка"))
    }
  )
}
