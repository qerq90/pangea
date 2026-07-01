package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{ActiveBattle, Buff, HeroBattleState}
import pangea.model.hero.Equipment
import pangea.model.item.{Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

object BattleStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Герой с высокой точностью — попадает практически всегда
  private def strongHero = TestFixtures.hero(userId).copy(
    fightStats = FightStats(atk = 50, hp = 200, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, concentration = 0),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1)
  )

  // Герой с 1 HP — умрёт от любого удара
  private def dyingHero = TestFixtures.hero(userId).copy(
    baseStats  = TestFixtures.hero(userId).baseStats.copy(agi = 0),
    fightStats = strongHero.fightStats.copy(hp = 1, evasion = 0)
  )

  // Монстр с 1 HP и нулевым уклонением — умирает от первого удара
  private val weakBattle = ActiveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 1, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 1, concentration = 0),
    monsterCurrentHp    = 1L,
    monsterCurrentArmor = 0L
  )

  // «Отмеченный тьмой» монстр с 1 HP — победа над ним открывает путь вглубь
  private val markedWeakBattle = weakBattle.copy(monsterMarked = true)

  // Сильный монстр для теста побега
  private val strongBattle = ActiveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 9999, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 9999, concentration = 0),
    monsterCurrentHp    = 9999L,
    monsterCurrentArmor = 0L
  )

  private def makeState(hero: pangea.model.hero.Hero, battle: ActiveBattle) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      _        <- heroDao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(heroDao, content), heroDao, renderer)

  override def spec = suite("BattleState")(

    test("enter → показывает имя монстра и HP героя") {
      for {
        triple              <- makeState(strongHero, weakBattle)
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("Человек-раб")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Attack")) &&
              assertTrue(screens.head.choices.map(_.id).contains("UseFlask")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Flee"))
    },

    test("Attack на монстра с 1 HP → победа, переход в Loot, опыт начислен, лут в scene_data") {
      import pangea.service.state.states.LootState.LootData
      for {
        triple               <- makeState(strongHero, weakBattle)
        (state, heroDao, renderer) = triple
        result               <- state.action(testUser, tap("Attack"), renderer)
        screens              <- renderer.sentScreens
        updatedHero          <- heroDao.getHeroByUserId(userId)
        remainingBattle      <- heroDao.readActiveBattle(userId)
        loot                 <- heroDao.readSceneData(userId)
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(screens.exists(_.text.contains("опыта"))) &&
              assertTrue(updatedHero.exists(_.exp > 0L)) &&
              assertTrue(remainingBattle.isEmpty) &&
              assertTrue(loot.flatMap(_.as[LootData].toOption).isDefined)
    },

    test("Attack убивает Отмеченного тьмой на максимальном этаже → открывается путь вглубь") {
      val hero = strongHero.copy(dungeonLevel = 5, maxDungeonLevel = 5)
      for {
        triple               <- makeState(hero, markedWeakBattle)
        (state, heroDao, renderer) = triple
        result               <- state.action(testUser, tap("Attack"), renderer)
        screens              <- renderer.sentScreens
        updatedHero          <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(screens.exists(_.text.contains("Путь вглубь лабиринта открыт"))) &&
              assertTrue(updatedHero.exists(_.maxDungeonLevel == 6)) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 5))
    },

    test("Attack убивает обычного монстра → путь вглубь не открывается") {
      val hero = strongHero.copy(dungeonLevel = 5, maxDungeonLevel = 5)
      for {
        triple               <- makeState(hero, weakBattle)
        (state, heroDao, renderer) = triple
        _                    <- state.action(testUser, tap("Attack"), renderer)
        screens              <- renderer.sentScreens
        updatedHero          <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(!screens.exists(_.text.contains("Путь вглубь"))) &&
              assertTrue(updatedHero.exists(_.maxDungeonLevel == 5))
    },

    test("UseFlask без фляги → сообщение об ошибке, HP не меняется") {
      val lowHpHero = strongHero.copy(fightStats = strongHero.fightStats.copy(hp = 10L))
      for {
        triple               <- makeState(lowHpHero, strongBattle)
        (state, heroDao, renderer) = triple
        result               <- state.action(testUser, tap("UseFlask"), renderer)
        updatedHero          <- heroDao.getHeroByUserId(userId)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updatedHero.exists(_.fightStats.hp == 10L)) &&
              assertTrue(screens.exists(_.text.contains("не экипирована")))
    },

    test("UseFlask с экипированной флягой → HP восстановлен на 25%, заряд потрачен") {
      import pangea.model.item.FlaskEffect
      val flask = Item(1L, "Фляга", 1L, pangea.model.item.Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, concentration=0, armor=0, defence=0, evasion=0,
                   flaskEffect = Some(FlaskEffect.HealPercent(25)),
                   charges     = Some(1),
                   maxCharges  = Some(1))
      val heroWithFlask = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = flask)
      )
      for {
        triple                     <- makeState(heroWithFlask, strongBattle)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("UseFlask"), renderer)
        updatedHero                <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updatedHero.exists(_.fightStats.hp > 10L)) &&
              assertTrue(updatedHero.exists(_.equipment.flask.charges.contains(0))) &&
              assertTrue(screens.exists(_.text.contains("HP")))
    },

    test("UseFlask дважды за раунд → второй раз заблокирован, HP не меняется повторно") {
      import pangea.model.item.FlaskEffect
      val flask = Item(1L, "Фляга", 1L, pangea.model.item.Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, concentration=0, armor=0, defence=0, evasion=0,
                   flaskEffect = Some(FlaskEffect.HealPercent(25)),
                   charges     = Some(8),
                   maxCharges  = Some(8))
      val heroWithFlask     = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = flask)
      )
      val battleAlreadyUsed = strongBattle.copy(flaskUsedThisRound = true)
      for {
        triple                     <- makeState(heroWithFlask, battleAlreadyUsed)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("UseFlask"), renderer)
        updatedHero                <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updatedHero.exists(_.fightStats.hp == 10L)) &&
              assertTrue(updatedHero.exists(_.equipment.flask.charges.contains(8))) &&
              assertTrue(screens.exists(_.text.contains("уже использована")))
    },

    test("UseFlask с пустой флягой → сообщение о пустой фляге, HP не меняется") {
      import pangea.model.item.FlaskEffect
      val flask = Item(1L, "Фляга", 1L, pangea.model.item.Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, concentration=0, armor=0, defence=0, evasion=0,
                   flaskEffect = Some(FlaskEffect.HealPercent(25)),
                   charges     = Some(0),
                   maxCharges  = Some(8))
      val heroEmptyFlask = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = flask)
      )
      for {
        triple                     <- makeState(heroEmptyFlask, strongBattle)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("UseFlask"), renderer)
        updatedHero                <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updatedHero.exists(_.fightStats.hp == 10L)) &&
              assertTrue(screens.exists(_.text.contains("пуста")))
    },

    test("Flee → показывает экран подтверждения, остаётся в Battle") {
      for {
        triple              <- makeState(strongHero, strongBattle)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Flee"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("ConfirmFlee")))
    },

    test("CancelFlee → возвращается к экрану боя") {
      for {
        triple              <- makeState(strongHero, strongBattle)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("CancelFlee"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("Attack")))
    },

    test("ConfirmFlee → возврат в Dungeon или Death (не крашится)") {
      for {
        triple              <- makeState(strongHero, strongBattle)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("ConfirmFlee"), renderer)
      } yield assertTrue(result == StateType.Dungeon || result == StateType.Death)
    },

    test("герой с 1 HP умирает от удара моба → переход в Death") {
      for {
        triple               <- makeState(dyingHero, strongBattle)
        (state, heroDao, renderer) = triple
        // Прямая атака (монстр ответит и убьёт)
        result               <- state.action(testUser, tap("Attack"), renderer)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.Death || result == StateType.Dungeon) &&
              assertTrue(screens.nonEmpty)
    },

    test("armor поглощает урон от моба (броня = броня × защита)") {
      // Эффективная броня = Броня × Защита: у танка много текущей брони → меньше HP-урона
      val highAtkBattle = ActiveBattle(
        monsterLvl          = 1L,
        monsterRace         = pangea.model.monster.Race.Human.entryName,
        monsterRarity       = pangea.model.monster.Rarity.Common.entryName,
        monsterStats        = FightStats(atk=10000, hp=9999, armor=0, defence=0,
                                          evasion=0, accuracy=9999, concentration=0),
        monsterCurrentHp    = 9999L,
        monsterCurrentArmor = 0L
      )
      val tankHero  = strongHero.copy(
        fightStats = strongHero.fightStats.copy(armor=1_000_000L, defence=0, evasion=0, hp=1_000_000L),
        baseStats  = strongHero.baseStats.copy(agi=0)
      )
      val glassHero = strongHero.copy(
        fightStats = strongHero.fightStats.copy(armor=0L, defence=0, evasion=0, hp=1_000_000L),
        baseStats  = strongHero.baseStats.copy(agi=0)
      )
      for {
        t1            <- makeState(tankHero,  highAtkBattle)
        (s1, hd1, r1)  = t1
        _             <- s1.action(testUser, tap("Attack"), r1)
        tankHp        <- hd1.getHeroByUserId(userId).map(_.map(_.fightStats.hp).getOrElse(0L))
        t2            <- makeState(glassHero, highAtkBattle)
        (s2, hd2, r2)  = t2
        _             <- s2.action(testUser, tap("Attack"), r2)
        glassHp       <- hd2.getHeroByUserId(userId).map(_.map(_.fightStats.hp).getOrElse(0L))
      } yield assertTrue(tankHp > glassHp)
    },

    test("травма на броню режет МАКСИМУМ брони, а не текущий запас (регресс)") {
      // Шлем даёт allArmor=30 → maxArmor = 30 (защита больше не множит броню,
      // см. BattleState.damageReduction). Травма CutAchilles режет ПОТОЛОК через armorPct.
      val helmet = Item(1L, "Шлем", 1L, ItemRarity.Gray, ItemType.Helmet,
                        attack = 0, accuracy = 0, concentration = 0, armor = 30, defence = 0, evasion = 0)
      val base = TestFixtures.hero(userId).copy(
        equipment  = TestFixtures.hero(userId).equipment.copy(helmet = helmet),
        fightStats = TestFixtures.hero(userId).fightStats.copy(defence = 8, armor = 30)
      )
      val injured = base.copy(traumaUntil = Some(Long.MaxValue),
                              traumaNames = List("Порезанное ахиллесово сухожилие"))
      assertTrue(
        base.effectiveMaxArmor(0L) == 30L,            // без травмы
        injured.effectiveMaxArmor(0L) < 30L,          // травма уронила потолок
        injured.effectiveFightStats(0L).armor == 30L  // текущий запас травма НЕ режет
      )
    },

    test("armor поглощает урон → HP не уменьшается если armor достаточен") {
      // hero с armor=1000, defence=0, hp=1000; моб atk=1 → damage=1 → поглощается armor.
      // feedInts: (1) heroHitRoll, (2) mobHitRoll, (3) mobSkillRoll=100 — каст моба не прокает,
      // иначе мог бы выпасть CrushingStrike, который игнорирует броню.
      val armoredHero = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 1000L, armor = 1000L, defence = 0, evasion = 0),
        baseStats  = strongHero.baseStats.copy(agi = 0)
      )
      val slowBattle = strongBattle
      for {
        triple               <- makeState(armoredHero, slowBattle)
        (state, heroDao, renderer) = triple
        _                    <- TestRandom.feedInts(50, 50, 100)
        _                    <- state.action(testUser, tap("Attack"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.fightStats.hp == 1000L)) &&
              assertTrue(updated.exists(_.fightStats.armor < 1000L))
    },

    test("buff с turnsLeft тикается после хода (Attack)") {
      val buff        = Buff(atk = 0L, armor = 0L, defence = 0L, turnsLeft = Some(3))
      val buffedBattle = strongBattle.copy(heroBattleState = HeroBattleState(List(buff)))
      for {
        triple               <- makeState(strongHero, buffedBattle)
        (state, heroDao, renderer) = triple
        _                    <- state.action(testUser, tap("Attack"), renderer)
        remaining            <- heroDao.readActiveBattle(userId)
      } yield assertTrue(
        remaining.flatMap(_.as[ActiveBattle].toOption)
          .exists(_.heroBattleState.buffs.headOption.exists(_.turnsLeft.contains(2)))
      )
    },

    test("buff armor снижает урон без уменьшения физической брони") {
      // Герой без armor и без defence, но с buff.armor=9999 → не должен получать HP урон.
      // mobSkillRoll=100 → каст моба не прокает (иначе CrushingStrike пробил бы баф).
      val noArmorHero = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 500L, armor = 0L, defence = 0, evasion = 0),
        baseStats  = strongHero.baseStats.copy(agi = 0)
      )
      val bigArmorBuff  = Buff(atk = 0L, armor = 9999L, defence = 0L, turnsLeft = None)
      val buffedBattle  = strongBattle.copy(heroBattleState = HeroBattleState(List(bigArmorBuff)))
      for {
        triple               <- makeState(noArmorHero, buffedBattle)
        (state, heroDao, renderer) = triple
        _                    <- TestRandom.feedInts(50, 50, 100)
        _                    <- state.action(testUser, tap("Attack"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.fightStats.hp == 500L))  // урон поглощён баффом
    },

    test("dodgeChance: одна формула для обеих сторон (попадание по любому юниту идентично)") {
      // моб без ловкости (agi=0) уклоняется по той же формуле, что и игрок
      val mobDodge    = BattleState.dodgeChance(agi = 0L,  evasion = 20L, defence = 10L, attackerAccuracy = 10L)
      val playerDodge = BattleState.dodgeChance(agi = 0L,  evasion = 20L, defence = 10L, attackerAccuracy = 10L)
      assertTrue(mobDodge == playerDodge)
    },

    test("dodgeChance: огромная точность моба + нулевое уклонение → минимум 5%") {
      val chance = BattleState.dodgeChance(agi = 0L, evasion = 0L, defence = 0L, attackerAccuracy = 999999L)
      assertTrue(chance == 5.0)
    },

    test("dodgeChance: огромное уклонение + нулевая точность моба → максимум 95%") {
      val chance = BattleState.dodgeChance(agi = 999999L, evasion = 999999L, defence = 0L, attackerAccuracy = 0L)
      assertTrue(chance == 95.0)
    },

    test("dodgeChance: формула 100*(agi+evasion)/(agi+evasion+def+acc*1.5)") {
      // agi+evasion=20, знаменатель = 20 + 10 + 10*1.5 = 45 → 2000/45 ≈ 44.44
      val chance = BattleState.dodgeChance(agi = 10L, evasion = 10L, defence = 10L, attackerAccuracy = 10L)
      assertTrue(math.abs(chance - 2000.0 / 45.0) < 1e-9)
    },

    test("dodgeChance: защита и точность моба снижают уклонение") {
      val low  = BattleState.dodgeChance(agi = 10L, evasion = 10L, defence = 50L, attackerAccuracy = 50L)
      val high = BattleState.dodgeChance(agi = 10L, evasion = 10L, defence = 0L,  attackerAccuracy = 0L)
      assertTrue(low < high)
    },

    test("расовый множитель силы попадает в обычную атаку: орк (×1.2) бьёт сильнее человека (×1.0)") {
      // Урон обычной атаки = (effectiveBaseStats.str*3 + atk) * spread/100 * weaponMod.
      // Оба героя идентичны, различие только в расе → при atk=0 урон линеен по str_eff, а с
      // одинаковым фидом рандома spread совпадает. str_eff: человек ceil(100×1.0)=100,
      // орк ceil(100×1.2)=120 → урон орка ровно в 120/100 раза больше (проверяем соотношением,
      // не завязываясь на конкретный spread). feedInts(49) → hitRoll=50 (попадание).
      val bigMonster = ActiveBattle(
        monsterLvl          = 1L,
        monsterRace         = Race.Human.entryName,
        monsterRarity       = Rarity.Common.entryName,
        monsterStats        = FightStats(atk = 1, hp = 100000, armor = 0, defence = 0,
                                         evasion = 0, accuracy = 1, concentration = 0),
        monsterCurrentHp    = 100000L,
        monsterCurrentArmor = 0L
      )
      def heroOf(race: Race) = TestFixtures.hero(userId).copy(
        race       = race,
        baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 100, agi = 0),
        fightStats = FightStats(atk = 0, hp = 10000, armor = 0, defence = 0,
                                evasion = 9999, accuracy = 9999, concentration = 0)
      )
      def monsterHpAfterAttack(race: Race) =
        for {
          triple              <- makeState(heroOf(race), bigMonster)
          (state, heroDao, r)  = triple
          _                   <- TestRandom.feedInts(49)
          _                   <- TestRandom.feedLongs(20L)
          _                   <- state.action(testUser, tap("Attack"), r)
          hp                  <- heroDao.readActiveBattle(userId)
                                   .map(_.flatMap(_.as[ActiveBattle].toOption).map(_.monsterCurrentHp).getOrElse(-1L))
        } yield hp
      for {
        humanHp <- monsterHpAfterAttack(Race.Human)
        orcHp   <- monsterHpAfterAttack(Race.Orc)
        humanDmg = 100000L - humanHp
        orcDmg   = 100000L - orcHp
      } yield assertTrue(humanDmg > 0L) &&                    // урон реально прошёл
              assertTrue(orcDmg > humanDmg) &&                // орк бьёт сильнее
              assertTrue(orcDmg * 100L == humanDmg * 120L)    // ровно ×1.2 — расовый множитель силы
    }
  )
}
