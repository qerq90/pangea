package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{Buff, Burn, HeroBattleState, Regen, SoloPveBattle}
import pangea.model.item.{Gem, GemKind, Item, ItemDetails, ItemType, PotionKind, Rarity => ItemRarity}
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

  private def flaskCharges(i: Item): Option[Int] = i.details match {
    case f: ItemDetails.Flask => Some(f.charges)
    case _                    => None
  }

  private def beltCharges(i: Item): Option[Int] = i.details match {
    case b: ItemDetails.Belt => Some(b.charges)
    case _                   => None
  }

  private def belt(potion: PotionKind, charges: Int, maxCharges: Int = 5): Item =
    Item(9L, "Пояс", 1L, ItemRarity.Green, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(potion, charges = charges, maxCharges = maxCharges))

  private def healFlask(charges: Int): Item =
    Item(1L, "Фляга", 1L, ItemRarity.Gray, ItemType.Flask,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Flask(pangea.model.item.FlaskEffect.HealPercent(25), charges = charges, maxCharges = charges))

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Герой с высокой точностью — попадает практически всегда
  private def strongHero = TestFixtures.hero(userId).copy(
    fightStats = FightStats(atk = 50, hp = 200, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 0),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1)
  )

  // ── Снаряжение набора «Упырь» ───────────────────────────────────────────────
  private def setPiece(id: Long, itemType: ItemType, armor: Long = 0L): Item =
    Item(id, "Предмет", 1L, ItemRarity.Blue, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = armor, defence = 0, evasion = 0,
      set = Some(pangea.model.item.ItemSet.Ghoul))

  private def armorPiece(id: Long, armor: Long): Item =
    Item(id, "Нагрудник", 1L, ItemRarity.Blue, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = armor, defence = 0, evasion = 0,
      set = Some(pangea.model.item.ItemSet.Ghoul))

  /** Экипировка с `n` предметами «Упыря» в сетовых слотах (порядок как в Equipment.setSlots). */
  private def ghoulEquipment(n: Int): pangea.model.hero.Equipment = {
    val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
      ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = setPiece(200L + i, t)
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
  }

  private def ghoulHero(pieces: Int) = strongHero.copy(equipment = ghoulEquipment(pieces))

  /** То же для «Охотника»: `n` предметов набора в сетовых слотах. */
  private def hunterEquipment(n: Int): pangea.model.hero.Equipment = {
    val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
      ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = Item(300L + i, "Предмет", 1L, ItemRarity.Blue, t,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        set = Some(pangea.model.item.ItemSet.Hunter))
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
  }

  private def hunterHero(pieces: Int) = strongHero.copy(equipment = hunterEquipment(pieces))

  /** Экипировка набора `set` на `n` предметов; оружие можно задать своё
   *  (например с камнем стихии) — оно займёт двенадцатый слот. */
  private def setEquipment(set: pangea.model.item.ItemSet, n: Int,
                           weapon: Option[Item]): pangea.model.hero.Equipment = {
    val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
      ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)
    val eq = slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (acc, (t, i)) =>
      val it = Item(400L + i, "Предмет", 1L, ItemRarity.Blue, t,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, set = Some(set))
      i match {
        case 0  => acc.copy(helmet = it)
        case 1  => acc.copy(shoulderPads = it)
        case 2  => acc.copy(chestPlate = it)
        case 3  => acc.copy(bracelets = it)
        case 4  => acc.copy(gloves = it)
        case 5  => acc.copy(pants = it)
        case 6  => acc.copy(boots = it)
        case 7  => acc.copy(amulet = it)
        case 8  => acc.copy(firstRing = it)
        case 9  => acc.copy(secondRing = it)
        case 10 => acc.copy(belt = it)
        case _  => acc.copy(weapon = it)
      }
    }
    weapon.fold(eq)(w => eq.copy(weapon = w))
  }

  /** Меч набора «Дикое пламя» с Рубином (Огонь) в гнезде. */
  private def flameWeapon(grade: Int = 1) =
    Item(500L, "Пламенный меч", 1L, ItemRarity.Blue, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = List(Some(Gem(GemKind.Ruby, grade))),
      set = Some(pangea.model.item.ItemSet.WildFlame))

  // Герой с 1 HP — умрёт от любого удара
  private def dyingHero = TestFixtures.hero(userId).copy(
    baseStats  = TestFixtures.hero(userId).baseStats.copy(agi = 0),
    fightStats = strongHero.fightStats.copy(hp = 1, evasion = 0)
  )

  // Монстр с 1 HP и нулевым уклонением — умирает от первого удара
  private val weakBattle = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 1, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = 1L,
    monsterCurrentArmor = 0L
  )

  // «Отмеченный тьмой» монстр с 1 HP — победа над ним открывает путь вглубь
  private val markedWeakBattle = weakBattle.copy(monsterMarked = true)

  // Сильный монстр для теста побега
  private val strongBattle = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 9999, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 9999, energy = 0),
    monsterCurrentHp    = 9999L,
    monsterCurrentArmor = 0L
  )

  private def makeState(hero: pangea.model.hero.Hero, battle: SoloPveBattle) =
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

    // ── Горение съедает лечение ───────────────────────────────────────────────
    test("горящий герой лечится флягой слабее: −(50% + процент горения)") {
      val heroWithFlask = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = healFlask(1)))
      def healedWith(burn: Option[Int]) = {
        val b = strongBattle.copy(effects = strongBattle.effects.copy(heroBurn = burn.map(Burn(_))))
        for {
          t <- makeState(heroWithFlask, b)
          (state, heroDao, renderer) = t
          _    <- state.action(testUser, tap("UseFlask"), renderer)
          hero <- heroDao.getHeroByUserId(userId).map(_.get)
        } yield hero.fightStats.hp - 10L
      }
      for {
        clean  <- healedWith(None)
        burned <- healedWith(Some(10)) // 50 + 10 = 60% лечения долой
      } yield assertTrue(clean > 0L) &&
              assertTrue(burned == clean * 40L / 100L)
    },

    test("при 50% горения лечение обнуляется, но герой продолжает гореть") {
      val heroWithFlask = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = healFlask(1)))
      val burning = strongBattle.copy(effects = strongBattle.effects.copy(heroBurn = Some(Burn(60))))
      for {
        t <- makeState(heroWithFlask, burning)
        (state, heroDao, renderer) = t
        _       <- state.action(testUser, tap("UseFlask"), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        after   <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        screens <- renderer.sentScreens
      } yield assertTrue(hero.fightStats.hp == 10L) && // лечение съедено целиком
              // горение никуда не делось — оно тикает дальше и даже растёт
              assertTrue(after.effects.heroBurn.exists(_.pct >= 60)) &&
              assertTrue(screens.map(_.text).mkString.contains("Пламя пожирает лечение"))
    },

    test("зелье лечения из пояса горящему герою тоже помогает хуже") {
      def healedWith(burn: Option[Int]) = {
        val h = strongHero.copy(
          fightStats = strongHero.fightStats.copy(hp = 10L),
          equipment  = TestFixtures.emptyEquipment.copy(belt = belt(PotionKind.Healing, 1, 1)))
        val b = strongBattle.copy(effects = strongBattle.effects.copy(heroBurn = burn.map(Burn(_))))
        for {
          t <- makeState(h, b)
          (state, heroDao, renderer) = t
          _    <- state.action(testUser, tap("UseBelt"), renderer)
          hero <- heroDao.getHeroByUserId(userId).map(_.get)
        } yield hero.fightStats.hp - 10L
      }
      for {
        clean  <- healedWith(None)
        burned <- healedWith(Some(20)) // 50 + 20 = 70% долой
      } yield assertTrue(clean > 0L) && assertTrue(burned == clean * 30L / 100L)
    },

    test("регенерация тикает в полную силу: горение режет только активное лечение") {
      def regenTick(burn: Option[Int]) = {
        val h = strongHero.copy(fightStats = strongHero.fightStats.copy(hp = 10L))
        val b = strongBattle.copy(effects = strongBattle.effects.copy(
          heroRegen = Some(Regen(20)), heroBurn = burn.map(Burn(_))))
        for {
          t <- makeState(h, b)
          (state, _, renderer) = t
          _       <- TestRandom.feedInts(60, 1) *> TestRandom.feedLongs(100L, 100L)
          _       <- state.action(testUser, tap("Attack"), renderer)
          screens <- renderer.sentScreens
          // Итоговое HP сравнивать нельзя: у горящего его же и подъедает огонь.
          // Смотрим на саму строку регенерации — сколько она вылечила.
        } yield screens.map(_.text).mkString
          .linesIterator.find(_.contains("Регенерация восстанавливает")).getOrElse("")
      }
      for {
        clean  <- regenTick(None)
        burned <- regenTick(Some(10))
      } yield assertTrue(clean.nonEmpty) &&
              assertTrue(burned == clean) // тик регенерации горение не трогает
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
                   attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                   details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 1, maxCharges = 1))
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
              assertTrue(updatedHero.exists(h => flaskCharges(h.equipment.flask).contains(0))) &&
              assertTrue(screens.exists(_.text.contains("HP")))
    },

    test("UseFlask дважды за раунд → второй раз заблокирован, HP не меняется повторно") {
      import pangea.model.item.FlaskEffect
      val flask = Item(1L, "Фляга", 1L, pangea.model.item.Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                   details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 8, maxCharges = 8))
      val heroWithFlask     = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = flask)
      )
      val battleAlreadyUsed = strongBattle.copy(consumableUsedThisRound = true)
      for {
        triple                     <- makeState(heroWithFlask, battleAlreadyUsed)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("UseFlask"), renderer)
        updatedHero                <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updatedHero.exists(_.fightStats.hp == 10L)) &&
              assertTrue(updatedHero.exists(h => flaskCharges(h.equipment.flask).contains(8))) &&
              assertTrue(screens.exists(_.text.contains("уже использовали")))
    },

    test("UseFlask с пустой флягой → сообщение о пустой фляге, HP не меняется") {
      import pangea.model.item.FlaskEffect
      val flask = Item(1L, "Фляга", 1L, pangea.model.item.Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                   details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 0, maxCharges = 8))
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

    test("enter → кнопка «Пояс» показана, если пояс несёт зелье") {
      val hero = strongHero.copy(equipment = TestFixtures.emptyEquipment.copy(belt = belt(PotionKind.Healing, 3)))
      for {
        triple              <- makeState(hero, strongBattle)
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.head.choices.map(_.id).contains("UseBelt"))
    },

    test("enter → кнопки «Пояс» нет, если пояс без зелья") {
      for {
        triple              <- makeState(strongHero, strongBattle) // emptyEquipment: пояс — NoItem
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(!screens.head.choices.map(_.id).contains("UseBelt"))
    },

    test("UseBelt зелье лечения → HP восстановлен, заряд потрачен") {
      val hero = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(belt = belt(PotionKind.Healing, 2))
      )
      for {
        triple                     <- makeState(hero, strongBattle)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("UseBelt"), renderer)
        updated                    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updated.exists(_.fightStats.hp > 10L)) &&
              assertTrue(updated.exists(h => beltCharges(h.equipment.belt).contains(1)))
    },

    test("Фляга и пояс делят лимит: после фляги пояс в этом раунде заблокирован") {
      val hero = strongHero.copy(
        fightStats = strongHero.fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = healFlask(8), belt = belt(PotionKind.Healing, 2))
      )
      for {
        triple                     <- makeState(hero, strongBattle)
        (state, heroDao, renderer)  = triple
        _                          <- state.action(testUser, tap("UseFlask"), renderer)
        result                     <- state.action(testUser, tap("UseBelt"), renderer)
        updated                    <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updated.exists(h => beltCharges(h.equipment.belt).contains(2))) && // заряд не потрачен
              assertTrue(screens.exists(_.text.contains("уже использовали")))
    },

    test("UseBelt зелье яда → баф ядовитых атак; следующая атака травит моба") {
      val hero = strongHero.copy(equipment = TestFixtures.emptyEquipment.copy(belt = belt(PotionKind.Poison, 1)))
      for {
        triple                     <- makeState(hero, strongBattle)
        (state, heroDao, renderer)  = triple
        _                          <- state.action(testUser, tap("UseBelt"), renderer)
        afterDrink                 <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _                          <- state.action(testUser, tap("Attack"), renderer)
        afterAttack                <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(afterDrink.effects.heroPoisonousAttacks) &&
              assertTrue(afterDrink.effects.monsterPoison.isEmpty) &&
              assertTrue(!afterAttack.effects.heroPoisonousAttacks) &&
              assertTrue(afterAttack.effects.monsterPoison.isDefined)
    },

    test("Огонь в оружии: прок (30%) поджигает моба — накладывается горение") {
      // Меч с камнем Рубин (Огонь) в гнезде.
      val fireWeapon = Item(2L, "Пламенный меч", 1L, ItemRarity.Blue, ItemType.Weapon,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        sockets = List(Some(Gem(GemKind.Ruby, 1))))
      val hero = strongHero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = fireWeapon))
      for {
        triple                     <- makeState(hero, strongBattle)
        (state, heroDao, renderer)  = triple
        // Порядок бросков: удар героя(hit) → прок огня(≤30) → удар моба(miss) → каст моба(нет).
        _                          <- TestRandom.feedInts(60, 10, 3, 90)
        _                          <- TestRandom.feedLongs(20L)
        _                          <- state.action(testUser, tap("Attack"), renderer)
        after                      <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(after.effects.monsterBurn.isDefined)
    },

    test("огонь в бою: по HP ×(1.10+усиление), по броне ×(0.80+усиление), общий урон не накручивается") {
      // Рубин грейд 5 → усиление +10 п.п.: по HP ×1.20, по броне ×0.90.
      // Урон до стихии: (str 1×3 + атака 500) × spread 100% = 503.
      // Регрессия: раньше усиление множило ВЕСЬ урон (503×1.10=553), а грани
      // оставались базовыми — выходило 608 по HP и 442 по броне вместо 603 и 452.
      val fireWeapon = Item(2L, "Пламенный меч", 1L, ItemRarity.Blue, ItemType.Weapon,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        sockets = List(Some(Gem(GemKind.Ruby, 5))))
      val hero = strongHero.copy(
        fightStats = strongHero.fightStats.copy(atk = 500),
        equipment  = TestFixtures.emptyEquipment.copy(weapon = fireWeapon))
      val armored = strongBattle.copy(
        monsterCurrentArmor = 5000L,
        monsterStats        = strongBattle.monsterStats.copy(armor = 5000L))
      for {
        // Броня 0 → весь урон в HP: 503 × 1.20 = 603.
        t1            <- makeState(hero, strongBattle)
        (s1, dao1, r1) = t1
        // Броски: удар героя (попал), прок огня (90 > 30 — не сработал, чтобы
        // горение не путало числа), удар моба, каст моба.
        _             <- TestRandom.feedInts(60, 90, 3, 90)
        _             <- TestRandom.feedLongs(100L) // spread = 100%
        _             <- s1.action(testUser, tap("Attack"), r1)
        b1            <- dao1.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

        // Брони с запасом → весь урон в броню: 503 × 0.90 = 452, в HP ничего.
        t2            <- makeState(hero, armored)
        (s2, dao2, r2) = t2
        _             <- TestRandom.feedInts(60, 90, 3, 90)
        _             <- TestRandom.feedLongs(100L)
        _             <- s2.action(testUser, tap("Attack"), r2)
        b2            <- dao2.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(strongBattle.monsterCurrentHp - b1.monsterCurrentHp == 603L) &&
              assertTrue(armored.monsterCurrentArmor - b2.monsterCurrentArmor == 452L) &&
              assertTrue(b2.monsterCurrentHp == armored.monsterCurrentHp)
    },

    test("огонь + молния в бою: по броне −40%+усиление, по HP 0%+усиление, 20% урона брони уходит в HP") {
      // Рубин и Топаз по 1 грейду → усиление 0.02·2 = +4 п.п.
      // Броня: (−20% огонь −20% молния) + 4% = ×0.64. HP: (+10% −10%) + 4% = ×1.04.
      // Урон до стихий: (str 1×3 + атака 500) × spread 100% = 503.
      val stormWeapon = Item(3L, "Грозовой меч", 1L, ItemRarity.Blue, ItemType.Weapon,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        sockets = List(Some(Gem(GemKind.Ruby, 1)), Some(Gem(GemKind.Topaz, 1))))
      val hero = strongHero.copy(
        fightStats = strongHero.fightStats.copy(atk = 500),
        equipment  = TestFixtures.emptyEquipment.copy(weapon = stormWeapon))
      val armored = strongBattle.copy(
        monsterCurrentArmor = 5000L,
        monsterStats        = strongBattle.monsterStats.copy(armor = 5000L))
      for {
        t             <- makeState(hero, armored)
        (state, dao, r) = t
        // Броски: удар героя, прок огня, прок молнии (оба 90 > 30 — не сработали),
        // удар моба, каст моба. Порядок проков — по Element.values: огонь, молния.
        _             <- TestRandom.feedInts(60, 90, 90, 3, 90)
        _             <- TestRandom.feedLongs(100L) // spread = 100%
        _             <- state.action(testUser, tap("Attack"), r)
        after         <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        armorLost      = armored.monsterCurrentArmor - after.monsterCurrentArmor
        hpLost         = armored.monsterCurrentHp - after.monsterCurrentHp
      } yield assertTrue(armorLost == 321L) &&              // 503 × 0.64
              assertTrue(hpLost == 64L) &&                  // 20% от 321 — особенность молнии
              assertTrue(hpLost == (armorLost * 0.20).toLong)
    },

    // ── Набор «Упырь» ─────────────────────────────────────────────────────────
    test("«Упырь» 4: удар, целиком поглощённый бронёй, крови не даёт") {
      val hero = ghoulHero(4).copy(fightStats = strongHero.fightStats.copy(atk = 500, hp = 100))
      // Броня моба с запасом — весь урон уходит в неё, до HP не доходит.
      val armored = strongBattle.copy(
        monsterCurrentArmor = 5000L,
        monsterStats        = strongBattle.monsterStats.copy(armor = 5000L))
      for {
        t             <- makeState(hero, armored)
        (state, dao, r) = t
        _             <- TestRandom.feedInts(60, 3, 90)
        _             <- TestRandom.feedLongs(100L)
        _             <- state.action(testUser, tap("Attack"), r)
        after         <- dao.getHeroByUserId(userId).map(_.get)
        battle        <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(battle.monsterCurrentArmor < 5000L) && // урон по броне прошёл
              assertTrue(battle.monsterCurrentHp == armored.monsterCurrentHp) && // в HP — нет
              assertTrue(after.fightStats.hp == 100L) // и лечения не было
    },

    test("«Упырь» 4: часть нанесённого урона возвращается герою в HP") {
      val hero = ghoulHero(4).copy(fightStats = strongHero.fightStats.copy(atk = 500, hp = 100))
      for {
        t             <- makeState(hero, strongBattle)
        (state, dao, r) = t
        _             <- TestRandom.feedInts(60, 3, 90) // удар героя, удар моба, каст моба
        _             <- TestRandom.feedLongs(100L)
        _             <- state.action(testUser, tap("Attack"), r)
        after         <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(after.fightStats.hp > 100L) // 2% от 503 ушли в лечение
    },

    test("«Упырь» 6: удар по HP с шансом пускает цели кровь") {
      val hero = ghoulHero(6).copy(fightStats = strongHero.fightStats.copy(atk = 500))
      for {
        t             <- makeState(hero, strongBattle)
        (state, dao, r) = t
        // Броски: удар героя, бросок кровотечения (10 ≤ 30 — сработал), удар моба, каст моба.
        _             <- TestRandom.feedInts(60, 10, 3, 90)
        _             <- TestRandom.feedLongs(100L)
        _             <- state.action(testUser, tap("Attack"), r)
        after         <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(after.effects.monsterBleed.exists(_.pct == 4))
    },

    test("«Упырь» 6: при неудачном броске кровотечения нет") {
      val hero = ghoulHero(6).copy(fightStats = strongHero.fightStats.copy(atk = 500))
      for {
        t             <- makeState(hero, strongBattle)
        (state, dao, r) = t
        _             <- TestRandom.feedInts(60, 90, 3, 90) // 90 > 30 — не сработало
        _             <- TestRandom.feedLongs(100L)
        _             <- state.action(testUser, tap("Attack"), r)
        after         <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(after.effects.monsterBleed.isEmpty)
    },

    test("«Упырь» 10: урон кровотечения врага лечит героя ровно на свою величину") {
      val hero    = ghoulHero(10).copy(fightStats = strongHero.fightStats.copy(atk = 1, hp = 100))
      // Моб уже истекает кровью: 10% от 9999 макс. HP = 999 урона за тик.
      val bleeding = strongBattle.copy(
        effects = strongBattle.effects.copy(monsterBleed = Some(pangea.model.battle.Bleed(10))))
      for {
        t             <- makeState(hero, bleeding)
        (state, dao, r) = t
        _             <- TestRandom.feedInts(60, 90, 3, 90)
        _             <- TestRandom.feedLongs(100L)
        _             <- state.action(testUser, tap("Attack"), r)
        after         <- dao.getHeroByUserId(userId).map(_.get)
        maxHp          = hero.effectiveMaxHp(0L)
      } yield assertTrue(after.fightStats.hp == (100L + 999L).min(maxHp))
    },

    test("«Упырь» 12: победа даёт пир — HP и броня восстановлены, сообщение показано") {
      val eq   = ghoulEquipment(12).copy(chestPlate = armorPiece(99L, armor = 100L))
      val hero = strongHero.copy(
        equipment  = eq,
        fightStats = strongHero.fightStats.copy(hp = 10, armor = 0))
      for {
        t             <- makeState(hero, weakBattle) // моб с 1 HP — умрёт от удара
        (state, dao, r) = t
        _             <- TestRandom.feedInts(60)
        _             <- TestRandom.feedLongs(100L)
        result        <- state.action(testUser, tap("Attack"), r)
        after         <- dao.getHeroByUserId(userId).map(_.get)
        screens       <- r.sentScreens
        maxHp          = hero.effectiveMaxHp(0L)
        // На 12 предметах работает и порог 4: удар (str 1×3 + атака 50 = 53)
        // сперва вернул 2% в HP, и только потом сработал пир.
        lifesteal      = 53L * 2L / 100L
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(after.fightStats.hp == 10L + lifesteal + maxHp * 25L / 100L) &&
              assertTrue(after.fightStats.armor == 100L * 20L / 100L) && // 20% от потолка брони
              assertTrue(screens.exists(_.text.contains("жуткий пир")))
    },

    test("без набора «Упырь» пира при победе нет") {
      val hero = strongHero.copy(fightStats = strongHero.fightStats.copy(hp = 10))
      for {
        t             <- makeState(hero, weakBattle)
        (state, dao, r) = t
        _             <- TestRandom.feedInts(60)
        _             <- TestRandom.feedLongs(100L)
        _             <- state.action(testUser, tap("Attack"), r)
        after         <- dao.getHeroByUserId(userId).map(_.get)
        screens       <- r.sentScreens
      } yield assertTrue(after.fightStats.hp == 10L) &&
              assertTrue(!screens.exists(_.text.contains("жуткий пир")))
    },

    // ── Набор «Дикое пламя» ───────────────────────────────────────────────────
    test("«Дикое пламя» 4: обе грани урона огнём выше на 10 п.п.") {
      val wf   = pangea.model.item.ItemSet.WildFlame
      // Рубин грейд 1 → усиление +2 п.п.: без набора HP ×1.12, с набором ×1.22.
      def heroWith(pieces: Int) = strongHero.copy(
        fightStats = strongHero.fightStats.copy(atk = 500),
        equipment  = setEquipment(wf, pieces, Some(flameWeapon())))
      for {
        t1            <- makeState(heroWith(4), strongBattle)
        (s1, dao1, r1) = t1
        _             <- TestRandom.feedInts(60, 90, 3, 90) // удар, прок огня (нет), моб, каст
        _             <- TestRandom.feedLongs(100L)
        _             <- s1.action(testUser, tap("Attack"), r1)
        b1            <- dao1.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        withSet        = strongBattle.monsterCurrentHp - b1.monsterCurrentHp

        t2            <- makeState(heroWith(2), strongBattle) // порог 4 не набран
        (s2, dao2, r2) = t2
        _             <- TestRandom.feedInts(60, 90, 3, 90)
        _             <- TestRandom.feedLongs(100L)
        _             <- s2.action(testUser, tap("Attack"), r2)
        b2            <- dao2.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        without        = strongBattle.monsterCurrentHp - b2.monsterCurrentHp
        // Урон до стихии: атака 500 уже поднята порогом 2 на +5% → 525,
        // плюс str 1×3 = 528. Грань по HP: огонь +10% и усиление грейда +2%,
        // с порогом 4 — ещё +10 п.п.
      } yield assertTrue(without == 591L) && // 528 × 1.12
              assertTrue(withSet == 644L)    // 528 × 1.22
    },

    test("«Дикое пламя» 6: горение растёт за раунд вдвое быстрее") {
      val wf = pangea.model.item.ItemSet.WildFlame
      def burnAfterRound(pieces: Int) = {
        val hero    = strongHero.copy(equipment = setEquipment(wf, pieces, Some(flameWeapon())))
        val burning = strongBattle.copy(
          effects = strongBattle.effects.copy(monsterBurn = Some(pangea.model.battle.Burn(2))))
        for {
          t             <- makeState(hero, burning)
          (state, dao, r) = t
          _             <- TestRandom.feedInts(60, 90, 3, 90)
          _             <- TestRandom.feedLongs(100L)
          _             <- state.action(testUser, tap("Attack"), r)
          after         <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        } yield after.effects.monsterBurn.map(_.pct)
      }
      for {
        fast <- burnAfterRound(6)
        slow <- burnAfterRound(4)
      } yield assertTrue(slow.contains(4)) && // 2 → +2
              assertTrue(fast.contains(6))    // 2 → +2 +2
    },

    test("«Дикое пламя» 10: шанс поджечь выше — бросок 55 прокает только с набором") {
      val wf = pangea.model.item.ItemSet.WildFlame
      def burnedWith(pieces: Int) = {
        val hero = strongHero.copy(equipment = setEquipment(wf, pieces, Some(flameWeapon())))
        for {
          t             <- makeState(hero, strongBattle)
          (state, dao, r) = t
          // 55 > 30 (базовый шанс), но ≤ 60 (30 + 30 от набора).
          _             <- TestRandom.feedInts(60, 55, 3, 90)
          _             <- TestRandom.feedLongs(100L)
          _             <- state.action(testUser, tap("Attack"), r)
          after         <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        } yield after.effects.monsterBurn.isDefined
      }
      for {
        withSet    <- burnedWith(10)
        withoutSet <- burnedWith(8)
      } yield assertTrue(withSet) && assertTrue(!withoutSet)
    },

    test("«Дикое пламя» 12: умение поджигает врага даже без прока стихии") {
      val wf     = pangea.model.item.ItemSet.WildFlame
      val weapon = flameWeapon().copy(details = ItemDetails.Weapon(pangea.model.skill.Skill.SweepingStrike))
      val hero   = strongHero.copy(
        fightStats = strongHero.fightStats.copy(atk = 100, energy = 1000),
        equipment  = setEquipment(wf, 12, Some(weapon)))
      val withSlot = strongBattle.copy(
        skillSlots = List(pangea.model.battle.SkillSlotState(weapon.id, pangea.model.skill.Skill.SweepingStrike)))
      for {
        t             <- makeState(hero, withSlot)
        (state, dao, r) = t
        _             <- TestRandom.feedInts(90, 60, 90, 3, 90) // прок огня не сработал
        _             <- TestRandom.feedLongs(100L, 100L)
        _             <- state.action(testUser, tap(s"Skill_${weapon.id}"), r)
        after         <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(after.effects.monsterBurn.isDefined)
    },

    test("«Дикое пламя» 12: горение режет защиту врага — умение бьёт сильнее") {
      val wf     = pangea.model.item.ItemSet.WildFlame
      val weapon = flameWeapon().copy(details = ItemDetails.Weapon(pangea.model.skill.Skill.SweepingStrike))
      // «Размашистый удар» режется защитой моба, поэтому срез защиты на нём виден.
      def hitWith(pieces: Int) = {
        val hero = strongHero.copy(
          fightStats = strongHero.fightStats.copy(atk = 100, energy = 1000),
          equipment  = setEquipment(wf, pieces, Some(weapon)))
        val burning = strongBattle.copy(
          monsterStats = strongBattle.monsterStats.copy(defence = 200L),
          effects      = strongBattle.effects.copy(monsterBurn = Some(pangea.model.battle.Burn(20))),
          skillSlots   = List(pangea.model.battle.SkillSlotState(weapon.id, pangea.model.skill.Skill.SweepingStrike)))
        for {
          t             <- makeState(hero, burning)
          (state, dao, r) = t
          _             <- TestRandom.feedInts(90, 60, 90, 3, 90)
          _             <- TestRandom.feedLongs(100L, 100L)
          _             <- state.action(testUser, tap(s"Skill_${weapon.id}"), r)
          after         <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        } yield burning.monsterCurrentHp - after.monsterCurrentHp
      }
      for {
        shredded <- hitWith(12) // горение на 20% срезает защиту на 20 п.п.
        plain    <- hitWith(10)
      } yield assertTrue(shredded > plain)
    },

    // ── Набор «Охотник» ───────────────────────────────────────────────────────
    test("«Охотник» 6: промах переигрывается при удачном броске") {
      // Уклонение моба 100% → герой промахивается всегда; повтор тоже промажет,
      // поэтому проверяем сам факт второй попытки по строке в логе.
      val hero  = hunterHero(6)
      val dodgy = strongBattle.copy(
        monsterStats = strongBattle.monsterStats.copy(evasion = 100000L))
      for {
        t             <- makeState(hero, dodgy)
        (state, _, r)  = t
        // Броски: удар героя (промах), бросок повтора (10 ≤ 25 — сработал),
        // удар героя повторно (промах), удар моба, каст моба.
        _             <- TestRandom.feedInts(1, 10, 1, 3, 90)
        _             <- state.action(testUser, tap("Attack"), r)
        screens       <- r.sentScreens
        text           = screens.map(_.text).mkString("\n")
      } yield assertTrue(text.contains("Промах не в счёт")) &&
              assertTrue(text.split("промахивается").length - 1 >= 1)
    },

    test("«Охотник» 6: повтор не больше одного за раунд") {
      val hero  = hunterHero(6)
      val dodgy = strongBattle.copy(
        monsterStats = strongBattle.monsterStats.copy(evasion = 100000L))
      for {
        t             <- makeState(hero, dodgy)
        (state, _, r)  = t
        // Броски: промах, повтор (10 ≤ 25), снова промах, затем удар моба и его
        // каст (100 — не кастует). Четвёртое число намеренно 10: если бы после
        // повтора делался ещё один бросок, оно снова дало бы повтор и лог вырос бы.
        _             <- TestRandom.feedInts(1, 10, 1, 10, 100)
        _             <- state.action(testUser, tap("Attack"), r)
        screens       <- r.sentScreens
        text           = screens.map(_.text).mkString("\n")
      } yield assertTrue(text.split("Промах не в счёт").length - 1 == 1)
    },

    test("«Охотник» 6: при неудачном броске повтора нет") {
      val hero  = hunterHero(6)
      val dodgy = strongBattle.copy(
        monsterStats = strongBattle.monsterStats.copy(evasion = 100000L))
      for {
        t             <- makeState(hero, dodgy)
        (state, _, r)  = t
        _             <- TestRandom.feedInts(1, 90, 3, 90) // 90 > 25 — повтора нет
        _             <- state.action(testUser, tap("Attack"), r)
        screens       <- r.sentScreens
      } yield assertTrue(!screens.map(_.text).mkString("\n").contains("Промах не в счёт"))
    },

    test("«Охотник» 10: первая вредящая способность моба гасится, вторая уже проходит") {
      val hero = hunterHero(10).copy(fightStats = strongHero.fightStats.copy(hp = 500, armor = 0))
      for {
        t             <- makeState(hero, strongBattle)
        (state, dao, r) = t
        // Броски раунда: удар героя, удар моба, каст моба (1 ≤ шанс — кастует), выбор скилла.
        _             <- TestRandom.feedInts(60, 3, 1, 0)
        _             <- TestRandom.feedLongs(100L)
        _             <- state.action(testUser, tap("Attack"), r)
        afterFirst    <- dao.getHeroByUserId(userId).map(_.get)
        battle1       <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        screens       <- r.sentScreens
      } yield assertTrue(battle1.effects.cancelSpent) && // отмена израсходована
              assertTrue(afterFirst.fightStats.hp == 500L) && // урон не прошёл
              assertTrue(screens.map(_.text).mkString("\n").contains("срываете его приём"))
    },

    test("«Охотник» 12: первое умение бьёт вдвое, второе — обычно") {
      val weapon = Item(5L, "Меч", 1L, ItemRarity.Blue, ItemType.Weapon,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        details = ItemDetails.Weapon(pangea.model.skill.Skill.SweepingStrike),
        set = Some(pangea.model.item.ItemSet.Hunter))
      val hero = hunterHero(12).copy(
        fightStats = strongHero.fightStats.copy(atk = 100, energy = 1000),
        equipment  = hunterEquipment(12).copy(weapon = weapon))
      // Готовность умения живёт в самом бою (skillSlots), а не выводится из
      // экипировки, — иначе слот считается незарегистрированным и ход не тратится.
      val withSlot = strongBattle.copy(
        skillSlots = List(pangea.model.battle.SkillSlotState(weapon.id, pangea.model.skill.Skill.SweepingStrike)))
      // Контроль: тот же герой, но 10 предметов — порог 12 не набран, всё
      // остальное (в т.ч. пороги 2/4/6/10) совпадает, значит разница только в удвоении.
      val heroNoDouble = hunterHero(10).copy(
        fightStats = strongHero.fightStats.copy(atk = 100, energy = 1000),
        equipment  = hunterEquipment(10).copy(weapon = weapon))
      for {
        t             <- makeState(hero, withSlot)
        (state, dao, r) = t
        // Два разброса: один на урон умения, второй на обычную атаку, которая
        // идёт следом в том же ходу. Иначе второй бросок был бы случайным.
        _             <- TestRandom.feedInts(60, 3, 90)
        _             <- TestRandom.feedLongs(100L, 100L)
        _             <- state.action(testUser, tap(s"Skill_${weapon.id}"), r)
        battle1       <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        doubled        = withSlot.monsterCurrentHp - battle1.monsterCurrentHp

        t2            <- makeState(heroNoDouble, withSlot)
        (state2, dao2, r2) = t2
        _             <- TestRandom.feedInts(60, 3, 90)
        _             <- TestRandom.feedLongs(100L, 100L)
        _             <- state2.action(testUser, tap(s"Skill_${weapon.id}"), r2)
        battle2       <- dao2.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        plain          = withSlot.monsterCurrentHp - battle2.monsterCurrentHp

        // За умением в том же ходу идёт обычная атака, поэтому в суммах сидит и
        // она. Меряем её отдельно на тех же бросках и вычитаем — остаётся чистый
        // урон умения, который и должен удвоиться.
        t3            <- makeState(heroNoDouble, withSlot)
        (state3, dao3, r3) = t3
        _             <- TestRandom.feedInts(60, 3, 90)
        _             <- TestRandom.feedLongs(100L)
        _             <- state3.action(testUser, tap("Attack"), r3)
        battle3       <- dao3.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        attackOnly     = withSlot.monsterCurrentHp - battle3.monsterCurrentHp
      } yield assertTrue(battle1.effects.doubleSpent) &&
              assertTrue(!battle2.effects.doubleSpent) &&
              assertTrue(plain - attackOnly > 0L) &&
              assertTrue(doubled - attackOnly == (plain - attackOnly) * 2L)
    },

    test("UseBelt зелье атаки → добавлен временный баф атаки на 5 ходов") {
      val hero = strongHero.copy(equipment = TestFixtures.emptyEquipment.copy(belt = belt(PotionKind.Attack, 1)))
      for {
        triple                     <- makeState(hero, strongBattle)
        (state, heroDao, renderer)  = triple
        _                          <- state.action(testUser, tap("UseBelt"), renderer)
        after                      <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(after.heroBattleState.buffs.exists(b => b.atk >= 1L && b.turnsLeft.contains(5)))
    },

    test("UseBelt с пустым поясом → сообщение, заряды не тратятся") {
      val hero = strongHero.copy(equipment = TestFixtures.emptyEquipment.copy(belt = belt(PotionKind.Healing, 0)))
      for {
        triple                     <- makeState(hero, strongBattle)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("UseBelt"), renderer)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("закончились")))
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
      val highAtkBattle = SoloPveBattle(
        monsterLvl          = 1L,
        monsterRace         = pangea.model.monster.Race.Human.entryName,
        monsterRarity       = pangea.model.monster.Rarity.Common.entryName,
        monsterStats        = FightStats(atk=10000, hp=9999, armor=0, defence=0,
                                          evasion=0, accuracy=9999, energy=0),
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
                        attack = 0, accuracy = 0, energy = 0, armor = 30, defence = 0, evasion = 0)
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
      val buff        = Buff(atk = 0L, armor = 0L, defence = 0L, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(3))
      val buffedBattle = strongBattle.copy(heroBattleState = HeroBattleState(List(buff)))
      for {
        triple               <- makeState(strongHero, buffedBattle)
        (state, heroDao, renderer) = triple
        _                    <- state.action(testUser, tap("Attack"), renderer)
        remaining            <- heroDao.readActiveBattle(userId)
      } yield assertTrue(
        remaining.flatMap(_.as[SoloPveBattle].toOption)
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
      val bigArmorBuff  = Buff(atk = 0L, armor = 9999L, defence = 0L, dodgePct = 0L, defencePct = 0L, turnsLeft = None)
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
      val bigMonster = SoloPveBattle(
        monsterLvl          = 1L,
        monsterRace         = Race.Human.entryName,
        monsterRarity       = Rarity.Common.entryName,
        monsterStats        = FightStats(atk = 1, hp = 100000, armor = 0, defence = 0,
                                         evasion = 0, accuracy = 1, energy = 0),
        monsterCurrentHp    = 100000L,
        monsterCurrentArmor = 0L
      )
      def heroOf(race: Race) = TestFixtures.hero(userId).copy(
        race       = race,
        baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 100, agi = 0),
        fightStats = FightStats(atk = 0, hp = 10000, armor = 0, defence = 0,
                                evasion = 9999, accuracy = 9999, energy = 0)
      )
      def monsterHpAfterAttack(race: Race) =
        for {
          triple              <- makeState(heroOf(race), bigMonster)
          (state, heroDao, r)  = triple
          _                   <- TestRandom.feedInts(49)
          _                   <- TestRandom.feedLongs(20L)
          _                   <- state.action(testUser, tap("Attack"), r)
          hp                  <- heroDao.readActiveBattle(userId)
                                   .map(_.flatMap(_.as[SoloPveBattle].toOption).map(_.monsterCurrentHp).getOrElse(-1L))
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
