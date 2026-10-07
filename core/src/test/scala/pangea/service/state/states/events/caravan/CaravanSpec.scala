package pangea.service.state.states.events.caravan

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.FlaskGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.caravan.{CaravanGenerator, CaravanRates, CaravanScene}
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, DivineKind, FlaskKind, Item, ItemDetails, ItemType, PassiveKind, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity => MobRarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Караван: три сцены подхода, дурман и страх до боя, торговля с рук, кража в
  * дыму и бой волнами с башнями в хвосте строя. */
object CaravanSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def content = ZIO.attempt(SceneContent.load())

  private def passive(kind: PassiveKind, itemType: ItemType, id: Long): Item =
    Item(id, "Предмет", 1L, ItemRarity.Gray, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Passive(kind))

  private def hero(
    dungeonLevel: Int         = 10,
    silver:       Long        = 0L,
    passives:     List[(PassiveKind, ItemType)] = Nil,
    flask:        Option[Item] = None,
    blade:        Option[Item] = None
  ): Hero = {
    val h  = TestFixtures.hero(userId, dungeonLevel = dungeonLevel)
    val eq = passives.zipWithIndex.foldLeft(TestFixtures.emptyEquipment) {
      case (acc, ((kind, ItemType.Helmet), i)) => acc.copy(helmet = passive(kind, ItemType.Helmet, 100L + i))
      case (acc, ((kind, t), i))               => acc.copy(gloves = passive(kind, t, 100L + i))
    }
    h.copy(lvl = 10L, silver = silver, equipment = eq.copy(
      flask            = flask.getOrElse(Item.NoItem),
      additionalWeapon = blade.getOrElse(Item.NoItem)))
  }

  private def caravan(h: Hero = hero(), items: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- content
    } yield (CaravanState(dao, inv, TestItemRepository.make, c), dao, inv, r)

  /** Караван орков: десять охранников, одна башня, три вещи в поклаже. */
  private def scene(
    guards:   Int     = 10,
    towers:   Int     = 1,
    stage:    Int     = CaravanRates.StageSpotted,
    weakened: Boolean = false,
    smoke:    Boolean = false,
    scared:   Int     = 0,
    goods:    Int     = CaravanRates.Goods,
    prices:   List[Long] = List(100L, 200L, 300L)
  ): CaravanScene =
    CaravanScene(
      race     = Race.Orc.entryName,
      guards   = guards,
      towers   = towers,
      stage    = stage,
      weakened = weakened,
      smoke    = smoke,
      scared   = scared,
      goods    = (1 to goods).toList.map(i => good(i.toLong)),
      prices   = prices.take(goods))

  private def good(id: Long): Item =
    Item(id, s"Товар $id", 10L, ItemRarity.Blue, ItemType.Helmet,
      attack = 1, accuracy = 1, energy = 1, armor = 1, defence = 1, evasion = 1,
      details = ItemDetails.Plain)

  private def put(dao: TestHeroDao, s: CaravanScene): Task[Unit] = dao.writeSceneData(userId, s.asJson)

  private def sceneOf(dao: TestHeroDao): Task[Option[CaravanScene]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[CaravanScene].toOption))

  private def lootOf(dao: TestHeroDao): Task[Option[LootData]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))

  private def battleOf(dao: TestHeroDao): Task[Option[SoloPveBattle]] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private def labels(r: TestRenderer): Task[List[String]] = r.sentScreens.map(_.last.choices.map(_.label))

  override def spec = suite("Караван")(

    test("в пуле событий 1%, забранный у серебряной жилы; билеты дальше по списку не сдвинулись") {
      val ev = StateType.events
      assertTrue(ev.count(_ == StateType.Caravan) == 1) &&
      assertTrue(ev.count(_ == StateType.SilverVein) == 9 && ev.size == 100) &&
      assertTrue(ev(37) == StateType.MonsterCave && ev(38) == StateType.FlowerMeadow) &&
      assertTrue(ev(88) == StateType.Caravan) &&
      assertTrue(ev(89) == StateType.TreasureMobs && ev(99) == StateType.ElementalLair)
    },

    test("караван: 10–20 охранников, 1–2 башни, раса — из основных") {
      val rolled = (1L to 300L).toList.map(seed => CaravanGenerator.generate(Rng(seed))._1)
      val guards = rolled.map(_.guards)
      val towers = rolled.map(_.towers)
      assertTrue(guards.forall(g => g >= CaravanRates.MinGuards && g <= CaravanRates.MaxGuards)) &&
      assertTrue(guards.distinct.size >= 8 && towers.distinct.sorted == List(1, 2)) &&
      assertTrue(rolled.map(_.race).distinct.toSet.subsetOf(Race.mortals.map(_.entryName).toSet)) &&
      assertTrue(rolled.forall(s => s.stage == CaravanRates.StageSpotted && !s.weakened && s.scared == 0))
    },

    test("охрана: 50% второй грейд, 40% третий, 9% четвёртый, 1% именной") {
      val pool = CaravanRates.GuardPool
      assertTrue(pool.size == 100) &&
      assertTrue(pool.count(_ == MobRarity.Uncommon) == 50 && pool.count(_ == MobRarity.Rare) == 40) &&
      assertTrue(pool.count(_ == MobRarity.Mythical) == 9 && pool.count(_ == MobRarity.Legendary) == 1) &&
      assertTrue((1L to 400L).map(s => CaravanGenerator.guardRarity(Rng(s))._1).distinct.size >= 3)
    },

    test("товары: 35% синих, 32% фиолетовых, 32% пурпурных, 1% легендарных") {
      val pool = CaravanRates.GoodsPool
      assertTrue(pool.size == 100) &&
      assertTrue(pool.count(_ == ItemRarity.Blue) == 35 && pool.count(_ == ItemRarity.Purple) == 32) &&
      assertTrue(pool.count(_ == ItemRarity.Violet) == 32 && pool.count(_ == ItemRarity.Orange) == 1)
    },

    test("цена: (уровень+20)×4 на множитель редкости с разбросом в пятую часть") {
      def range(rarity: ItemRarity): (Long, Long) = {
        val prices = (1L to 200L).toList.map(s => CaravanGenerator.price(10L, rarity, Rng(s))._1)
        (prices.min, prices.max)
      }
      val base = (10L + 20L) * 4L // 120 серебра за единицу множителя
      def ok(rarity: ItemRarity, factor: Double): Boolean = {
        val (lo, hi) = range(rarity)
        val mid      = base * factor
        lo >= (mid * 0.8).toLong - 1 && hi <= (mid * 1.2).toLong + 1 && lo < mid && hi > mid
      }
      assertTrue(ok(ItemRarity.Blue, 6.0) && ok(ItemRarity.Purple, 8.0)) &&
      assertTrue(ok(ItemRarity.Violet, 12.0) && ok(ItemRarity.Orange, 25.0)) &&
      // легендарная вещь 10-го уровня идёт около трёх тысяч серебра — вчетверо
      // дороже синей даже на худшем броске
      assertTrue(range(ItemRarity.Orange)._1 > range(ItemRarity.Blue)._2 * 2)
    },

    test("первая сцена: можно подобраться и поторговать, напугать и красться — нельзя") {
      for {
        t <- caravan()
        (state, dao, _, r) = t
        _   <- put(dao, scene())
        _   <- state.enter(testUser, r)
        btn <- labels(r)
        txt <- texts(r)
      } yield assertTrue(txt.contains("караван орков")) &&
              assertTrue(btn.exists(_.contains("Действовать тихо")) && btn.exists(_.contains("Поторговать"))) &&
              assertTrue(!btn.exists(_.contains("Напугать")) && !btn.exists(_.contains("скрытно"))) &&
              assertTrue(btn.exists(_.contains("Напасть")) && btn.exists(_.contains("Использовать предмет")))
    },

    test("шаг за шагом: подобрался → выждал; на последней сцене ждать больше нечего") {
      for {
        t <- caravan()
        (state, dao, _, r) = t
        _      <- put(dao, scene())
        _      <- state.action(testUser, tap("CaravanSneakUp"), r)
        close  <- sceneOf(dao)
        waited <- labels(r)
        _      <- state.action(testUser, tap("CaravanWait"), r)
        moment <- sceneOf(dao)
        last   <- labels(r)
      } yield assertTrue(close.exists(_.stage == CaravanRates.StageClose)) &&
              assertTrue(waited.exists(_.contains("Выжидать"))) &&
              assertTrue(moment.exists(_.stage == CaravanRates.StageMoment)) &&
              assertTrue(!last.exists(_.contains("Выжидать")) && !last.exists(_.contains("Действовать тихо")))
    },

    test("на последней сцене «Напугать» и «Скрытно» открывают только пассивки") {
      val plain     = hero()
      val terrible  = hero(passives = List(PassiveKind.Terrifying -> ItemType.Gloves))
      val stealthy  = hero(passives = List(PassiveKind.Stealthy -> ItemType.Helmet))
      def open(h: Hero) =
        for {
          t <- caravan(h)
          (state, dao, _, r) = t
          _   <- put(dao, scene(stage = CaravanRates.StageMoment))
          _   <- state.enter(testUser, r)
          btn <- labels(r)
        } yield btn
      for {
        none  <- open(plain)
        scare <- open(terrible)
        sneak <- open(stealthy)
      } yield assertTrue(!none.exists(_.contains("Напугать")) && !none.exists(_.contains("скрытно"))) &&
              assertTrue(scare.exists(_.contains("Напугать")) && !scare.exists(_.contains("скрытно"))) &&
              assertTrue(sneak.exists(_.contains("скрытно")) && !sneak.exists(_.contains("Напугать")))
    },

    test("«Ужасающий» уводит четверть охраны, но пугать дважды одним и тем же нельзя") {
      val h = hero(passives = List(PassiveKind.Terrifying -> ItemType.Gloves))
      for {
        t <- caravan(h)
        (state, dao, _, r) = t
        _      <- put(dao, scene(guards = 20, stage = CaravanRates.StageMoment))
        _      <- state.action(testUser, tap("CaravanScare"), r)
        once   <- sceneOf(dao)
        _      <- state.action(testUser, tap("CaravanScare"), r)
        twice  <- sceneOf(dao)
        _      <- state.action(testUser, tap("CaravanScare"), r)
        thrice <- sceneOf(dao)
        said   <- texts(r)
      } yield assertTrue(once.exists(s => s.scared == 1 && s.fighting == 15)) &&
              assertTrue(twice.exists(s => s.scared == 2 && s.fighting == 10)) &&
              assertTrue(thrice.exists(_.scared == CaravanState.MaxScares)) &&
              assertTrue(said.contains("Пугать больше некого"))
    },

    test("божественное оружие пугает так же и тратит один заряд; вместе с рыком — половина каравана") {
      val blade = DivineKind.item(DivineKind.DarkLordSword, 10L, ItemRarity.Orange).copy(id = 7L)
      val h     = hero(passives = List(PassiveKind.Terrifying -> ItemType.Gloves), blade = Some(blade))
      for {
        t <- caravan(h)
        (state, dao, _, r) = t
        _      <- put(dao, scene(guards = 20, stage = CaravanRates.StageMoment))
        _      <- state.action(testUser, tap("CaravanSupply"), r)
        list   <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("CaravanUse_7"), r)
        after  <- sceneOf(dao)
        charge <- dao.getHeroByUserId(userId).map(_.get.equipment.additionalWeapon.divine.get.charges)
        _      <- state.action(testUser, tap("CaravanScare"), r)
        both   <- sceneOf(dao)
      } yield assertTrue(list.choices.map(_.id).contains("CaravanUse_7")) &&
              assertTrue(after.exists(s => s.scared == 1 && s.fighting == 15)) &&
              assertTrue(charge == DivineKind.chargesFor(ItemRarity.Orange) - 1) &&
              assertTrue(both.exists(s => s.scared == CaravanState.MaxScares && s.fighting == 10))
    },

    test("сонный дурман: охрана вялая, бой идёт двумя волнами, башни в нём не участвуют") {
      val dope = BrewKind.item(BrewKind.SleepingDope).copy(id = 7L)
      for {
        t <- caravan(items = List(dope))
        (state, dao, inv, r) = t
        _     <- put(dao, scene(guards = 11, towers = 2))
        _     <- state.action(testUser, tap("CaravanSupply"), r)
        _     <- state.action(testUser, tap("CaravanUse_7"), r)
        after <- sceneOf(dao)
        said  <- texts(r)
      } yield assertTrue(after.exists(s => s.weakened && !s.smoke)) &&
              assertTrue(after.exists(s => s.waves == 2 && s.towersInFight == 0)) &&
              // одиннадцать охранников делятся поровну, лишний идёт в первую волну
              assertTrue(after.exists(s => s.waveSize(1) == 6 && s.waveSize(2) == 5)) &&
              assertTrue(said.contains("просели на 20%") && inv.snapshot.isEmpty)
    },

    test("дымная фляга одурманивает и открывает дорогу к поклаже, тратя заряд") {
      val flask = FlaskGenerator.item(FlaskKind.Smoke, ItemRarity.Blue).copy(id = 5L)
      for {
        t <- caravan(hero(flask = Some(flask)))
        (state, dao, _, r) = t
        _     <- put(dao, scene())
        _     <- state.action(testUser, tap("CaravanSupply"), r)
        _     <- state.action(testUser, tap("CaravanUse_5"), r)
        after <- sceneOf(dao)
        left  <- dao.getHeroByUserId(userId).map(_.get.equipment.flask.details)
      } yield assertTrue(after.exists(s => s.weakened && s.smoke)) &&
              assertTrue(left match {
                case ItemDetails.Flask(_, charges, max) => charges == max - 1
                case _                                  => false
              })
    },

    test("в бою охрана бьёт на пятую часть слабее, если её одурманили") {
      def atk(weakened: Boolean) =
        for {
          t <- caravan()
          (state, dao, _, r) = t
          _      <- put(dao, scene(guards = 1, towers = 0, weakened = weakened,
                      stage = CaravanRates.StageMoment))
          _      <- state.action(testUser, tap("CaravanAttack"), r)
          battle <- battleOf(dao)
        } yield battle.get
      for {
        plain <- atk(weakened = false)
        doped <- atk(weakened = true)
        base   = MonsterGenerator.generateOfRaceAndRarity(10, Race.Orc, doped.rarity).fightStats
      } yield assertTrue(plain.monsterStats.atk > 0L) &&
              assertTrue(doped.monsterStats.atk == (base.atk * 80L / 100L).max(1L)) &&
              assertTrue(doped.monsterStats.energy == base.energy * 80L / 100L)
    },

    test("нападение: одна волна, башни поодаль, а за поклажей герой возвращается к обозу") {
      for {
        t <- caravan()
        (state, dao, _, r) = t
        _      <- put(dao, scene(guards = 3, towers = 2, stage = CaravanRates.StageMoment))
        out    <- state.action(testUser, tap("CaravanAttack"), r)
        battle <- battleOf(dao)
        loot   <- lootOf(dao)
        said   <- texts(r)
        places  = battle.get.group.places
        back    = loot.get.eventData.flatMap(_.as[CaravanScene].toOption).get
      } yield assertTrue(out == StateType.Battle && said.contains("их 3")) &&
              assertTrue(battle.exists(b => b.group.others.size == 4 && b.group.activePos == 1)) &&
              // охрана идёт подряд от героя, а башни стоят поодаль — на 14 и 15
              assertTrue(places == List(2, 3, CaravanRates.TowerPlace, CaravanRates.TowerPlace + 1)) &&
              assertTrue(CaravanRates.TowerPlace == 14) &&
              // добычу с мобов соберёт экран добычи, а поклажу герой заберёт,
              // вернувшись к обозу
              assertTrue(loot.exists(l => l.items.isEmpty && l.returnState.contains(StateType.Caravan))) &&
              assertTrue(back.spoils && back.goods.size == 3)
    },

    test("одурманенный караван: первая волна возвращает к каравану, вторая отдаёт поклажу") {
      for {
        t <- caravan()
        (state, dao, _, r) = t
        _      <- put(dao, scene(guards = 4, towers = 2, weakened = true, stage = CaravanRates.StageMoment))
        _      <- state.action(testUser, tap("CaravanAttack"), r)
        first  <- battleOf(dao)
        wave1  <- lootOf(dao)
        said1  <- texts(r)
        back    = wave1.get.eventData.flatMap(_.as[CaravanScene].toOption).get
        _      <- put(dao, back)
        _      <- state.action(testUser, tap("CaravanAttack"), r)
        second <- battleOf(dao)
        wave2  <- lootOf(dao)
      } yield assertTrue(first.exists(_.group.others.size == 1) && second.exists(_.group.others.size == 1)) &&
              // башни одурманенного каравана в бой не идут: строй только из охраны
              assertTrue(first.exists(b => b.group.places == List(2) && !b.group.others.exists(
                sl => sl.race == Race.Construct.entryName))) &&
              assertTrue(said1.contains("Волна 1 из 2")) &&
              assertTrue(back.wave == 1 && back.stage == CaravanRates.StageMoment) &&
              assertTrue(wave1.exists(l => l.items.isEmpty && l.returnState.contains(StateType.Caravan))) &&
              // после второй волны обоз стоит без охраны, поклажа ещё в повозках
              assertTrue(wave2.exists(l => l.items.isEmpty && l.returnState.contains(StateType.Caravan))) &&
              assertTrue(wave2.get.eventData.flatMap(_.as[CaravanScene].toOption).exists(_.spoils))
    },

    test("перебив охрану, герой разбирает повозки — и поклажа уходит на экран добычи") {
      for {
        t <- caravan()
        (state, dao, _, r) = t
        // бой позади: экран добычи вернул героя к обозу без охраны
        _     <- put(dao, scene(stage = CaravanRates.StageMoment).copy(wave = 1, spoils = true))
        _     <- state.enter(testUser, r)
        offer <- r.sentScreens.map(_.last)
        out   <- state.action(testUser, tap("CaravanSpoils"), r)
        loot  <- lootOf(dao)
        said  <- texts(r)
      } yield assertTrue(offer.choices.map(_.id) == List("CaravanSpoils")) &&
              assertTrue(offer.text.contains("обоз стоит без хозяев")) &&
              assertTrue(out == StateType.Loot && said.contains("обираете повозки")) &&
              assertTrue(loot.exists(l => l.items.size == 3 && l.returnState.isEmpty))
    },

    test("прилавок каравана сравнивает товар с надетым — как у Ришелье") {
      for {
        // товар каравана — шлемы, и шлем у героя уже надет: есть с чем сравнить
        t <- caravan(hero(silver = 1000L, passives = List(PassiveKind.Stealthy -> ItemType.Helmet)))
        (state, dao, _, r) = t
        _      <- put(dao, scene(prices = List(100L, 200L, 300L)))
        _      <- state.action(testUser, tap("CaravanTrade"), r)
        shelf  <- r.sentScreens.map(_.last)
        // а без шлема сравнивать не с чем — и лишнего в тексте нет
        b <- caravan(hero(silver = 1000L))
        (bare, bareDao, _, bareR) = b
        _      <- put(bareDao, scene(prices = List(100L, 200L, 300L)))
        _      <- bare.action(testUser, tap("CaravanTrade"), bareR)
        plain  <- bareR.sentScreens.map(_.last)
      } yield assertTrue(shelf.text.contains(Item.ComparisonSeparator) && shelf.text.contains("Надето")) &&
              // характеристики самого товара при этом никуда не делись
              assertTrue(shelf.text.contains("Цена: 100") && shelf.text.contains("⚔ +1")) &&
              assertTrue(!plain.text.contains(Item.ComparisonSeparator) && !plain.text.contains("Надето"))
    },

    test("торговля идёт только с серебра на руках: банк каравану не указ") {
      for {
        t <- caravan(hero(silver = 150L))
        (state, dao, inv, r) = t
        _      <- put(dao, scene(prices = List(100L, 200L, 300L)))
        _      <- state.action(testUser, tap("CaravanTrade"), r)
        shelf  <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("CaravanBuy_1"), r)  // 200 серебра — не по карману
        poor   <- texts(r)
        purse1 <- dao.getHeroByUserId(userId).map(_.get.silver)
        _      <- state.action(testUser, tap("CaravanBuy_0"), r)  // 100 серебра — берём
        purse2 <- dao.getHeroByUserId(userId).map(_.get.silver)
        after  <- sceneOf(dao)
      } yield assertTrue(shelf.text.contains("При вас: 150") && shelf.choices.count(_.id.startsWith("CaravanBuy_")) == 3) &&
              assertTrue(poor.contains("Столько у тебя нет") && purse1 == 150L) &&
              assertTrue(purse2 == 50L && inv.snapshot.size == 1) &&
              assertTrue(after.exists(s => s.goods.size == 2 && s.prices == List(200L, 300L)))
    },

    test("кража: без дыма караван замечает героя и сразу лезет драться") {
      val h = hero(passives = List(PassiveKind.Stealthy -> ItemType.Helmet))
      for {
        t <- caravan(h)
        (state, dao, _, r) = t
        _      <- put(dao, scene(stage = CaravanRates.StageMoment))
        out    <- state.action(testUser, tap("CaravanSneak"), r)
        said   <- texts(r)
        battle <- battleOf(dao)
      } yield assertTrue(out == StateType.Battle && battle.isDefined) &&
              assertTrue(said.contains("Без дыма к повозкам не подобраться"))
    },

    test("кража в дыму: половина на половину дойти и столько же уйти") {
      val h = hero(passives = List(PassiveKind.Stealthy -> ItemType.Helmet))
      def sneak(rolls: Int*) =
        for {
          t <- caravan(h)
          (state, dao, _, r) = t
          _    <- put(dao, scene(smoke = true, weakened = true, stage = CaravanRates.StageMoment))
          _    <- TestRandom.feedInts(rolls: _*)
          out  <- state.action(testUser, tap("CaravanSneak"), r)
          loot <- lootOf(dao)
          said <- texts(r)
        } yield (out, loot, said)
      for {
        clean   <- sneak(10, 10)   // подобрался и ушёл
        spotted <- sneak(10, 90)   // взял поклажу, но заметили
        caught  <- sneak(90)       // не дошёл
      } yield assertTrue(clean._1 == StateType.Loot && clean._2.exists(_.items.size == 3)) &&
              assertTrue(clean._3.contains("не досчитавшись поклажи")) &&
              assertTrue(spotted._1 == StateType.Battle && spotted._3.contains("Уйти тихо не вышло")) &&
              assertTrue(caught._1 == StateType.Battle && caught._3.contains("хрустит щебень"))
    },

    test("уйти можно на любой сцене: сцена стирается, герой возвращается в лабиринт") {
      for {
        t <- caravan()
        (state, dao, _, r) = t
        _    <- put(dao, scene(stage = CaravanRates.StageMoment))
        out  <- state.action(testUser, tap("CaravanLeave"), r)
        left <- sceneOf(dao)
        said <- texts(r)
      } yield assertTrue(out == StateType.Dungeon && left.isEmpty) &&
              assertTrue(said.contains("Вы пропускаете караван"))
    },

    test("уход от каравана по объявлению валит задание: оно пропадает с доски") {
      import pangea.model.quest.{BoardData, BoardKind, BoardSlot}
      /** Объявление на столе и сцена каравана; `done` — охрану уже перебили. */
      def go(action: String, done: Boolean) =
        for {
          t <- caravan()
          (state, dao, _, r) = t
          _    <- dao.writeQuestData(userId,
                    BoardData(slots = List(BoardSlot(BoardKind.CaravanRout, taken = true, done = done))).asJson)
          _    <- put(dao, scene(stage = CaravanRates.StageMoment, smoke = true, weakened = true))
          // оба броска кражи удачные: подкрался и ушёл незамеченным
          _    <- TestRandom.feedInts(1, 1)
          _    <- state.action(testUser, tap(action), r)
          said <- texts(r)
          data <- dao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).get)
        } yield (said, data)
      for {
        left   <- go("CaravanLeave", done = false)
        // тихая кража тоже уводит караван: охрана цела, задание не сделано
        sneaked <- go("CaravanSneak", done = false)
        // а разгромленную охрану уход не отменяет — за платой идут к доске
        after  <- go("CaravanLeave", done = true)
      } yield assertTrue(left._2.slots.isEmpty &&
                         left._1.contains("не уверен, что смогу найти их во второй раз")) &&
              assertTrue(sneaked._2.slots.isEmpty) &&
              assertTrue(after._2.slots.size == 1 && after._2.slots.head.done) &&
              assertTrue(!after._1.contains("задание провалено"))
    },

    test("караван переживает уход в «Персонаж»: тот же состав, тот же товар") {
      for {
        t <- caravan()
        (state, dao, _, r) = t
        s0    = scene(guards = 17, towers = 2, stage = CaravanRates.StageClose, weakened = true,
                  smoke = true, scared = 1)
        _    <- put(dao, s0)
        out  <- state.action(testUser, tap("OpenCharacter"), r)
        kept <- sceneOf(dao)
      } yield assertTrue(out == StateType.HeroStats) &&
              assertTrue(kept.contains(s0))
    }
  )
}
