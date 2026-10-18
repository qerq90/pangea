package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Grip, GripRates, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Лук и арбалет: стрелковое двуручное. Бьёт на три места в обе стороны, а
  * вблизи — по тому, кто напротив, и по соседям — на пятую часть слабее. */
object BowSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  private def weapon(grip: Grip, skill: Option[Skill] = None): Item =
    Item(101L, "Оружие", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(grip),
      details = skill.fold[ItemDetails](ItemDetails.Plain)(ItemDetails.Weapon(_)))

  /** Герой с оружием в правой руке и Тараном на кирасе, с запасом энергии. */
  private def hero(right: Item): Hero = {
    val base  = TestFixtures.hero(userId)
    val chest = Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(Skill.Ram))
    base.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 100L, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = base.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = right, chestPlate = chest))
  }

  private def monster: Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  /** Строй из `n` мобов: первый в паре с героем на первом месте, остальные за ним. */
  private def group(h: Hero, n: Int): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List.fill(n)(monster), h, Nil)
    b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0)))
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

  /** Куда можно ударить: места на кнопках экрана выбора цели. */
  private def targets(r: TestRenderer) =
    r.sentScreens.map(_.last.choices.filterNot(_.id == "CancelTarget").flatMap(_.data.get("target")).map(_.toInt))

  /** Сколько HP снял один удар по месту `pos`: герой попал, разброс ровно 100%. */
  private def strikeLoss(h: Hero, pos: Int) =
    for {
      t <- makeState(h, group(h, 5))
      (state, dao, r) = t
      _     <- TestRandom.feedInts(60, 99, 99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
      _     <- state.action(testUser, aimed("Attack", pos), r)
      after <- battleOf(dao)
    } yield after.monsterAt(pos).map(m => monster.fightStats.hp - m.currentHp).getOrElse(0L)

  private def generated(seed: Long, grip: Option[Grip] = None): Item =
    ItemGenerator.createItemOfType(ItemType.Weapon, 30L, ItemRarity.Blue, Rng(seed), grip)._1

  override def spec = suite("Луки и арбалеты")(

    test("двуручное — поровну меч, топор, копьё, лук и арбалет: лук или арбалет — каждое пятое") {
      val weapons = (1L to 4000L).map(s => generated(s))
      def share(g: Grip) = weapons.count(_.grip.contains(g)).toDouble / weapons.size
      // Одноручное — половина, из неё пятая часть — парирующие кинжалы.
      assertTrue(share(Grip.OneHanded) > 0.36 && share(Grip.OneHanded) < 0.44) &&
      assertTrue(share(Grip.Parrying) > 0.08 && share(Grip.Parrying) < 0.12) &&
      assertTrue(share(Grip.TwoHanded) > 0.26 && share(Grip.TwoHanded) < 0.34) &&
      assertTrue(share(Grip.Bow) > 0.08 && share(Grip.Bow) < 0.12) &&
      assertTrue(share(Grip.Crossbow) > 0.08 && share(Grip.Crossbow) < 0.12)
    },

    test("лук и арбалет: все сто процентов статов, гнездо как у двуручного, имя без «Двуручный»") {
      val bows    = (1L to 200L).map(s => generated(s, Some(Grip.Bow)))
      val swords  = (1L to 200L).map(s => generated(s, Some(Grip.TwoHanded)))
      val arbs    = (1L to 200L).map(s => generated(s, Some(Grip.Crossbow)))
      // «🔵 Хороший Лук Стрелка»: оружие — третьим словом
      val bowName = bows.forall(_.name.split(" ")(2) == "Лук")
      val arbName = arbs.forall(_.name.split(" ")(2) == "Арбалет")
      val noWord  = (bows ++ arbs).forall(!_.name.contains("Двуруч"))
      val stats   = bows.zip(swords).forall { case (b, s) => b.attack == s.attack && b.evasion == s.evasion }
      assertTrue(bowName && arbName && noWord && stats) &&
      assertTrue((bows ++ arbs).forall(i => i.twoHanded && i.ranged)) &&
      assertTrue(bows.forall(_.sockets.nonEmpty) && bows.exists(_.sockets.size == GripRates.TwoHandedMaxSockets)) &&
      // а на карточке сказано, чем лук отличается от меча
      assertTrue(bows.head.statsLines.exists(_.startsWith("🏹 Бьёт на 3 места")))
    },

    test("досягаемость: лук и арбалет — три места, прочее оружие и голые руки — соседи") {
      val bow  = hero(weapon(Grip.Bow))
      val arb  = hero(weapon(Grip.Crossbow))
      val two  = hero(weapon(Grip.TwoHanded))
      val bare = hero(Item.NoItem)
      assertTrue(BattleState.heroReach(bow) == 3 && BattleState.heroReach(arb) == 3) &&
      assertTrue(BattleState.heroReach(two) == 1 && BattleState.heroReach(bare) == 1) &&
      // умения оружия бьют как оружие, Таран и «Отбросить» — только вблизи
      assertTrue(BattleState.skillReach(bow, Skill.QuickStrike) == 3) &&
      assertTrue(BattleState.skillReach(bow, Skill.Ram) == 1 && BattleState.skillReach(bow, Skill.Shove) == 1)
    },

    test("с четвёртого места лук достаёт места с первого по седьмое") {
      val b = group(hero(weapon(Grip.Bow)), 8)
      // герой и активный на четвёртом, остальные — на 1–3 и 5–8
      val g = b.group.copy(heroPos = 4, activePos = 4, places = List(1, 2, 3, 5, 6, 7, 8))
      assertTrue(g.targetsWithin(3) == List(1, 2, 3, 4, 5, 6, 7)) &&
      assertTrue(g.attackTargets == List(3, 4, 5) && g.targetsWithin(1) == g.attackTargets)
    },

    test("вблизи стрелковое бьёт на пятую часть слабее: напротив и соседи; дальше — в полную силу") {
      val bow   = hero(weapon(Grip.Bow))
      val b     = group(bow, 5)
      def at(p: Int) = b.copy(group = b.group.copy(activePos = p))
      assertTrue(BattleState.rangedCloseMod(bow, at(1)) == 0.8 && BattleState.rangedCloseMod(bow, at(2)) == 0.8) &&
      assertTrue(BattleState.rangedCloseMod(bow, at(3)) == 1.0 && BattleState.rangedCloseMod(bow, at(4)) == 1.0) &&
      // меч вблизи не слабеет
      assertTrue(BattleState.rangedCloseMod(hero(weapon(Grip.TwoHanded)), at(1)) == 1.0)
    },

    test("в бою: из лука на выбор четыре цели, мечом — две; дальняя получает полный удар, ближняя — на пятую часть меньше") {
      val bow   = hero(weapon(Grip.Bow))
      val sword = hero(weapon(Grip.TwoHanded))
      for {
        tb <- makeState(bow, group(bow, 5))
        (bowState, _, rb) = tb
        _       <- bowState.action(testUser, tap("Attack"), rb)
        bowAims <- targets(rb)
        ts <- makeState(sword, group(sword, 5))
        (swordState, _, rs) = ts
        _         <- swordState.action(testUser, tap("Attack"), rs)
        swordAims <- targets(rs)
        far   <- strikeLoss(bow, 4)
        close <- strikeLoss(bow, 1)
      } yield assertTrue(bowAims == List(1, 2, 3, 4) && swordAims == List(1, 2)) &&
              assertTrue(far > 0L && close > 0L) &&
              assertTrue(close < far && close >= far * 3 / 4 && close <= far * 85 / 100)
    },

    test("умение оружия из лука целится на три места, Таран с кирасы — только вблизи") {
      val h = hero(weapon(Grip.Bow, Some(Skill.QuickStrike)))
      for {
        t <- makeState(h, group(h, 5))
        (state, _, r) = t
        _      <- state.action(testUser, tap("Skill_101"), r)
        weapon <- targets(r)
        _      <- state.action(testUser, tap("CancelTarget"), r)
        _      <- state.action(testUser, tap("Skill_202"), r)
        ram    <- targets(r)
      } yield assertTrue(weapon == List(1, 2, 3, 4) && ram == List(1, 2))
    }
  )
}
