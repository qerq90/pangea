package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{CubeCraft, MaterialGenerator}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{BeltSlot, DivineRates, Item, ItemDetails, ItemType, MaterialKind, RoseKind, RoseRates, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Раскрывшаяся роза: цветок из куба Азата, который бьёт по всему полю, как
  * божественное оружие, но силу берёт от уровня хозяина и держит три раскрытия.
  * Место цветка — ячейка пояса: в доп. слот он больше не надевается. */
object RoseBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def useSlot(idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"BeltUse","slot":"$idx"}"""))

  private val lvl = 10L

  private def flower(kind: RoseKind, charges: Int): Item = {
    val item = RoseKind.item(kind).copy(id = 7L)
    item.copy(details = ItemDetails.Rose(kind, charges, RoseRates.Charges))
  }

  /** Пояс с ячейками: синий держит три — больше в тестах и не нужно. */
  private def belt(slots: BeltSlot*): Item =
    Item(9L, "Пояс", 1L, ItemRarity.Blue, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(slots.toList))

  private def hero(kind: RoseKind, charges: Int = RoseRates.Charges): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 20, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = h.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(belt = belt(BeltSlot.Rose(kind, charges))))
  }

  private def monster(hp: Long): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 20, hp = hp, armor = 100, defence = 100, evasion = 0, accuracy = 9999, energy = 0))

  private def group(h: Hero, hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster), h, Nil)

  private def makeState(h: Hero, b: SoloPveBattle, inv: TestInventoryRepository = TestInventoryRepository.accepting) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, inv, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Что лежит в ячейках пояса героя. */
  private def slots(h: Hero): List[BeltSlot] = h.equipment.belt.details match {
    case b: ItemDetails.Belt => b.slots
    case _                   => Nil
  }

  /** Урон розы: ставка божественного удара на уровень ГЕРОЯ. */
  private val blow     = DivineRates.DamagePerLvl * lvl
  private val mobArmor = 100L

  override def spec = suite("Раскрывшаяся роза")(

    test("куб: роза + пыль самоцвета со стихией → цветок этой стихии на три раскрытия") {
      def craft(dust: MaterialKind) = CubeCraft.craft(
        List(MaterialGenerator.item(MaterialKind.UnopenedRose), MaterialGenerator.item(dust)), 5, Rng(1L))
      val red   = craft(MaterialKind.RubyDust)
      val blue  = craft(MaterialKind.SapphireDust)
      val green = craft(MaterialKind.EmeraldDust)
      // Аметистовая пыль и Чёрный порошок стихии не несут — роза их не берёт.
      val amethyst = craft(MaterialKind.AmethystDust)
      val skull    = craft(MaterialKind.dustOf(pangea.model.item.GemKind.Skull))
      assertTrue(red.chargesUsed == 1 && red.items.flatMap(_.rose).map(_.kind) == List(RoseKind.Red)) &&
      assertTrue(blue.items.flatMap(_.rose).map(_.kind) == List(RoseKind.Blue)) &&
      assertTrue(green.items.flatMap(_.rose).map(_.kind) == List(RoseKind.Green)) &&
      assertTrue(red.items.flatMap(_.rose).forall(r => r.charges == 3 && r.maxCharges == 3)) &&
      // роза и пыль израсходованы, на выходе один цветок
      assertTrue(red.items.size == 1) &&
      // цветок — расходник, а не вещь в слот: надеть его больше некуда
      assertTrue(red.items.head.itemType == ItemType.Rose &&
                 !ItemType.equippable.contains(ItemType.Rose)) &&
      assertTrue(!amethyst.anyApplied && !skull.anyApplied) &&
      assertTrue(RoseKind.fromDust(MaterialKind.AmethystDust).isEmpty)
    },

    test("своей кнопки в бою у цветка нет — он лежит в поясе и достаётся оттуда") {
      val h = hero(RoseKind.Red)
      for {
        t <- makeState(h, group(h, 100000L, 100000L))
        (state, _, r) = t
        _    <- state.enter(testUser, r)
        scr  <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("Belt"), r)
        list <- r.sentScreens.map(_.last)
      } yield assertTrue(!scr.choices.map(_.id).contains("UseRose")) &&
              assertTrue(scr.choices.map(_.id).contains("Belt")) &&
              // список пояса — отдельное сообщение с кнопками внутри него
              assertTrue(list.inline && list.text.contains("Красная роза")) &&
              assertTrue(list.choices.map(_.id) == List("BeltUse")) &&
              assertTrue(list.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
    },

    test("удар считается от уровня героя и бьёт всех: сперва броня, остатком по HP") {
      val h = hero(RoseKind.Blue)
      for {
        t <- makeState(h, group(h, 100000L, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        after   <- battleOf(dao)
        hero2   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.monsterCurrentHp == 100000L - (blow - mobArmor)) &&
              assertTrue(after.group.others.head.currentHp == 100000L - (blow - mobArmor)) &&
              assertTrue(screens.contains(blow.toString)) &&
              // раскрытие потратило один заряд из трёх
              assertTrue(slots(hero2) == List(BeltSlot.Rose(RoseKind.Blue, RoseRates.Charges - 1))) &&
              // мобы в ответ не бьют: раскрытие раунд не завершает
              assertTrue(after.monsterCurrentHp > 0L)
    },

    test("цветок — расходник раунда: второе раскрытие в том же раунде не проходит") {
      val h = hero(RoseKind.Yellow)
      for {
        t <- makeState(h, group(h, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        _       <- state.action(testUser, useSlot(0), r)
        hero2   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(hero2) == List(BeltSlot.Rose(RoseKind.Yellow, RoseRates.Charges - 1))) &&
              assertTrue(screens.contains("уже брались за пояс"))
    },

    test("последнее раскрытие осыпает цветок — ячейка пустеет, но помнит его") {
      val h = hero(RoseKind.Green, charges = 1)
      for {
        t <- makeState(h, group(h, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        hero2   <- dao.getHeroByUserId(userId).map(_.get)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(hero2) == List(BeltSlot.Rose(RoseKind.Green, 0))) &&
              // брать из ячейки больше нечего — в списке пояса её нет
              assertTrue(BattleState.beltUsable(hero2).isEmpty) &&
              assertTrue(screens.contains("осыпается лепестками")) &&
              // зелёная травит — яд лёг на врага
              assertTrue(after.effects.monsterPoison.isDefined)
    },

    test("после боя осыпавшаяся ячейка сама берёт такой же цветок из сумки") {
      val h     = hero(RoseKind.Red, charges = 1)
      val spare = flower(RoseKind.Red, RoseRates.Charges).copy(id = 42L)
      val inv   = TestInventoryRepository.withItems(List(spare))
      for {
        t <- makeState(h, group(h, 1L), inv)
        (state, dao, r) = t
        // раскрытие добивает моба: бой кончился — пояс добирает из сумки
        _       <- state.action(testUser, useSlot(0), r)
        hero2   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(hero2) == List(BeltSlot.Rose(RoseKind.Red, RoseRates.Charges))) &&
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(screens.contains("На пояс легло из сумки"))
    },

    test("цветок в сумке: уровня нет, зато видно, сколько раскрытий осталось") {
      val item = flower(RoseKind.White, charges = 2)
      assertTrue(item.displayTitle == "Белая роза") &&
      assertTrue(item.statsLines.exists(_.contains("Раскрытий: 2/3"))) &&
      assertTrue(item.statsLines.exists(_.contains("струи воздуха"))) &&
      assertTrue(item.statsLines.exists(_.contains("из ячейки пояса"))) &&
      assertTrue(RoseRates.Charges == 3)
    }
  )
}
