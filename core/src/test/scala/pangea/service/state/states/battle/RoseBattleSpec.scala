package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{CubeCraft, MaterialGenerator}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{DivineRates, Item, ItemDetails, MaterialKind, RoseKind, RoseRates}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Раскрывшаяся роза: цветок из куба Азата, который бьёт по всему полю, как
  * божественное оружие, но силу берёт от уровня хозяина и держит три раскрытия. */
object RoseBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  private def flower(kind: RoseKind, charges: Int): Item = {
    val item = RoseKind.item(kind).copy(id = 7L)
    item.copy(details = ItemDetails.Rose(kind, charges, RoseRates.Charges))
  }

  private def hero(kind: RoseKind, charges: Int = RoseRates.Charges): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 20, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = h.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(additionalWeapon = flower(kind, charges)))
  }

  private def monster(hp: Long): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 20, hp = hp, armor = 100, defence = 100, evasion = 0, accuracy = 9999, energy = 0))

  private def group(h: Hero, hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster), h, Nil)

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

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
      assertTrue(!amethyst.anyApplied && !skull.anyApplied) &&
      assertTrue(RoseKind.fromDust(MaterialKind.AmethystDust).isEmpty)
    },

    test("кнопка розы стоит над «Сбежать» и подписана её именем") {
      for {
        t <- makeState(hero(RoseKind.Red), group(hero(RoseKind.Red), 100000L, 100000L))
        (state, _, r) = t
        _   <- state.enter(testUser, r)
        scr <- r.sentScreens.map(_.last)
        roseBtn = scr.choices.find(_.id == "UseRose")
        fleeBtn = scr.choices.find(_.id == "Flee")
      } yield assertTrue(roseBtn.exists(_.label == "Красная роза")) &&
              assertTrue(roseBtn.flatMap(_.row).exists(row => fleeBtn.flatMap(_.row).exists(_ > row))) &&
              assertTrue(roseBtn.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
    },

    test("удар считается от уровня героя и бьёт всех: сперва броня, остатком по HP") {
      val h = hero(RoseKind.Blue)
      for {
        t <- makeState(h, group(h, 100000L, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("UseRose"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.monsterCurrentHp == 100000L - (blow - mobArmor)) &&
              assertTrue(after.group.others.head.currentHp == 100000L - (blow - mobArmor)) &&
              assertTrue(screens.contains(blow.toString)) &&
              // мобы в ответ не бьют: раскрытие раунд не завершает
              assertTrue(after.monsterCurrentHp > 0L)
    },

    test("раз в раунд: второе раскрытие в том же раунде не проходит") {
      val h = hero(RoseKind.Yellow)
      for {
        t <- makeState(h, group(h, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("UseRose"), r)
        _       <- state.action(testUser, tap("UseRose"), r)
        hero2   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(hero2.equipment.additionalWeapon.rose.exists(_.charges == RoseRates.Charges - 1)) &&
              assertTrue(screens.contains("второй раз за раунд"))
    },

    test("третье раскрытие осыпает цветок — доп. слот пустеет") {
      val h = hero(RoseKind.Green, charges = 1)
      for {
        t <- makeState(h, group(h, 100000L))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("UseRose"), r)
        hero2   <- dao.getHeroByUserId(userId).map(_.get)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(hero2.equipment.additionalWeapon == Item.NoItem) &&
              assertTrue(screens.contains("осыпается лепестками")) &&
              // зелёная травит — яд лёг на врага
              assertTrue(after.effects.monsterPoison.isDefined)
    },

    test("цветок в сумке: уровня нет, зато видно, сколько раскрытий осталось") {
      val item = flower(RoseKind.White, charges = 2)
      assertTrue(item.displayTitle == "Белая роза") &&
      assertTrue(item.statsLines.exists(_.contains("Раскрытий: 2/3"))) &&
      assertTrue(item.statsLines.exists(_.contains("струи воздуха"))) &&
      assertTrue(RoseRates.Charges == 3)
    }
  )
}
