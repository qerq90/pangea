package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Grip, Item, ItemDetails, ItemType, ShieldRates, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{Rune, RuneStone}
import pangea.model.skill.{MonsterSkill, Skill}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.InventoryState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Щит: держат в руке, но это не оружие. Защита у него всегда есть, умение —
  * Блок: до конца раунда удары и умения врагов по герою слабее на 75%. */
object ShieldSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def shield(id: Long = 301L): Item =
    Item(id, "Щит", 1L, ItemRarity.Gray, ItemType.Shield,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.HandSkill(Skill.Block))

  private def sword(id: Long, skill: Skill = Skill.QuickStrike): Item =
    Item(id, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      grip = Some(Grip.OneHanded), details = ItemDetails.Weapon(skill))

  private def hero(right: Item, left: Item = Item.NoItem): Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 100L, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = base.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = right, leftHand = left))
  }

  private val monster: Monster =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = 1000, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def generated(seed: Long, rarity: ItemRarity = ItemRarity.Blue): Item =
    ItemGenerator.createItemOfType(ItemType.Shield, 20L, rarity, Rng(seed))._1

  override def spec = suite("Щит")(

    test("щит падает так же часто, как оружие") {
      val items   = (1L to 8000L).map(s => ItemGenerator.createItem(20L, ItemRarity.Blue, Rng(s))._1)
      val shields = items.count(_.itemType == ItemType.Shield).toDouble / items.size
      val weapons = items.count(_.itemType == ItemType.Weapon).toDouble / items.size
      assertTrue(shields > 0.04 && shields < 0.06 && weapons > 0.04 && weapons < 0.06)
    },

    test("щит: пять видов, защита не меньше трёх за уровень, атаки нет, умение — Блок, гнёзд до двух") {
      val shields = (1L to 400L).map(s => generated(s))
      val top     = (1L to 100L).map(s => generated(s, ItemRarity.Orange))
      val green   = (1L to 100L).map(s => generated(s, ItemRarity.Green))
      val kinds   = List("Круглый Щит", "Треугольный Щит", "Баклер", "Башенный Щит", "Костяной Щит")
      val named   = shields.forall(i => kinds.exists(i.name.contains)) && kinds.forall(k => shields.exists(_.name.contains(k)))
      assertTrue(named) &&
      assertTrue(shields.forall(i => i.defence >= ShieldRates.DefencePerLvl * i.lvl && i.attack == 0L)) &&
      assertTrue(shields.forall(_.details == ItemDetails.HandSkill(Skill.Block))) &&
      assertTrue(shields.forall(_.sockets.size <= 2) && shields.exists(_.sockets.size == 2)) &&
      // легендарному — всегда два (таблица оружия даёт 2–3, щиту больше двух не положено)
      assertTrue(top.forall(_.sockets.size == 2) && green.forall(_.sockets.isEmpty)) &&
      assertTrue(shields.head.statsLines.contains("Активный навык: «Блок»"))
    },

    test("Блок — не руна: Казимир его со щита не снимает, рунных камней на него нет") {
      assertTrue(Rune.of(shield()).isEmpty) &&
      assertTrue(!RuneStone.all.contains(Rune.Active(Skill.Block))) &&
      // а обычное умение оружия руной остаётся
      assertTrue(Rune.of(sword(1L)).contains(Rune.Active(Skill.QuickStrike)))
    },

    test("щит берут в любую руку; два щита — это без оружия; щит снимает двуручное") {
      val base = hero(Item.NoItem)
      val (eq1, f1, _) = InventoryState.equip(base, sword(1L), None)
      val (eq2, _, _)  = InventoryState.equip(base.copy(equipment = eq1, fightStats = f1), shield(), None)
      val twoShields   = TestFixtures.emptyEquipment.copy(weapon = shield(301L), leftHand = shield(302L))
      val big          = sword(5L).copy(grip = Some(Grip.TwoHanded))
      val (eq3, _, out) = InventoryState.equip(hero(big), shield(), Some(2))
      assertTrue(eq2.weapon.id == 1L && eq2.leftHand.id == 301L && !eq2.unarmed) &&
      assertTrue(twoShields.unarmed && twoShields.handShields.size == 2) &&
      assertTrue(eq3.leftHand.id == 301L && eq3.weapon.itemType == ItemType.NoItem && out == List(big))
    },

    test("камни щита — броневые, а не оружейные") {
      val gem   = Gem(GemKind.Ruby, 1)
      val withGem = TestFixtures.emptyEquipment.copy(leftHand = shield().copy(sockets = List(Some(gem))))
      assertTrue(withGem.armorGems == List(gem) && withGem.weaponGems.isEmpty)
    },

    test("одинаковые умения не складываются: два одинаковых меча — одна кнопка, два щита — один Блок") {
      val swords  = hero(sword(1L), sword(2L)).activeSkillSlots.map(_.skill)
      val shields = hero(shield(301L), shield(302L)).activeSkillSlots.map(_.skill)
      val mixed   = hero(sword(1L), shield()).activeSkillSlots.map(_.skill)
      assertTrue(swords == List(Skill.QuickStrike) && shields == List(Skill.Block)) &&
      assertTrue(mixed == List(Skill.QuickStrike, Skill.Block))
    },

    test("кнопка Блока стоит в ряду действий, справа от «Атаки»") {
      val h = hero(sword(1L), shield())
      for {
        t <- makeState(h, SoloPveBattle.from(monster, h))
        (state, _, r) = t
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        row1    = screen.choices.filter(_.row.contains(1)).map(_.id)
      } yield assertTrue(row1.take(2) == List("Attack", "Skill_301")) &&
              // умение меча — над рядом действий, как раньше
              assertTrue(screen.choices.exists(c => c.id == "Skill_1" && c.row.contains(0)))
    },

    test("Блок кончает ход без удара, а удар моба в этом раунде серый щит режет вдвое; следующий — целиком") {
      val h = hero(sword(1L), shield())
      def round(block: Boolean) =
        for {
          t <- makeState(h, SoloPveBattle.from(monster, h))
          (state, dao, r) = t
          // Удар героя — бросок попадания и разброс; Блок — только разброс умения.
          _      <- if (block) TestRandom.feedInts(99) *> TestRandom.feedLongs(100L, 100L)
                    else TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
          _      <- state.action(testUser, tap(if (block) "Skill_301" else "Attack"), r)
          after  <- dao.getHeroByUserId(userId).map(_.get)
          battle <- battleOf(dao)
        } yield (h.fightStats.hp - after.fightStats.hp, battle)
      for {
        plain   <- round(block = false)
        blocked <- round(block = true)
        (taken, raised) = blocked
      } yield assertTrue(plain._1 > 0L && taken == BattleState.shieldCut(ShieldRates.blockPct(ItemRarity.Gray), plain._1)) &&
              // моб цел: удара после Блока нет
              assertTrue(raised.monsterCurrentHp == monster.fightStats.hp) &&
              assertTrue(raised.heroShieldUp && raised.skillSlots.find(_.itemId == 301L).exists(_.cooldown == 2))
    },

    test("щит опускается со следующим ходом героя") {
      val h = hero(sword(1L), shield())
      for {
        t <- makeState(h, SoloPveBattle.from(monster, h).copy(heroBlockPct = 50L))
        (state, dao, r) = t
        _      <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        battle <- battleOf(dao)
      } yield assertTrue(!battle.heroShieldUp)
    },

    test("умение моба под поднятым щитом режется тем же процентом") {
      val h     = hero(sword(1L), shield())
      val b     = SoloPveBattle.from(monster, h)
      val open  = MonsterSkill.QuickStrike.cast(b, h, 0L)
      val shut  = MonsterSkill.QuickStrike.cast(b.copy(heroBlockPct = 50L), h, 0L)
      val crush = MonsterSkill.CrushingStrike.cast(b.copy(heroBlockPct = 50L), h, 0L)
      val lost  = h.fightStats.hp - open.heroHp
      assertTrue(lost > 0L && h.fightStats.hp - shut.heroHp == BattleState.shieldCut(50L, lost)) &&
      assertTrue(h.fightStats.hp - crush.heroHp == BattleState.shieldCut(50L, (monster.fightStats.atk * 3L / 10L).max(1L)))
    },

    test("щит снимает свой процент, но единица проходит всегда; опущенный щит не режет") {
      assertTrue(BattleState.shieldCut(75L, 100L) == 25L && BattleState.shieldCut(50L, 100L) == 50L) &&
      assertTrue(BattleState.shieldCut(0L, 100L) == 100L && BattleState.shieldCut(50L, 1L) == 1L)
    },

    test("Блок по редкости: серый и белый — 50%, дальше +5% за редкость, легендарный — 75%; из двух щитов — лучший") {
      val pcts = List(ItemRarity.Gray, ItemRarity.White, ItemRarity.Green, ItemRarity.Blue,
                      ItemRarity.Purple, ItemRarity.Violet, ItemRarity.Orange).map(ShieldRates.blockPct)
      val white  = shield(301L).copy(rarity = ItemRarity.White)
      val violet = shield(302L).copy(rarity = ItemRarity.Violet)
      val two    = TestFixtures.emptyEquipment.copy(weapon = white, leftHand = violet)
      val blue   = shield(303L).copy(rarity = ItemRarity.Blue)
      val green  = hero(sword(1L), shield(304L).copy(rarity = ItemRarity.Green))
      assertTrue(pcts == List(50L, 50L, 55L, 60L, 65L, 70L, 75L)) &&
      assertTrue(two.blockPct == 70L) &&
      // в описании — число: на карточке щита — этого щита, в умениях — надетого
      assertTrue(Skill.Block.describe(green, Some(blue)).contains("на 60% меньше урона")) &&
      assertTrue(Skill.Block.describe(green).contains("на 55% меньше урона")) &&
      assertTrue(!Skill.Block.describe(green).contains("{block}"))
    },

    test("Блок синим щитом: в бою — его процент, и он же в строке лога") {
      val h = hero(sword(1L), shield().copy(rarity = ItemRarity.Blue))
      for {
        t <- makeState(h, SoloPveBattle.from(monster, h))
        (state, dao, r) = t
        _      <- TestRandom.feedInts(99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Skill_301"), r)
        battle <- battleOf(dao)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(battle.heroBlockPct == 60L && log.contains("слабее на 60%"))
    }
  )
}
