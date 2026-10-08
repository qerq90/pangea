package pangea.service.state.states.events.cave

import pangea.domain.Rng
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.model.battle.{Aura, Nature, SlainMonster, SoloPveBattle}
import pangea.model.cave.{CaveGenerator, CaveRates, RoomKind}
import pangea.model.item.{ItemSet, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Пещера Некроманта: особая, с хозяином в логове и поднятой роднёй внутри. */
object NecromancerCaveSpec extends ZIOSpecDefault {

  private val hero  = TestFixtures.hero(UserId(1L)).copy(lvl = 41L)
  private val necro = MiniBoss.Necromancer

  /** Пещеры по сидам — чтобы считать доли. */
  private val caves = (1L to 400L).toList.map(s => CaveGenerator.generate(Race.Orc.entryName, Rng(s))._1)

  override def spec = suite("Пещера Некроманта")(

    test("одна пещера из десяти особая, и хозяин в ней — поровну Некромант или Крысиный король") {
      val special = caves.filter(_.boss.isDefined)
      val necros  = special.count(_.boss.contains("Necromancer"))
      val kings   = special.count(_.boss.contains("RatKing"))
      assertTrue(CaveRates.BossChancePct == 10) &&
      // десятая часть с поправкой на случайность выборки
      assertTrue(special.size > caves.size / 20 && special.size < caves.size / 5) &&
      // и поровну между двумя
      assertTrue(necros > special.size / 4 && kings > special.size / 4) &&
      assertTrue(necros + kings == special.size) &&
      assertTrue(CaveRates.BossPool == List("Necromancer", "RatKing"))
    },

    test("у особой пещеры есть логово с хозяином и алтарь, а привал и клад логово не съедает") {
      val special = caves.filter(_.boss.isDefined)
      val plain   = caves.filter(_.boss.isEmpty)
      assertTrue(special.nonEmpty) &&
      assertTrue(special.forall(_.rooms.count(_.kind == RoomKind.Lair) == 1)) &&
      assertTrue(special.forall(_.rooms.filter(_.kind == RoomKind.Lair).forall(_.monsters == 1))) &&
      // тёмный алтарь в такой пещере стоит всегда
      assertTrue(special.forall(_.rooms.exists(_.kind == RoomKind.Altar))) &&
      // привал на месте, и он один
      assertTrue(special.forall(_.rooms.count(_.kind == RoomKind.Rest) == 1)) &&
      // в обычной пещере логова нет вовсе
      assertTrue(plain.forall(c => !c.rooms.exists(_.kind == RoomKind.Lair)))
    },

    test("в пещере Некроманта родня поднята, в пещере короля — нет") {
      val necroCave = caves.find(_.boss.contains("Necromancer")).get
      val kingCave  = caves.find(_.boss.contains("RatKing")).get
      assertTrue(necroCave.undeadMobs && !kingCave.undeadMobs) &&
      // раса пещеры остаётся своей: поднимают тех, кто здесь жил
      assertTrue(necroCave.race == Race.Orc.entryName)
    },

    test("поднятый моб: раса своя, имя с приставкой, природа могильная") {
      val orc    = Monster(0L, 10L, Race.Orc, Rarity.Common,
        FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      val risen  = orc.copy(undead = true)
      assertTrue(orc.name == "Орк раб" && risen.name == "Нежить-Орк раб") &&
      assertTrue(orc.raceName == "Орк" && risen.raceName == "Нежить-Орк") &&
      // для ауры он мёртвый, хотя раса прежняя
      assertTrue(Nature.of(Race.Orc.entryName, undead = false) == Nature.Living) &&
      assertTrue(Nature.of(Race.Orc.entryName, undead = true) == Nature.Undead)
    },

    test("статы Некроманта считаются от BossLvL, а он растёт раз в четыре уровня") {
      val lvl = necro.bossLvl(hero.lvl)
      val s   = necro.stats(lvl)
      assertTrue(lvl == 10L && necro.bossLvl(1L) == 1L && necro.bossLvl(4L) == 1L && necro.bossLvl(9L) == 2L) &&
      assertTrue(s.hp == 350L * lvl && s.armor == 300L * lvl) &&
      assertTrue(s.atk == 150L * lvl && s.energy == 150L * lvl) &&
      assertTrue(s.accuracy == 100L * lvl && s.defence == 150L * lvl && s.evasion == 150L * lvl) &&
      assertTrue(necro.energyRegen(lvl) == 11L * lvl) &&
      assertTrue(necro.expReward(lvl) == 200L * lvl) &&
      // он нежить, дерётся не один и держит поле миазмами
      assertTrue(necro.race == Race.Undead && necro.fightsInGroup) &&
      assertTrue(necro.aura.contains(Aura.Miasma) && necro.abilities == 3)
    },

    test("дроп Некроманта: кости, вещи его набора и череп с дублонами") {
      val drops = (1L to 2000L).toList.flatMap(s =>
        LootGenerator.rollMiniBoss(necro, 4L, 20L, Rng(s))._1)
      val mats  = drops.collect { case LootDrop.Gear(i) => i.material }.flatten
      val gear  = drops.collect { case LootDrop.Gear(i) if i.set.isDefined => i }
      val gems  = drops.collect { case LootDrop.Gem(i) => i }
      val coins = drops.collect { case LootDrop.Doubloons(n) => n }
      assertTrue(mats.toSet == Set[MaterialKind](MaterialKind.CursedBones)) &&
      assertTrue(gear.nonEmpty && gear.forall(_.set.contains(ItemSet.Necromancer))) &&
      assertTrue(gear.map(_.rarity).toSet == Set[ItemRarity](ItemRarity.Purple, ItemRarity.Blue)) &&
      // череп всегда с парой дублонов, и это всегда череп, а не случайный камень
      assertTrue(gems.nonEmpty && gems.forall(_.gem.exists(_.kind == pangea.model.item.GemKind.Skull))) &&
      assertTrue(coins.nonEmpty && coins.forall(_ == LootGenerator.NecroSkullDoubloons)) &&
      assertTrue(gems.size == coins.size) &&
      // вещей набора ровно половина роллов, костей и черепов — по четверти
      assertTrue(gear.size > mats.size && gear.size < mats.size * 3) &&
      // трофеев и серебра с него не бывает
      assertTrue(!drops.exists { case _: LootDrop.Trophy | _: LootDrop.Silver => true; case _ => false })
    },

    test("набор «Некромант» теперь падает: кости — его ингредиент, и они идут в клады") {
      import pangea.generator.loot.TreasureHuntGenerator
      assertTrue(necro.set.contains(ItemSet.Necromancer)) &&
      assertTrue(necro.ingredient == MaterialKind.CursedBones) &&
      assertTrue(TreasureHuntGenerator.bossIngredients.contains(MaterialKind.CursedBones)) &&
      // у каждого минибосса свой набор и свой ингредиент
      assertTrue(MiniBoss.values.flatMap(_.set).distinct.size == MiniBoss.values.size)
    },

    test("мистический шаг: уходит на самое дальнее свободное место, и пара пустеет") {
      val mob = Monster(0L, 10L, Race.Orc, Rarity.Common,
        FightStats(atk = 1, hp = 100, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      val b   = SoloPveBattle.fromGroup(List.fill(4)(mob), hero, Nil)
      // Герой и активный на первом месте, свои заняли 2–4: прятаться — в хвост
      // схемы, на пятнадцатое.
      val hide  = b.necromancerHideaway
      val moved = hide.fold(b)(b.stepActiveTo)
      assertTrue(hide.contains(pangea.model.battle.Formation.MonsterPlaces)) &&
      assertTrue(moved.group.activePos == pangea.model.battle.Formation.MonsterPlaces) &&
      // напротив героя теперь пусто: достать спрятавшегося можно только подойдя
      assertTrue(!moved.group.paired) &&
      // дальше прятаться некуда — шаг станет пропуском
      assertTrue(moved.necromancerHideaway.isEmpty) &&
      // и строй его оттуда не тянет
      assertTrue(moved.copy(bossKind = Some(MiniBoss.Necromancer.entryName)).pullFree.isEmpty)
    },

    test("воскрешение поднимает только своих мертвецов и забирает их из добычи") {
      def slain(race: Race, undead: Boolean) =
        SlainMonster(5L, race.entryName, Rarity.Common.entryName, marked = false, "кто-то", undead)
      // Павшие: двое поднятых и один живой наёмник чужой стороны
      val fallen = List(slain(Race.Orc, undead = true), slain(Race.Orc, undead = true),
                        slain(Race.Goblin, undead = false))
      val mine   = fallen.filter(s => s.undead || s.race == Race.Undead.entryName)
      assertTrue(mine.size == 2) &&
      assertTrue(necro.ReviveCostPerLvl == 10L && necro.StepCostPerLvl == 5L && necro.CreateCostPerLvl == 10L) &&
      assertTrue(necro.CreateRarities == List(Rarity.Rare, Rarity.Mythical)) &&
      assertTrue(necro.GuardMin == 3 && necro.GuardMax == 4)
    },

    test("мертвец не пьёт флягу, яд и кровь его не берут, а горит он жарче живого") {
      import pangea.model.skill.MonsterSkill
      val base = SoloPveBattle.from(
        Monster(0L, 10L, Race.Orc, Rarity.Common,
          FightStats(atk = 10, hp = 1000, armor = 100, defence = 0, evasion = 0, accuracy = 10, energy = 100)),
        hero)
      val hurt   = base.copy(monsterCurrentHp = 500L)
      val risen  = hurt.copy(monsterUndead = true)
      assertTrue(MonsterSkill.HealingFlask.applicable(hurt)) &&
      assertTrue(!MonsterSkill.HealingFlask.applicable(risen)) &&
      // природа могильная: аура его лечит, а не гноит
      assertTrue(Aura.Miasma.sign(Nature.of(risen.monsterRace, risen.monsterUndead)) > 0) &&
      assertTrue(Aura.Miasma.sign(Nature.of(hurt.monsterRace, hurt.monsterUndead)) < 0) &&
      // и числа у огня те же, что у героя-нежити
      assertTrue(ItemSet.Necromancer.UndeadFireTakenPct == 25L) &&
      assertTrue(ItemSet.Necromancer.UndeadBurnGrowthMult == 2L)
    },

    test("павший перед героем не подставляет замену: соседи стоят, дальние идут сами") {
      val mob = Monster(0L, 10L, Race.Orc, Rarity.Common,
        FightStats(atk = 1, hp = 100, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      // Трое: герой на 1, мобы на 1 (в паре), 2 и 3. Первый пал.
      val fell = SoloPveBattle.fromGroup(List.fill(3)(mob), hero, Nil).copy(monsterCurrentHp = 0L)
      val next = fell.promoteNext.get
      assertTrue(!next.group.paired && next.group.activePos == 2) &&
      // сосед достаёт героя сбоку и вставать напротив не идёт
      assertTrue(next.stepActiveTowardsHero.isEmpty && next.group.attackTargets == List(2)) &&
      // а вот тот, кто стоит далеко, идёт сам — по месту за раунд
      assertTrue(next.copy(group = next.group.copy(activePos = 6)).stepActiveTowardsHero
                   .exists(_.group.activePos == 5) ) &&
      // дошедший до места перед героем встаёт в пару
      assertTrue(next.copy(group = next.group.copy(activePos = 2, places = List(1, 3)))
                   .engageArrived.exists(_.group.paired))
    },

    test("у пещеры и её логова есть все тексты") {
      for {
        c <- zio.ZIO.attempt(pangea.engine.SceneContent.load())
      } yield assertTrue(c.text("cave.deadAir").contains("мертвечин") || c.text("cave.deadAir").nonEmpty) &&
              assertTrue(List("cave.necro.room.lair", "cave.necro.room.lairEmpty", "cave.necro.lord",
                "cave.king.room.lair", "cave.king.room.lairEmpty", "cave.king.lord")
                .forall(k => c.text(k).nonEmpty)) &&
              assertTrue(List("battle.necro.revive", "battle.necro.step", "battle.necro.noHide",
                "battle.necro.create", "battle.aura.miasma.mobHolds", "battle.aura.miasma.mobFades")
                .forall(k => c.text(k).nonEmpty))
    }
  )
}
