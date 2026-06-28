package pangea.generator

import pangea.domain.Rng
import pangea.generator.monster.MonsterGenerator
import pangea.model.monster.Rarity
import zio.test._

object MonsterGeneratorSpec extends ZIOSpecDefault {

  override def spec = suite("MonsterGenerator")(

    test("уровень монстра совпадает с уровнем лабиринта") {
      val (monster, _) = MonsterGenerator.generate(7, Rng(42L))
      assertTrue(monster.lvl == 7L)
    },

    test("все базовые статы > 0 на уровне 1") {
      val (m, _) = MonsterGenerator.generate(1, Rng(1L))
      assertTrue(m.fightStats.atk > 0L) &&
      assertTrue(m.fightStats.hp  > 0L)
    },

    test("статы растут с уровнем лабиринта") {
      val (low,  _) = MonsterGenerator.generate(1,  Rng(99L))
      val (high, _) = MonsterGenerator.generate(50, Rng(99L))
      assertTrue(high.fightStats.hp  > low.fightStats.hp) &&
      assertTrue(high.fightStats.atk > low.fightStats.atk)
    },

    test("в среднем легендарные монстры значительно сильнее обычных") {
      val n = 200
      def avgHp(seeds: Seq[Long]) =
        seeds.map(s => MonsterGenerator.generate(10, Rng(s))._1)
          .filter(_.rarity == Rarity.Legendary)
          .map(_.fightStats.hp)

      // генерируем достаточно монстров чтобы получить оба типа
      val monsters = (1 to n).map(i => MonsterGenerator.generate(10, Rng(i.toLong))._1)
      val legendaryHp = monsters.filter(_.rarity == Rarity.Legendary).map(_.fightStats.hp)
      val commonHp    = monsters.filter(_.rarity == Rarity.Common).map(_.fightStats.hp)

      // legendary factor (3.0) / common factor (0.8) = 3.75x, учитываем разброс рас
      val legendaryAvg = legendaryHp.sum.toDouble / legendaryHp.size
      val commonAvg    = commonHp.sum.toDouble    / commonHp.size
      assertTrue(legendaryAvg > commonAvg * 2.0)
    },

    test("одинаковый сид даёт одинакового монстра") {
      val (m1, _) = MonsterGenerator.generate(5, Rng(123L))
      val (m2, _) = MonsterGenerator.generate(5, Rng(123L))
      assertTrue(m1.race   == m2.race) &&
      assertTrue(m1.rarity == m2.rarity) &&
      assertTrue(m1.fightStats == m2.fightStats)
    },

    test("разные сиды дают разных монстров") {
      val results = (1 to 20).map(i => MonsterGenerator.generate(5, Rng(i.toLong))._1.rarity).toSet
      assertTrue(results.size > 1)
    },

    test("редкость легендарного монстра встречается среди 1000 генераций") {
      val rarities = (1 to 1000).map(i => MonsterGenerator.generate(1, Rng(i.toLong))._1.rarity)
      assertTrue(rarities.contains(Rarity.Legendary))
    },

    test("name возвращает пафосное имя из таблицы Monster.namesByRaceRarity") {
      val (monster, _) = MonsterGenerator.generate(1, Rng(42L))
      val base         = pangea.model.monster.Monster.namesByRaceRarity((monster.race, monster.rarity))
      val expected     = if (monster.marked) s"Отмеченный тьмой $base" else base
      assertTrue(monster.name == expected)
    },

    test("модификатор «Отмеченный тьмой»: только Rare+, ~1.25%, добавляет префикс") {
      import pangea.model.monster.Rarity
      val n        = 8000
      val monsters = (1 to n).map(i => MonsterGenerator.generate(10, Rng(i.toLong))._1)
      val marked   = monsters.filter(_.marked)
      val rate     = marked.size.toDouble / n
      val allowed: Set[Rarity] = Set(Rarity.Rare, Rarity.Mythical, Rarity.Legendary)
      assertTrue(marked.nonEmpty) &&
      assertTrue(marked.forall(_.name.startsWith("Отмеченный тьмой "))) &&
      assertTrue(marked.forall(m => allowed.contains(m.rarity))) &&
      assertTrue(monsters.filterNot(_.marked).forall(!_.name.startsWith("Отмеченный тьмой"))) &&
      assertTrue(rate > 0.005 && rate < 0.025) // 5% от ~25% Rare+ ≈ 1.25%
    },

    test("отмеченный монстр сильнее обычного той же расы/редкости/уровня (+20%)") {
      val n        = 5000
      val monsters = (1 to n).map(i => MonsterGenerator.generate(10, Rng(i.toLong))._1)
      val pairOpt = monsters.find(_.marked).flatMap { mk =>
        monsters.find(m => !m.marked && m.race == mk.race && m.rarity == mk.rarity).map((mk, _))
      }
      assertTrue(pairOpt.exists { case (mk, plain) =>
        math.abs(mk.fightStats.hp - (plain.fightStats.hp * 1.2).toLong) <= 1L
      })
    }
  )
}
