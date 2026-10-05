package pangea.model.monster

import pangea.domain.Rng
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Именные легендарные: у каждой расы свой список имён, в одном бою тёзок не
  * бывает, а прочие редкости как ходили по расе и тиру, так и ходят. */
object LegendaryNamesSpec extends ZIOSpecDefault {

  private val hero = TestFixtures.hero(UserId(1L))

  private def legendary(race: Race, name: Option[String]): Monster =
    Monster(0L, 10L, race, Rarity.Legendary,
      FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
      customName = name)

  override def spec = suite("Имена легендарных")(

    test("у каждой смертной расы свой список, и прежнее имя в нём первое") {
      val pools = Race.mortals.map(r => r -> Monster.legendaryNames.getOrElse(r, Nil))
      assertTrue(pools.forall { case (_, names) => names.sizeIs == 12 }) &&
      assertTrue(pools.forall { case (_, names) => names.distinct == names }) &&
      assertTrue(pools.forall { case (_, names) => names.forall(_.trim.nonEmpty) }) &&
      // прежнее имя расы осталось — и стоит первым
      assertTrue(pools.forall { case (race, names) =>
        Monster.namesByRaceRarity.get((race, Rarity.Legendary)).contains(names.head) }) &&
      // имена не повторяются и между расами
      assertTrue(pools.flatMap(_._2).distinct.size == pools.flatMap(_._2).size) &&
      // боссовым расам имена не раздаём: у минибоссов они свои
      assertTrue(Race.bossRaces.forall(r => Monster.legendaryNames.get(r).isEmpty))
    },

    test("имя получает только легендарный") {
      val lvls = (1L to 60L).toList
      val mobs = lvls.map(s => MonsterGenerator.generateOfRace(10, Race.Orc, Rng(s))._1)
      val pool = Monster.legendaryNames(Race.Orc)
      assertTrue(mobs.filter(_.rarity != Rarity.Legendary).forall(_.customName.isEmpty)) &&
      assertTrue(mobs.filter(_.rarity == Rarity.Legendary).forall(m => pool.contains(m.customName.get))) &&
      // у не-легендарных имя по-прежнему из таблицы «раса × редкость»
      assertTrue(mobs.filter(_.rarity == Rarity.Common).forall(_.name == "Орк раб"))
    },

    test("имя катается: за сотню легендарных вылезает не одно и то же") {
      val names = (1L to 400L).toList
        .map(s => MonsterGenerator.generateOfRaceAndRarity(10, Race.Elf, Rarity.Legendary,
          MonsterGenerator.legendaryName(Race.Elf, Rng(s))._1).name)
      assertTrue(names.distinct.size >= 10) &&
      assertTrue(names.forall(n => Monster.legendaryNames.getOrElse(Race.Elf, Nil).contains(n)))
    },

    test("«Отмеченный тьмой» встаёт перед именем, а кнопка берёт расу") {
      val m = legendary(Race.Demon, Some("Велиар")).copy(marked = true)
      assertTrue(m.name == s"${Monster.MarkedPrefix} Велиар") &&
      assertTrue(m.shortName == s"${Monster.MarkedPrefix} ${Race.Demon}") &&
      assertTrue(legendary(Race.Demon, Some("Велиар")).name == "Велиар")
    },

    test("в одном бою тёзок нет, пока в списке есть свободные имена") {
      val pool  = Monster.legendaryNames(Race.Goblin)
      val three = List.fill(3)(legendary(Race.Goblin, Some(pool.head)))
      val mixed = Monster.distinctNames(three)
      assertTrue(mixed.flatMap(_.customName).distinct.size == 3) &&
      assertTrue(mixed.flatMap(_.customName).forall(n => pool.contains(n))) &&
      // первому достаётся то, что выпало ему, разводятся следующие
      assertTrue(mixed.head.customName.contains(pool.head)) &&
      // безымянных это не трогает
      assertTrue(Monster.distinctNames(List(
        Monster(0L, 1L, Race.Orc, Rarity.Common, FightStats(1, 1, 0, 0, 0, 1, 0)),
        Monster(0L, 1L, Race.Orc, Rarity.Common, FightStats(1, 1, 0, 0, 0, 1, 0))
      )).forall(_.customName.isEmpty))
    },

    test("имён не хватило — тёзки всё-таки выходят, бой не ломается") {
      val pool = Monster.legendaryNames(Race.Gnome)
      val many = List.fill(pool.size + 2)(legendary(Race.Gnome, Some(pool.head)))
      val out  = Monster.distinctNames(many)
      assertTrue(out.size == many.size) &&
      assertTrue(out.flatMap(_.customName).distinct.size == pool.size)
    },

    test("имя доезжает до боя и обратно из строя") {
      val pool  = Monster.legendaryNames(Race.Human)
      val group = List(legendary(Race.Human, Some(pool(1))), legendary(Race.Human, Some(pool(2))))
      val battle = SoloPveBattle.fromGroup(group, hero, List(0L, 0L))
      // в паре — первый, в строю — второй, и у каждого своё имя
      val sided = battle.engage(2)
      assertTrue(battle.monsterName == pool(1)) &&
      assertTrue(battle.group.others.head.name == pool(2)) &&
      assertTrue(sided.monsterName == pool(2)) &&
      // вернули обратно — имя на месте
      assertTrue(sided.engage(1).monsterName == pool(1)) &&
      // и переживает сохранение
      assertTrue({
        import io.circe.syntax.EncoderOps
        battle.asJson.as[SoloPveBattle].toOption.exists(b =>
          b.monsterName == pool(1) && b.group.others.head.name == pool(2))
      })
    }
  )
}
