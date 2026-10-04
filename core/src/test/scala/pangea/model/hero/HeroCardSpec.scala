package pangea.model.hero

import pangea.model.monster.Race
import pangea.model.stats.{ParamsBuff, StatBoost, StatBoosts}
import pangea.model.trauma.Trauma
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Карточка «Персонажа»: число — текущее, в скобках за ним — родное, до расы,
  * травм и зелий. Там, где они совпали, скобки не рисуем. */
object HeroCardSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)

  private def hero(race: Race): Hero =
    TestFixtures.hero(userId).copy(race = race,
      baseStats = TestFixtures.hero(userId).baseStats.copy(str = 10L, vit = 10L, agi = 10L, int = 10L))

  override def spec = suite("Карточка персонажа")(

    test("человек без травм и зелий: числа одни и скобок нет") {
      val h    = hero(Race.Human)
      val card = h.getInfo(0L)
      assertTrue(card.contains("СИЛ 10") && card.contains("ТЕЛО 10")) &&
      assertTrue(card.contains("ЛОВ 10") && card.contains("ИНТ 10")) &&
      // у человека все множители единичные — родное равно текущему
      assertTrue(!card.contains("(10)")) &&
      assertTrue(h.nativeBaseStats.str == 10L && h.effectiveBaseStats(0L).str == 10L)
    },

    test("раса видна в скобках: орку приписали силу, а ум убавили") {
      val h    = hero(Race.Orc)
      val card = h.getInfo(0L)
      // ×1.2 силы и тела, ×0.8 ума и ловкости — родное у всех четырёх по десять
      assertTrue(h.nativeBaseStats.str == 10L && h.nativeBaseStats.int == 10L) &&
      assertTrue(card.contains("СИЛ 12 (10)") && card.contains("ТЕЛО 12 (10)")) &&
      assertTrue(card.contains("ЛОВ 8 (10)") && card.contains("ИНТ 8 (10)"))
    },

    test("травма и зелье двигают текущее, родное стоит на месте") {
      val base = hero(Race.Human)
      // «Ушиб ноги» бьёт по ловкости и уклонению, зелье прибавляет силы
      val hurt = base.copy(
        traumaUntil = Some(100000L),
        traumaNames = List(Trauma.BruisedLeg.name),
        statBoosts  = StatBoosts(List(StatBoost("test:str", ParamsBuff(str = 50, vit = 0, agi = 0, int = 0), 100000L))))
      val card = hurt.getInfo(0L)
      assertTrue(hurt.nativeBaseStats == base.nativeBaseStats) &&
      // сила выросла наполовину — текущее пятнадцать, родное десять
      assertTrue(hurt.effectiveBaseStats(0L).str == 15L && card.contains("СИЛ 15 (10)")) &&
      // а когда всё пройдёт, останется родное
      assertTrue(base.getInfo(0L).contains("СИЛ 10") && !base.getInfo(0L).contains("СИЛ 10 ("))
    },

    test("скобки только у четырёх характеристик: остальные строки одним числом") {
      val h     = hero(Race.Orc)
      val card  = h.getInfo(0L)
      val lines = card.linesIterator.toList
      // у орка раса сдвинула и потолок HP, и энергию — но там скобок быть не должно
      assertTrue(h.effectiveMaxHp(0L) > 10L * 24L) &&
      assertTrue(lines.exists(l => l.contains("СИЛ") && l.contains("("))) &&
      assertTrue(lines.filter(l => l.contains("❤") || l.contains("Энергия") ||
                                   l.contains("Атк") || l.contains("Точн")).forall(!_.contains("("))) &&
      // всего четыре скобки — по одной на характеристику
      assertTrue(card.count(_ == '(') == 4)
    },

    test("скобка появляется только при разнице") {
      assertTrue(Hero.withNative(7L, 7L) == "7") &&
      assertTrue(Hero.withNative(9L, 7L) == "9 (7)") &&
      assertTrue(Hero.withNative(5L, 7L) == "5 (7)")
    }
  )
}
