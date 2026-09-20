package pangea.model.rune

import io.circe.syntax.EncoderOps
import pangea.model.hero.HeroPassives
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.skill.Skill
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Руны: сила от понимания (квадратичная шкала), очки по редкости, клейма и
  * места на теле, стоимость, потолок, хранение. */
object RuneSpec extends ZIOSpecDefault {

  private def weapon(id: Long, skill: Skill, rarity: Rarity = Rarity.Gray): Item =
    Item(id, "Меч", 1L, rarity, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Weapon(skill))
  private def helmet(id: Long, kind: PassiveKind, rarity: Rarity = Rarity.Gray): Item =
    Item(id, "Шлем", 1L, rarity, ItemType.Helmet, attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0,
      details = ItemDetails.Passive(kind))

  private val cunning = Rune.Active(Skill.CunningStrike)
  private val healer  = Rune.Passive(PassiveKind.Healer)

  override def spec = suite("Руны")(
    test("сила: 1000 понимания — +100 %, 4000 — +200 %, 12 — +11 %; текст с запятой") {
      assertTrue(Rune.strengthPct(1000L) == 100.0 && Rune.strengthPct(4000L) == 200.0 && Rune.strengthPct(0L) == 0.0) &&
      assertTrue(math.abs(Rune.strengthPct(12L) - 10.954) < 0.01) &&
      assertTrue(Rune.mult(1000L) == 2.0 && Rune.strengthText(12L) == "+11,0%" && Rune.strengthText(300L) == "+54,8%")
    },
    test("очки за вещь по редкости: серые/белые/зелёные 1, синие 2, обе фиолетовые 3, оранжевые 5") {
      assertTrue(List(Rarity.Gray, Rarity.White, Rarity.Green).map(Rune.points) == List(1L, 1L, 1L)) &&
      assertTrue(Rune.points(Rarity.Blue) == 2L && Rune.points(Rarity.Purple) == 3L && Rune.points(Rarity.Violet) == 3L && Rune.points(Rarity.Orange) == 5L)
    },
    test("руна с вещи: оружие и нагрудник — боевая, остальное — пассивная; ключ туда и обратно") {
      val w = weapon(1L, Skill.CunningStrike)
      val h = helmet(2L, PassiveKind.Healer)
      val plain = w.copy(details = ItemDetails.Plain)
      assertTrue(Rune.of(w).contains(cunning) && Rune.of(h).contains(healer) && Rune.of(plain).isEmpty) &&
      assertTrue(Rune.byKey(cunning.key).contains(cunning) && Rune.byKey(healer.key).contains(healer) && Rune.byKey("x:Nope").isEmpty)
    },
    test("клейма: места 2 боевых и 4 пассивных, стоимость 500·(1+n) по всем, переклеймование не трогает понимание") {
      val d0 = RuneData.empty
      val d1 = d0.brand(cunning).brand(Rune.Active(Skill.Ram))
      val d2 = d1.brand(healer)
      val (d3, gained) = d2.deepen(cunning, 7L, cap = 30L)
      val d4 = d3.unbrand(cunning)
      assertTrue(d0.nextCost == 500L && d1.nextCost == 1500L && d2.nextCost == 2000L) &&
      assertTrue(!d1.hasRoomFor(Rune.Active(Skill.Bleeding)) && d1.hasRoomFor(healer) && d1.brand(cunning) == d1) &&
      assertTrue(gained == 7L && d3.understandingOf(cunning) == 7L) &&
      assertTrue(!d4.isBranded(cunning) && d4.understandingOf(cunning) == 7L && d4.nextCost == 1500L)
    },
    test("понимание не выше потолка: 30 на уровень; лишнее не берётся") {
      val (d1, g1) = RuneData.empty.deepen(cunning, 25L, Rune.cap(1L))
      val (d2, g2) = d1.deepen(cunning, 10L, Rune.cap(1L))
      val (d3, g3) = d2.deepen(cunning, 10L, Rune.cap(1L))
      assertTrue(Rune.cap(1L) == 30L && Rune.cap(10L) == 300L) &&
      assertTrue(g1 == 25L && g2 == 5L && d2.understandingOf(cunning) == 30L && g3 == 0L && d3 == d2)
    },
    test("понимание множит число пассивки — и с вещи, и с тела; шансы не выше 100") {
      val hero = TestFixtures.hero(UserId(1L)).copy(runes = RuneData.empty.brand(healer).copy(understanding = Map(healer.key -> 1000L)))
      val p = HeroPassives(Set(PassiveKind.Healer, PassiveKind.Stealthy), Map(PassiveKind.Healer -> 2.0, PassiveKind.Stealthy -> 9.0))
      assertTrue(hero.passives.kinds.contains(PassiveKind.Healer) && hero.passives.healMult == 1.2) &&
      assertTrue(p.healMult == 1.2 && p.battleEncounterFactor == 0.0) &&
      assertTrue(HeroPassives(Set(PassiveKind.Healer)).healMult == 1.1)
    },
    test("клеймо боевой руны — слот в бою с отрицательным id; та же руна на вещи — одна кнопка") {
      val base = TestFixtures.hero(UserId(1L))
      val branded = base.copy(runes = RuneData.empty.brand(cunning))
      val worn = branded.copy(equipment = base.equipment.copy(weapon = weapon(5L, Skill.CunningStrike)))
      assertTrue(branded.activeSkillSlots.map(s => (s.itemId, s.skill)) == List((Rune.bodySlotId(Skill.CunningStrike), Skill.CunningStrike))) &&
      assertTrue(Rune.isBodySlot(Rune.bodySlotId(Skill.CunningStrike)) && !Rune.isBodySlot(5L)) &&
      assertTrue(worn.activeSkillSlots.map(_.itemId) == List(5L)) &&
      assertTrue(worn.copy(runes = RuneData.empty.brand(cunning).copy(understanding = Map(cunning.key -> 1000L))).runeMult(Skill.CunningStrike) == 2.0)
    },
    test("хранение: пустой объект читается как пустые руны с настройкой по умолчанию; поля выживают туда и обратно") {
      val d = RuneData(List("CunningStrike"), List("Healer"), Map("s:CunningStrike" -> 12L), Set("Gray", "Blue"))
      assertTrue(io.circe.Json.obj().as[RuneData].toOption.contains(RuneData.empty)) &&
      assertTrue(RuneData.empty.burns(Rarity.Gray) && RuneData.empty.burns(Rarity.White) && !RuneData.empty.burns(Rarity.Green)) &&
      assertTrue(d.asJson.as[RuneData].toOption.contains(d)) &&
      assertTrue(d.toggleBurn(List(Rarity.Purple, Rarity.Violet)).burns(Rarity.Violet) && !d.toggleBurn(List(Rarity.Gray)).burns(Rarity.Gray))
    }
  )
}
