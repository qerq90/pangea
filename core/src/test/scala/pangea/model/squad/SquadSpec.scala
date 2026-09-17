package pangea.model.squad

import io.circe.syntax.EncoderOps
import zio.test._

/** Отряд: позиции 1..4 на героя и союзников, найм на первую свободную,
  * перестановки (с союзником — меняются, с героем — герой на прежнюю),
  * отлучка по свитку на сутки. */
object SquadSpec extends ZIOSpecDefault {

  private val lvl = 10L

  override def spec = suite("Squad")(

    test("статы союзника — ставка на уровень героя; отваров за найм — уровень на пять, но не меньше одного") {
      val h = AllyKind.Human.stats(lvl)
      val g = AllyKind.Gnome.stats(lvl)
      assertTrue(h.hp == 1250L && h.armor == 1500L && h.atk == 200L && h.energy == 1000L) &&
      assertTrue(h.accuracy == 1000L && h.defence == 400L && h.evasion == 1000L) &&
      assertTrue(g.hp == 800L && g.armor == 1750L && g.defence == 500L && g.evasion == 600L) &&
      assertTrue(AllyKind.Human.energyRegen(lvl) == 70L) &&
      assertTrue(AllyKind.brewsFor(1L) == 1L && AllyKind.brewsFor(4L) == 1L && AllyKind.brewsFor(5L) == 1L &&
                 AllyKind.brewsFor(10L) == 2L && AllyKind.brewsFor(23L) == 4L) &&
      assertTrue(AllyKind.Human.element == pangea.model.battle.Element.Fire &&
                 AllyKind.Murloc.element == pangea.model.battle.Element.Lightning &&
                 AllyKind.Gnome.element == pangea.model.battle.Element.Cold)
    },

    test("найм: на первую свободную позицию, здоровым; повторно — нет; на полный отряд — нет") {
      val s1 = Squad.empty.hire(AllyKind.Human, lvl, 0L)
      val s2 = s1.hire(AllyKind.Murloc, lvl, 0L)
      val s3 = s2.hire(AllyKind.Gnome, lvl, 0L)
      val s4 = s3.hire(AllyKind.Human, lvl, 0L)
      assertTrue(s1.allyAt(2).exists(a => a.kind == AllyKind.Human && a.hp == 1250L && a.armor == 1500L && a.energy == 1000L)) &&
      assertTrue(s2.allyAt(3).exists(_.kind == AllyKind.Murloc) && s3.allyAt(4).exists(_.kind == AllyKind.Gnome)) &&
      assertTrue(s4 == s3 && s3.freePosition.isEmpty) &&
      assertTrue(s3.inOrder.map(_.position) == List(2, 3, 4))
    },

    test("герой на позиции 3: наём обходит его место") {
      val s = Squad(heroPos = 3).hire(AllyKind.Human, lvl, 0L).hire(AllyKind.Murloc, lvl, 0L).hire(AllyKind.Gnome, lvl, 0L)
      assertTrue(s.inOrder.map(a => a.kind -> a.position) == List(AllyKind.Human -> 1, AllyKind.Murloc -> 2, AllyKind.Gnome -> 4))
    },

    test("перестановка: на пустое — просто; на союзника — меняются; на героя — герой встаёт на прежнюю") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 0L).hire(AllyKind.Murloc, lvl, 0L) // герой 1, Йорген 2, Плюх 3
      val toFree = s.move(AllyKind.Human, 4)
      val swap   = s.move(AllyKind.Human, 3)
      val toHero = s.move(AllyKind.Murloc, 1)
      assertTrue(toFree.allyAt(4).exists(_.kind == AllyKind.Human) && toFree.allyAt(2).isEmpty) &&
      assertTrue(swap.allyAt(3).exists(_.kind == AllyKind.Human) && swap.allyAt(2).exists(_.kind == AllyKind.Murloc)) &&
      assertTrue(toHero.heroPos == 3 && toHero.allyAt(1).exists(_.kind == AllyKind.Murloc)) &&
      assertTrue(s.move(AllyKind.Human, 2) == s && s.move(AllyKind.Human, 5) == s && s.move(AllyKind.Gnome, 4) == s)
    },

    test("герой шагает на позицию союзника — тот встаёт на его прежнюю") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 0L)
      val m = s.moveHero(2)
      assertTrue(m.heroPos == 2 && m.allyAt(1).exists(_.kind == AllyKind.Human)) &&
      assertTrue(s.moveHero(1) == s && s.moveHero(9) == s)
    },

    test("свиток: из отряда вон, сутки в отлучке, потом возвращается — и встречается один раз") {
      val now = 1_000_000L
      val s   = Squad.empty.hire(AllyKind.Human, lvl, 0L).sentAway(AllyKind.Human, now)
      assertTrue(!s.has(AllyKind.Human) && s.isAway(AllyKind.Human, now) && s.isAway(AllyKind.Human, now + AllyRates.AwayMs - 1L)) &&
      assertTrue(!s.isAway(AllyKind.Human, now + AllyRates.AwayMs)) &&
      assertTrue(s.returned(now).isEmpty && s.returned(now + AllyRates.AwayMs) == List(AllyKind.Human)) &&
      assertTrue(s.welcomeBack(AllyKind.Human).returned(now + AllyRates.AwayMs).isEmpty)
    },

    test("отдых восстанавливает всех; текущее не выше потолков") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 0L).update(AllyKind.Human)(_.copy(hp = 1L, armor = 0L, energy = 5L))
      val r = s.restored(lvl)
      val c = Ally(AllyKind.Human, 2, 99999L, -5L, 99999L).clamped(lvl)
      assertTrue(r.allyAt(2).exists(a => a.hp == 1250L && a.armor == 1500L && a.energy == 1000L)) &&
      assertTrue(c.hp == 1250L && c.armor == 0L && c.energy == 1000L)
    },

    test("найм на 12 часов: отработавший уходит из отряда и садится за стол через 12 часов; реплики о свитке ему не положено") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 1000L)
      val (same, none) = s.expire(1000L + AllyRates.HireMs - 1L)
      val (gone, who)  = s.expire(1000L + AllyRates.HireMs)
      val back = 1000L + AllyRates.HireMs + AllyRates.OffDutyMs
      assertTrue(s.allyAt(2).exists(_.hiredUntil == 1000L + AllyRates.HireMs)) &&
      assertTrue(same == s && none.isEmpty) &&
      assertTrue(who == List(AllyKind.Human) && !gone.has(AllyKind.Human)) &&
      assertTrue(gone.isAway(AllyKind.Human, back - 1L) && !gone.isAway(AllyKind.Human, back)) &&
      assertTrue(gone.returned(back).isEmpty && gone.welcomeBack(AllyKind.Human).offDuty.isEmpty) &&
      assertTrue(AllyRates.HireMs == 12L * 60L * 60L * 1000L && AllyRates.OffDutyMs == AllyRates.HireMs)
    },

    test("отряд переживает сериализацию, а пустая запись читается как пустой отряд") {
      val s    = Squad(heroPos = 2, allies = List(Ally(AllyKind.Gnome, 1, 10L, 20L, 30L)), away = Map("Human" -> 5L))
      val back = s.asJson.as[Squad].toOption
      val old  = io.circe.Json.obj().as[Squad].toOption
      assertTrue(back.contains(s)) && assertTrue(old.contains(Squad.empty))
    }
  )
}
