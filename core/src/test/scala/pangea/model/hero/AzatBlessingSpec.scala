package pangea.model.hero

import pangea.test.TestFixtures
import pangea.model.user.UserId
import zio.test._

/** Благословение Азата: суточные отдыхи, продление и строка в карточке героя. */
object AzatBlessingSpec extends ZIOSpecDefault {

  private val day  = 24L * 60L * 60L * 1000L
  private val noon = day / 2 // полдень первых суток эпохи

  override def spec = suite("Благословение Азата")(

    test("старая запись храма без новых полей читается, а не обнуляет купленное") {
      // Так выглядел azat_data до появления суточных отдыхов: поля
      // restsGrantedAt в записи нет. Производный декодер требовал его и ронял
      // разбор целиком — герой терял куб, заряды, благословение и отдыхи.
      val old = io.circe.parser.parse(
        """{"cube":"Active","cubeCharges":3,"cubeItems":[],"blessingUntil":123,"instantRests":7}"""
      ).toOption.get
      val parsed = old.as[AzatState].toOption.get
      assertTrue(parsed.cube == CubeStatus.Active) &&
      assertTrue(parsed.cubeCharges == 3) &&
      assertTrue(parsed.blessingUntil.contains(123L)) &&
      assertTrue(parsed.instantRests == 7) &&
      // отсутствующее поле берётся по умолчанию, остальное остаётся при герое
      assertTrue(parsed.restsGrantedAt.isEmpty) &&
      // и совсем пустой объект тоже читается — это чистое состояние храма
      assertTrue(io.circe.Json.obj().as[AzatState].toOption.contains(AzatState.empty))
    },

    test("каждые сутки в 00:00 добавляется 50 быстрых отдыхов") {
      val azat = AzatState(blessingUntil = Some(noon + 10 * day), restsGrantedAt = Some(noon))
      val next = azat.withDailyRests(noon + day)      // прошла одна полночь
      val week = azat.withDailyRests(noon + 7 * day)  // прошло семь
      assertTrue(AzatState.BlessingDailyRests == 50) &&
      assertTrue(next.instantRests == 50) &&
      // за неделю офлайна отдыхи не пропали — доначислились все разом
      assertTrue(week.instantRests == 350)
    },

    test("до первой полуночи и без благословения не начисляется ничего") {
      val blessed = AzatState(blessingUntil = Some(noon + 10 * day), restsGrantedAt = Some(noon))
      val plain   = AzatState(instantRests = 7)
      assertTrue(blessed.withDailyRests(noon + day / 4) == blessed) && // те же сутки
      assertTrue(plain.withDailyRests(noon + 30 * day) == plain)       // благословения нет
    },

    test("после конца благословения сутки больше не капают") {
      // Благословение кончилось через двое суток, а зашли мы через десять.
      val azat = AzatState(blessingUntil = Some(noon + 2 * day), restsGrantedAt = Some(noon))
      val got  = azat.withDailyRests(noon + 10 * day)
      assertTrue(got.instantRests == 100) // ровно две полуночи, а не десять
    },

    test("повторное начисление не задваивается: отметка сдвигается") {
      val azat  = AzatState(blessingUntil = Some(noon + 10 * day), restsGrantedAt = Some(noon))
      val once  = azat.withDailyRests(noon + day)
      val twice = once.withDailyRests(noon + day)
      assertTrue(once.instantRests == 50) && assertTrue(twice == once)
    },

    test("благословение одно: повторная покупка продлевает срок, а не заводит второе") {
      val active   = AzatState(blessingUntil = Some(noon + 3 * day), instantRests = 10)
      // Так продлевает храм: от остатка, а не от «сейчас».
      val base     = active.blessingUntil.filter(_ > noon).getOrElse(noon)
      val extended = active.copy(
        blessingUntil = Some(base + AzatState.BlessingDurationMs),
        instantRests  = active.instantRests + AzatState.BlessingInstantRests)
      assertTrue(extended.blessingUntil.contains(noon + 3 * day + AzatState.BlessingDurationMs)) &&
      assertTrue(extended.instantRests == 260) &&
      // срок один и тот же — второго благословения не появляется
      assertTrue(extended.blessingActive(noon + 9 * day))
    },

    test("сутки закрываются в 00:00 по Москве, а не по UTC") {
      // 21:30 UTC — это уже 00:30 следующего дня в Москве.
      val eveningUtc = 21L * 3600000L + 1800000L
      val azat = AzatState(blessingUntil = Some(eveningUtc + 30 * day), restsGrantedAt = Some(eveningUtc - 3600000L))
      assertTrue(AzatState.MoscowOffsetMs == 3L * 60L * 60L * 1000L) &&
      // по UTC полночь ещё не наступила, по МСК — уже прошла
      assertTrue(azat.withDailyRests(eveningUtc).instantRests == 50) &&
      assertTrue(AzatState.midnightsBetween(eveningUtc - 3600000L, eveningUtc) == 1L)
    },

    test("в карточке персонажа виден остаток быстрых отдыхов") {
      val hero = TestFixtures.hero(UserId(1L))
      val card = hero.getInfo(0L, blessed = true, instantRests = 137)
      // Отдыхи остаются и после конца благословения — показываем их и без него.
      val expired = hero.getInfo(0L, blessed = false, instantRests = 5)
      assertTrue(card.contains("⚡ Быстрых отдыхов: 137")) &&
      assertTrue(expired.contains("⚡ Быстрых отдыхов: 5")) &&
      assertTrue(!expired.contains("Благословение")) &&
      // без зарядов строки нет вовсе
      assertTrue(!hero.getInfo(0L).contains("Быстрых отдыхов"))
    },

    test("в карточке персонажа строка про благословение стоит под опытом") {
      val hero    = TestFixtures.hero(UserId(1L))
      val blessed = hero.getInfo(0L, blessed = true)
      val plain   = hero.getInfo(0L)
      val lines   = blessed.linesIterator.toList
      val expIdx  = lines.indexWhere(_.contains("опыта"))
      assertTrue(!plain.contains("Благословение")) &&
      assertTrue(lines(expIdx + 1).contains("Благословение Активно")) &&
      // между ним и статами — пустая строка (отступ)
      assertTrue(lines(expIdx + 2).trim.isEmpty) &&
      assertTrue(lines(expIdx + 3).contains("СИЛ"))
    }
  )
}
