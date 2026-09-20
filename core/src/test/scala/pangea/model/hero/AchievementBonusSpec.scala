package pangea.model.hero

import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Бонусы достижений к характеристикам доходят до всего, что считается от
  * эффективных базовых статов: карточки персонажа, урона, энергии. */
object AchievementBonusSpec extends ZIOSpecDefault {

  private val hero = TestFixtures.hero(UserId(1L)) // человек: расовые множители 1.0, статы 10/10/10/10

  override def spec = suite("Бонусы достижений")(
    test("«Зельевар I» +2 к интеллекту, «Любимец Мурлоков» +1 к ловкости, «Гроза Мурлоков» +1 к силе — в эффективных статах и карточке") {
      val all  = hero.withAchievement(Achievement.Brewer1).withAchievement(Achievement.MurlocFavorite).withAchievement(Achievement.MurlocBane)
      val base = hero.effectiveBaseStats(0L)
      val eff  = all.effectiveBaseStats(0L)
      assertTrue(eff.int == base.int + 2L && eff.agi == base.agi + 1L && eff.str == base.str + 1L && eff.vit == base.vit) &&
      assertTrue(all.getInfo(0L).contains(s"СИЛ ${base.str + 1}") && all.getInfo(0L).contains(s"ЛОВ ${base.agi + 1}") &&
                 all.getInfo(0L).contains(s"ИНТ ${base.int + 2}")) &&
      // ловкость и интеллект питают максимум энергии — он тоже растёт
      assertTrue(all.maxEnergy(0L) == hero.maxEnergy(0L) + 5L * 2L + 2L * 1L)
    },
    test("без достижений прибавок нет, повторная выдача не удваивает") {
      val twice = hero.withAchievement(Achievement.MurlocBane).withAchievement(Achievement.MurlocBane)
      assertTrue(Achievement.strBonus(hero) == 0L && Achievement.agiBonus(hero) == 0L && Achievement.intBonus(hero) == 0L) &&
      assertTrue(twice.achievements.count(_ == Achievement.MurlocBane.entryName) == 1 && Achievement.strBonus(twice) == 1L)
    }
  )
}
