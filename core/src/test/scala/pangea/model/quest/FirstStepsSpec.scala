package pangea.model.quest

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.LoreData
import pangea.model.home.{HomeData, House}
import pangea.model.user.UserId
import pangea.service.state.FirstStepsQuest
import pangea.test.TestFixtures
import zio.ZIO
import zio.test._

/** «Встать на ноги»: окрепнуть, обзавестись логовом, разведать город. Пункты
  * считаются по тому, что у героя уже есть. */
object FirstStepsSpec extends ZIOSpecDefault {

  private val hero = TestFixtures.hero(UserId(1L))

  private def done(qs: NpcQuest*): NpcQuests = qs.foldLeft(NpcQuests.empty)(_ finish _)

  override def spec = suite("Встать на ноги")(

    test("окреп — с пятого уровня; разведка — по пяти вступительным заданиям горожан") {
      val weak   = FirstSteps.progress(hero.copy(lvl = 4L), done(NpcQuest.Innkeeper, NpcQuest.Gustavo))
      val strong = FirstSteps.progress(hero.copy(lvl = 5L), NpcQuests.empty)
      // задания тех, кто просит спуститься глубоко, и цепочки по счёту убитых в разведку не идут
      val deep   = FirstSteps.progress(hero, done(NpcQuest.Kazimir, NpcQuest.Fet, NpcQuest.Rakhadim, NpcQuest.Marisa, NpcQuest.Murloc))
      assertTrue(!weak.strong && weak.cityDone == 2 && !weak.city) &&
      assertTrue(strong.strong && strong.cityDone == 0) &&
      assertTrue(deep.cityDone == 0) &&
      assertTrue(FirstSteps.CityQuests == List(NpcQuest.Innkeeper, NpcQuest.Richelieu, NpcQuest.Gustavo,
                                               NpcQuest.Priest, NpcQuest.Horn))
    },

    test("без дома задание не закрывается; любой дом у Юноны закрывает пункт «логово»") {
      val all    = FirstSteps.progress(hero.copy(lvl = 30L), done(FirstSteps.CityQuests: _*))
      val housed = FirstSteps.progress(hero.copy(lvl = 30L), done(FirstSteps.CityQuests: _*), HomeData(tier = 1))
      assertTrue(all.strong && all.city && !all.house && !all.done) &&
      assertTrue(housed.house && housed.done) &&
      assertTrue(House.values.forall(h => FirstSteps.progress(hero, NpcQuests.empty, HomeData(tier = h.tier)).house))
    },

    test("на экране — три пункта с отметками") {
      for {
        content <- ZIO.attempt(SceneContent.load())
        text     = FirstStepsQuest.objectives(content, hero.copy(lvl = 6L), done(NpcQuest.Priest))
      } yield assertTrue(text.linesIterator.toList == List(
                "✅ Окрепнуть — дойти до 5 уровня",
                "⬜ Обзавестись логовом — купить дом",
                "⬜ Разведать город — выполнить вступительные задания горожан (1/5)"))
    },

    test("отметка о взятом задании живёт в лоре, и старый лор читается без неё") {
      val old = LoreData(metJoe = true).asJson.mapObject(_.remove("firstSteps"))
      assertTrue(old.as[LoreData].map(_.firstSteps) == Right(false)) &&
      assertTrue(LoreData(firstSteps = true).asJson.as[LoreData].map(_.firstSteps) == Right(true))
    }
  )
}
