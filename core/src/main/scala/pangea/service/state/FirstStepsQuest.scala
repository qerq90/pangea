package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.home.HomeData
import pangea.model.quest.{FirstSteps, NpcQuests}

/** Как задание «Встать на ноги» выглядит на экране (см. [[FirstSteps]]): пункты
  * с отметками — ✅ сделано, ⬜ ещё нет. */
object FirstStepsQuest {

  /** Пункты задания, по строке на каждый. */
  def objectives(content: SceneContent, hero: Hero, quests: NpcQuests, home: HomeData = HomeData.empty): String = {
    val p = FirstSteps.progress(hero, quests, home)
    def mark(ok: Boolean): String = if (ok) "✅" else "⬜"
    List(
      s"${mark(p.strong)} ${content.format("firstSteps.strong", "lvl" -> FirstSteps.TargetLvl.toString)}",
      s"${mark(p.house)} ${content.text("firstSteps.house")}",
      s"${mark(p.city)} ${content.format("firstSteps.city",
        "done" -> p.cityDone.toString, "total" -> FirstSteps.CityQuests.size.toString)}"
    ).mkString("\n")
  }

  /** Сообщение о том, что задание получено. */
  def taken(content: SceneContent, hero: Hero, quests: NpcQuests): String =
    content.format("firstSteps.taken",
      "title"      -> content.text("firstSteps.title"),
      "objectives" -> objectives(content, hero, quests))
}
