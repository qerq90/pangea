package pangea.service.state.states.hero

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Knowledge
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{HerbLore, State, UserAction}
import zio.Task

/** «Знания» в меню персонажа: что герой умеет (см. [[Knowledge]]) и как узнал —
  * сам или по книге. Пока пусто — так и говорим. */
case class KnowledgeState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map("BackFromKnowledge" -> Target.Goto(StateType.HeroStats)),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.Knowledge) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Knowledge)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    HerbLore.readLore(heroDao, user.userId).flatMap { lore =>
      val known = Knowledge.values.filter(lore.knows).toList
      val body  =
        if (known.isEmpty) content.text("knowledge.empty")
        else known.map { k =>
          val how = if (lore.learnedAlone(k)) content.text("knowledge.selfTaught") else content.text("knowledge.fromBook")
          s"📖 ${k.title} ($how)\n${k.description}"
        }.mkString("\n\n")
      renderer.show(user, Screen(content.text("knowledge.header") + "\n\n" + body,
        List(content.choice("BackFromKnowledge", "knowledge.back"))))
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
