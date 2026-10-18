package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, LoreData}
import pangea.model.quest.{BoardData, DailyNpc, DailyQuests, DailyRates, FirstSteps, NpcQuest, NpcQuests}
import pangea.service.state.states.guild.QuestBoardState
import zio.Task

/** «📜 Задания» из «Персонажа»: всё, что у героя сейчас в работе, — одним
  * сообщением. Сюжетное «Встать на ноги», задания горожан на своём шаге,
  * сегодняшние поручения и объявления с доски. Только читает, ничего не пишет. */
object QuestJournal {

  def text(heroDao: HeroDao, content: SceneContent, hero: Hero, nowMs: Long): Task[String] =
    for {
      lore   <- heroDao.readLoreData(hero.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
      quests <- NpcQuestLog.load(heroDao, hero.userId)
      daily  <- DailyQuestLog.load(heroDao, hero.userId)
      board  <- heroDao.readQuestData(hero.userId).map(_.flatMap(_.as[BoardData].toOption))
    } yield render(content, hero, lore, quests, daily, board, nowMs)

  /** Разделы — через пустую строку: сюжет, горожане, поручения, доска. Пусто
    * везде — так и говорим. */
  def render(content: SceneContent, hero: Hero, lore: LoreData, quests: NpcQuests, daily: DailyQuests,
             board: Option[BoardData], nowMs: Long): String = {
    val story = Option.when(lore.firstSteps && !FirstSteps.progress(hero, quests).done)(
      content.format("journal.story",
        "title"      -> content.text("firstSteps.title"),
        "objectives" -> FirstStepsQuest.objectives(content, hero, quests))).toList
    val npcs = NpcQuest.values.toList.filter(quests.isTaken).map(npcLine(content, _, quests))
    val today = DailyRates.dayOf(nowMs)
    val dailies = DailyNpc.values.toList.flatMap { npc =>
      daily.today(npc, today).filter(t => t.taken && !t.done).map { t =>
        content.format("journal.daily",
          "npc"  -> content.text(s"journal.dailyNpc.${npc.key}"),
          "line" -> content.format(s"daily.${npc.key}.tasks.${t.kind.key}.${if (t.ready) "ready" else "active"}",
                      "goal" -> t.need.toString, "have" -> t.progress.toString, "what" -> t.what))
      }
    }
    // Доска переписывается по понедельникам вместе со взятым, и перерос раздел —
    // тоже новая: со старой доски в работе ничего не числится.
    val boards = board.filter(_.fresh(nowMs, hero.lvl)).toList.flatMap { data =>
      data.slots.filter(_.taken).map { slot =>
        content.format("journal.board",
          "board" -> data.board.title,
          "what"  -> QuestBoardState.ask(content, slot),
          "state" -> content.text(if (slot.ready) "journal.boardReady" else "journal.boardActive"))
      }
    }
    val sections = List(story, npcs, dailies, boards).filter(_.nonEmpty).map(_.mkString("\n"))
    if (sections.isEmpty) content.text("journal.empty")
    else content.text("journal.header") + "\n\n" + sections.mkString("\n\n")
  }

  /** Задание горожанина: название и что делать сейчас — по шагу, со счётчиком,
    * где он есть. Шаг, которому нет строки, задание не прячет: тогда остаётся
    * одно название. */
  private def npcLine(content: SceneContent, q: NpcQuest, quests: NpcQuests): String = {
    val p     = quests.of(q)
    val base  = s"journal.quests.${q.key}"
    val steps = content.list(s"$base.steps")
    val step  = steps.lift(p.step - 1).map(_.replace("{counter}", p.counter.toString))
    // Вскрытое письмо Марисе — это и карта к тайнику: можно идти, не дожидаясь Марисы.
    val extra = Option.when(q == NpcQuest.Marisa && p.bonus && p.step < 3)(content.text(s"$base.map"))
    content.format("journal.npc",
      "title" -> content.text(s"$base.title"),
      "step"  -> (step.toList ++ extra).mkString(" "))
  }
}
