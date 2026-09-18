package pangea.model.quest

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/** Сюжетные задания горожан — по одному на каждого, кто встречает новичка в
  * Кинэте. Каждое учит одной механике и рассказывает свой кусок истории города.
  * Ключ — имя секции в `scenes.yaml` (`npcQuests.<key>`). */
sealed abstract class NpcQuest(val key: String) extends EnumEntry

object NpcQuest extends Enum[NpcQuest] {
  val values: IndexedSeq[NpcQuest] = findValues

  /** Трактирщик, «Плата за первую кружку»: принести любой трофей. */
  case object Innkeeper extends NpcQuest("innkeeper")

  /** Ришелье, «Товар с того света»: продать три серых вещи через «Продать хлам». */
  case object Richelieu extends NpcQuest("richelieu")

  /** Густаво, «Подопытный»: выпить зелье, победить троих, пока оно действует. */
  case object Gustavo extends NpcQuest("gustavo")

  /** Жрец, «То, что не пропадает»: показать найденный камень. */
  case object Priest extends NpcQuest("priest")

  /** Мастер Горн, «Ржавая вилка»: набрать сто репутации, получить улучшение даром. */
  case object Horn extends NpcQuest("horn")

  /** «Письмо Марисе»: письмо с пятидесятого убитого, поиски адресата, тайник
    * Кельвина и коллектор. Шаги: 1 — письмо в руках, 2 — спросил Трактирщика,
    * 3 — нашёл Марису (ждёт в Портовом квартале); `bonus` — письмо вскрыто,
    * карта на руках. */
  case object Marisa extends NpcQuest("marisa")

  /** С какого по счёту убитого моба выпадает письмо Марисе. */
  val MarisaLetterKill: Long = 50L

  /** «Деревня Мурлоков»: после сотого убитого к герою выходит старейшина
    * Мрачноглаз и просит принести в деревню снаряжение. Шаги: 1 — сотня
    * набрана, старейшина ещё не вышел; 2 — карта на руках, `counter` — сколько
    * снаряжения сдано; 3 — двадцать сдано, но клинок не влез в сумку и ждёт в
    * деревне. Закрывается помощью (клинок, «Любимец Мурлоков») или налётом
    * («Гроза Мурлоков»). */
  case object Murloc extends NpcQuest("murloc")

  /** С какого по счёту убитого к герою выходит старейшина мурлоков. */
  val MurlocElderKill: Long = 100L

  /** Сколько снаряжения просит старейшина. */
  val MurlocGearGoal: Long = 20L

  def byKey(key: String): Option[NpcQuest] = values.find(_.key == key)

  /** Сколько репутации Горн просит набрать трофеями. */
  val HornReputationGoal: Long = 100L

  /** Сколько побед под зельем нужно Густаво. */
  val GustavoVictoriesGoal: Long = 3L

  /** Сколько серых вещей Ришелье хочет купить разом. */
  val RichelieuGrayGoal: Int = 3

  /** Опыт за задание — как за столько боёв на текущем этаже. */
  val ExpPerDungeonLevel: Long = 5L
}

/** Ход одного задания. `step` — 0, пока не взято, дальше номер текущего шага;
  * `counter` — что копится на шаге (победы, репутация); `since` — отметка
  * времени начала шага, если шагу важно время; `bonus` — разовая поблажка от
  * NPC (Густаво наливает ещё одно бесплатное зелье); `done` — выполнено. */
final case class NpcQuestProgress(
  step:    Int     = 0,
  counter: Long    = 0L,
  since:   Long    = 0L,
  bonus:   Boolean = false,
  done:    Boolean = false
) {
  def taken: Boolean  = step > 0 && !done
  def onStep(n: Int): Boolean = taken && step == n
}

object NpcQuestProgress {
  val empty: NpcQuestProgress = NpcQuestProgress()

  implicit val encoder: Encoder[NpcQuestProgress] = (p: NpcQuestProgress) => Json.obj(
    "step"    -> p.step.asJson,
    "counter" -> p.counter.asJson,
    "since"   -> p.since.asJson,
    "bonus"   -> p.bonus.asJson,
    "done"    -> p.done.asJson
  )

  implicit val decoder: Decoder[NpcQuestProgress] = (c: HCursor) =>
    for {
      step    <- c.getOrElse[Int]("step")(0)
      counter <- c.getOrElse[Long]("counter")(0L)
      since   <- c.getOrElse[Long]("since")(0L)
      bonus   <- c.getOrElse[Boolean]("bonus")(false)
      done    <- c.getOrElse[Boolean]("done")(false)
    } yield NpcQuestProgress(step, counter, since, bonus, done)
}

/** Все сюжетные задания героя и знания, которые они дают. Durable, лежит в
  * `heroes.npc_quests`. `recipes` — рецепты куба, которые герою уже объяснили
  * (пока только запись; показ в меню куба — следующим шагом). */
final case class NpcQuests(
  quests:  Map[String, NpcQuestProgress] = Map.empty,
  recipes: List[String]                  = Nil
) {
  def of(q: NpcQuest): NpcQuestProgress = quests.getOrElse(q.key, NpcQuestProgress.empty)

  def updated(q: NpcQuest, p: NpcQuestProgress): NpcQuests = copy(quests = quests.updated(q.key, p))

  def update(q: NpcQuest)(f: NpcQuestProgress => NpcQuestProgress): NpcQuests = updated(q, f(of(q)))

  def isDone(q: NpcQuest): Boolean = of(q).done

  def isTaken(q: NpcQuest): Boolean = of(q).taken

  def onStep(q: NpcQuest, n: Int): Boolean = of(q).onStep(n)

  /** Взять задание: первый шаг, счётчики с нуля. */
  def take(q: NpcQuest, now: Long): NpcQuests = updated(q, NpcQuestProgress(step = 1, since = now))

  /** Закрыть задание. */
  def finish(q: NpcQuest): NpcQuests = update(q)(_.copy(done = true))

  def withRecipe(recipe: String): NpcQuests =
    if (recipes.contains(recipe)) this else copy(recipes = recipes :+ recipe)
}

object NpcQuests {
  val empty: NpcQuests = NpcQuests()

  /** Рецепт куба «три горсти пыли → надколотый камень», который открывает Жрец. */
  val DustAssemblyRecipe: String = "dustAssembly"

  implicit val encoder: Encoder[NpcQuests] = (q: NpcQuests) => Json.obj(
    "quests"  -> q.quests.asJson,
    "recipes" -> q.recipes.asJson
  )

  /** Декодер рукописный, с запасными значениями: новое поле не должно ронять
    * разбор уже сохранённых заданий (см. память о производных декодерах). */
  implicit val decoder: Decoder[NpcQuests] = (c: HCursor) =>
    for {
      quests  <- c.getOrElse[Map[String, NpcQuestProgress]]("quests")(Map.empty)
      recipes <- c.getOrElse[List[String]]("recipes")(Nil)
    } yield NpcQuests(quests, recipes)
}
