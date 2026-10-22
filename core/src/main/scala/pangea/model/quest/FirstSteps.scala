package pangea.model.quest

import pangea.model.hero.Hero
import pangea.model.home.HomeData

/** «Встать на ноги» — задание, которое герой ставит себе сам, очнувшись в
  * сторожке посреди лабиринта: окрепнуть, обзавестись логовом и разведать город.
  *
  * Копить для него ничего не нужно: каждый пункт проверяется по тому, что у
  * героя уже есть, — по уровню, по дому (любому, у Юноны Кинтслев) и по сданным
  * заданиям горожан. Взятым его отмечает `LoreData.firstSteps`. */
object FirstSteps {

  /** Окрепнуть — дойти до этого уровня. */
  val TargetLvl: Long = 5L

  /** Разведать город — выполнить вступительные задания горожан: каждое учит
    * одной механике и рассказывает свой кусок истории Кинэта. За заданиями
    * Казимира, Фета и Рахадима надо спускаться глубоко, а письмо Марисе и
    * мурлоки приходят сами, по счёту убитых, — эти в разведку не идут. */
  val CityQuests: List[NpcQuest] =
    List(NpcQuest.Innkeeper, NpcQuest.Richelieu, NpcQuest.Gustavo, NpcQuest.Priest, NpcQuest.Horn)

  /** Где герой по каждому пункту: окреп ли, есть ли логово, сколько заданий
    * горожан сдано. */
  final case class Progress(strong: Boolean, house: Boolean, cityDone: Int) {
    def city: Boolean = cityDone >= CityQuests.size
    def done: Boolean = strong && house && city
  }

  def progress(hero: Hero, quests: NpcQuests, home: HomeData = HomeData.empty): Progress =
    Progress(
      strong   = hero.lvl >= TargetLvl,
      house    = home.owns,
      cityDone = CityQuests.count(quests.isDone))
}
