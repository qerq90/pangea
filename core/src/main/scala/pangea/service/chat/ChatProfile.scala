package pangea.service.chat

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.ItemType
import pangea.service.state.states.EquipmentState

/** Что бот пишет в общую беседу в ответ на «Мой профиль» и «Моё снаряжение».
  *
  * Карточка собирается ровно из того же, что игрок видит у себя: профиль — из
  * [[Hero.getInfo]] и строки о травмах, как на экране «Персонаж»; снаряжение —
  * из тех же слотов, что и в «Снаряжении», только без характеристик. Там, где
  * пусто, так и написано: беседа должна видеть и дырки в доспехе.
  */
object ChatProfile {

  def profileHeader(name: String): String = s"👤 $name"
  def gearHeader(name: String): String    = s"🛡 $name — снаряжение"

  /** Пустой слот. */
  val Empty: String = "—"

  /** Карточка героя — то же, что по кнопке «Персонаж», с именем в заголовке. */
  def profile(name: String, hero: Hero, nowMs: Long, blessed: Boolean,
              instantRests: Int, content: SceneContent): String = {
    val trauma = hero.traumaRemainingText(nowMs).map { remaining =>
      val names = hero.activeTraumas(nowMs).map(_.name)
      "\n" + content.format("heroStats.traumaActive",
        "traumaNames" -> (if (names.isEmpty) "Травмы" else names.mkString(", ")),
        "remaining"   -> remaining)
    }.getOrElse("")
    s"${profileHeader(name)}\n${hero.getInfo(nowMs, blessed, instantRests)}$trauma"
  }

  /** Слоты снаряжения: что надето и где пусто. Характеристики не показываем —
    * беседе интересно, во что игрок одет, а не на сколько у него точность. */
  def gear(name: String, hero: Hero): String = {
    val lines = EquipmentState.slots.map { slot =>
      val item = slot.get(hero.equipment)
      val what = if (item.itemType == ItemType.NoItem) Empty else item.displayTitle
      s"${slot.name}: $what"
    }
    s"${gearHeader(name)}\n${lines.mkString("\n")}"
  }
}
