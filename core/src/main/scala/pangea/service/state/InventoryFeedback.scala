package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.HeroId
import pangea.model.item.{Item, MaterialKind}
import pangea.repository.inventory.InventoryRepository
import zio.Task

/**
 * Общий фидбэк по сумке при получении предмета любым способом (лут, найденный
 * предмет, покупка, снятие экипировки). Держит формулировки в одном месте:
 *  - `freeSlotsLine` — «сколько свободных слотов осталось» после успешного получения;
 *  - `common.inventoryFull` (через `content.text`) — единое сообщение о переполнении;
 *  - `refusalLine` — почему вещь не легла: сумка полна или пыли уже сотня.
 */
object InventoryFeedback {

  /** Строка отказа для вещи, которая не поместилась. Пыль места не занимает,
    * поэтому её не приняли по своей причине — предел на вид (см.
    * `MaterialKind.MaxDustPerKind`), и говорить о переполненной сумке неверно. */
  def refusalLine(content: SceneContent, item: Item, storage: Boolean = false): String =
    item.dustKind match {
      case Some(kind) =>
        content.format(if (storage) "common.dustLimitHere" else "common.dustLimit",
          "name" -> kind.displayName, "n" -> MaterialKind.MaxDustPerKind.toString)
      case None => content.text("common.inventoryFull")
    }

  def freeSlotsLine(
    inventoryRepo: InventoryRepository,
    content:       SceneContent,
    heroId:        HeroId
  ): Task[String] =
    inventoryRepo.get(heroId)
      .mapError(e => new Throwable(e.toString))
      .map(inv => content.format("common.freeSlots",
        "free" -> inv.freeSlots.toString))
}
