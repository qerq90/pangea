package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.HeroId
import pangea.repository.inventory.InventoryRepository
import zio.Task

/**
 * Общий фидбэк по сумке при получении предмета любым способом (лут, найденный
 * предмет, покупка, снятие экипировки). Держит формулировки в одном месте:
 *  - `freeSlotsLine` — «сколько свободных слотов осталось» после успешного получения;
 *  - `common.inventoryFull` (через `content.text`) — единое сообщение о переполнении.
 */
object InventoryFeedback {

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
