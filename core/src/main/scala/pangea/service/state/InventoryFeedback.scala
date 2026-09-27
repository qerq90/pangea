package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.repository.inventory.InventoryRepository
import pangea.service.artifact.{ArtifactIntake, Intake}
import zio.{Task, ZIO}

/**
 * Общий фидбэк по сумке при получении предмета любым способом (лут, найденный
 * предмет, покупка, снятие экипировки). Держит формулировки в одном месте:
 *  - `freeSlotsLine` — «сколько свободных слотов осталось» после успешного получения;
 *  - `common.inventoryFull` (через `content.text`) — единое сообщение о переполнении;
 *  - `refusalLine` — почему вещь не легла: сумка полна или пыли уже сотня;
 *  - `intakeLine` — то же самое, но с учётом артефактов: вещь мог перехватить
 *    Ларец или Живая сумка, и тогда считать надо их места, а не сумкины.
 */
object InventoryFeedback {

  /** Строка отказа для вещи, которая не поместилась. Пыль места не занимает,
    * поэтому её не приняли по своей причине — предел на вид (см.
    * `MaterialKind.MaxDustPerKind`), и говорить о переполненной сумке неверно. */
  def refusalLine(content: SceneContent, item: Item, storage: Boolean = false): String =
    if (item.weightless)
      content.format(if (storage) "common.dustLimitHere" else "common.dustLimit",
        "name" -> item.name, "n" -> Item.HoardLimit.toString)
    else content.text("common.inventoryFull")

  /** Куда легла только что полученная вещь и сколько там осталось места.
    * Артефакты забирают добычу молча, и без этой строки игрок не понимает, куда
    * делась трава и сколько ещё влезет, — поэтому все, кто выдаёт вещь по
    * одной, говорят об этом одинаково. */
  def intakeLine(
    inventoryRepo: InventoryRepository,
    content:       SceneContent,
    heroId:        HeroId,
    item:          Item,
    intake:        Intake
  ): Task[String] = intake match {
    case Intake.ToArtifact(kind, free) => ZIO.succeed(ArtifactIntake.line(content, item, kind, free))
    case Intake.ToInventory            => freeSlotsLine(inventoryRepo, content, heroId)
    case Intake.Refused                =>
      freeSlotsLine(inventoryRepo, content, heroId).map(slots => s"${refusalLine(content, item)}\n$slots")
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
