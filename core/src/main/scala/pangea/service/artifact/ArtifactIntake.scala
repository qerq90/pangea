package pangea.service.artifact

import pangea.engine.SceneContent
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Куда попадает только что выпавшая вещь. Камни сами летят в Ларец Азата,
 *  травы и отвары — в Живую сумку; нет артефакта или в нём нет места — всё как
 *  раньше, в сумку героя. */
sealed trait Intake
object Intake {
  /** Вещь забрал артефакт; `free` — сколько мест в нём осталось. */
  final case class ToArtifact(kind: ArtifactKind, free: Int) extends Intake
  case object ToInventory extends Intake
  /** Не влезло никуда (сумка полна или предел невесомого). */
  case object Refused extends Intake
}

object ArtifactIntake {

  /** Положить добычу: сперва артефакту, которому она по нраву, потом в сумку. */
  def accept(
    artifacts:     Option[ArtifactRepository],
    inventoryRepo: InventoryRepository,
    heroId:        HeroId,
    item:          Item
  ): Task[Intake] =
    artifacts match {
      case None       => toInventory(inventoryRepo, heroId, item)
      case Some(repo) => intoArtifact(repo, inventoryRepo, heroId, item)
    }

  private def intoArtifact(
    artifacts:     ArtifactRepository,
    inventoryRepo: InventoryRepository,
    heroId:        HeroId,
    item:          Item
  ): Task[Intake] =
    artifacts.get(heroId).either.flatMap {
      case Right(all) =>
        all.keeperFor(item) match {
          case Some(kind) =>
            artifacts.put(heroId, kind, item).either.flatMap {
              case Right(a) => ZIO.succeed(Intake.ToArtifact(kind, a.freeSlots))
              case Left(_)  => toInventory(inventoryRepo, heroId, item)
            }
          case None => toInventory(inventoryRepo, heroId, item)
        }
      case Left(_) => toInventory(inventoryRepo, heroId, item)
    }

  /** Строка игроку о том, что вещь ушла в артефакт и сколько там осталось мест. */
  def line(content: SceneContent, item: Item, kind: ArtifactKind, free: Int): String =
    content.format("artifact.intakeLine",
      "name" -> item.displayTitle, "artifact" -> kind.label, "free" -> free.toString)

  private def toInventory(inventoryRepo: InventoryRepository, heroId: HeroId, item: Item): Task[Intake] =
    inventoryRepo.addItem(heroId, item).as[Intake](Intake.ToInventory)
      .catchAll(_ => ZIO.succeed(Intake.Refused))
}
