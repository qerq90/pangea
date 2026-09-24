package pangea.service.state.states.artifact

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.artifact.{Artifact, ArtifactKind}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/** «Рюкзак» из меню персонажа: сама сумка и сборные артефакты из Лавки Фета.
 *  Кнопки Ларца и Живой сумки появляются только у того, кто их купил. */
case class BackpackState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  artifacts:     ArtifactRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "OpenBag"        -> Target.Goto(StateType.Inventory),
      "OpenCasket"     -> Target.Goto(StateType.Casket),
      "OpenLivingBag"  -> Target.Goto(StateType.LivingBag),
      "BackFromBackpack" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.Backpack) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      all  <- artifacts.get(hero.id).mapError(asThrowable)
      casket = all.of(ArtifactKind.Casket)
      bag    = all.of(ArtifactKind.LivingBag)
      lines  = content.format("backpack.bagLine", "free" -> inv.freeSlots.toString, "max" -> inv.maxItems.toString) ::
                 List(line(ArtifactKind.Casket, casket), line(ArtifactKind.LivingBag, bag)).flatten
      choices = List(
        Some(Choice("OpenBag", content.text("backpack.bagLabel"), row = Some(0))),
        Option.when(casket.owned)(Choice("OpenCasket", content.text("artifact.casket.title"), row = Some(1))),
        Option.when(bag.owned)(Choice("OpenLivingBag", content.text("artifact.bag.title"), row = Some(1))),
        Some(Choice("BackFromBackpack", content.text("backpack.back"), color = ChoiceColor.Negative, row = Some(2)))
      ).flatten
      _ <- renderer.show(user, Screen(content.text("backpack.title") + "\n" + lines.mkString("\n"), choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def line(kind: ArtifactKind, a: Artifact): Option[String] =
    Option.when(a.owned)(content.format("backpack.artifactLine",
      "title"    -> content.text(s"artifact.${kind.key}.title"),
      "items"    -> a.occupied.toString,
      "capacity" -> a.capacity.toString,
      "charges"  -> a.charges.toString))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
