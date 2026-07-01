package pangea.service.state.states.registration

import io.circe.Json
import io.circe.jawn.decode
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Beat, Branch, Choice, Journal, Narrative, Players, Renderer, SceneContent, Screen, Target}
import pangea.model.GameEvent
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.state.StateType.Registration
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class RegistrationState(
  players:       Players,
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  journal:       Journal,
  content:       SceneContent
) extends State {

  override def enter(user: User, renderer: Renderer): Task[Unit] = ZIO.unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private lazy val travelNarrative: Narrative = {
    val bs = content.beats("registration.travel").map {
      case ("Travel7", beat) =>
        "Travel7" -> Beat(
          beat.text,
          user => players.getDisplayName(user).map(name => List(content.choice("Travel8", "registration.nameLabel", "name" -> name)))
        )
      case other => other
    }
    new Narrative(bs)
  }

  private lazy val branch: Branch = new Branch(
    routes = Map(
      "EndOfTravel"     -> Target.Run { (user, _, renderer)  => endTravel(user, renderer) },
      "Travel"          -> Target.Run { (user, ua, renderer)  => updateRace(user, ua, renderer) },
      "RaceDescription" -> Target.Run { (user, ua, renderer)  => getRaceDescription(user, ua.text, renderer) },
      "Race"            -> Target.Run { (user, _, renderer)   => getRace(user, renderer) }
    ) ++ travelNarrative.toRoutes(Registration),
    fallback = Target.Run { (user, _, renderer) => showWelcome(user, renderer) }
  )

  private def endTravel(user: User, renderer: Renderer): Task[StateType] =
    for {
      _    <- renderer.show(user, Screen(content.text("registration.endTravel"), Nil))
      hero <- heroDao.getHeroByUserId(user.userId)
      _    <- ZIO.whenCase(hero) { case Some(h) =>
                 ZIO.foreachDiscard(RegistrationState.starterItems) { item =>
                   itemRepo.persist(h.id, item)
                     .flatMap(itemWithId => inventoryRepo.addItem(h.id, itemWithId))
                     .orElse(ZIO.unit)
                 } *>
                 renderer.show(user, Screen(content.text("registration.startingEquipment"), Nil))
               }
    } yield StateType.Dungeon

  private def updateRace(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      race <- ZIO.fromEither(decode[Race](ua.payload.get))
      _    <- heroDao.updateRace(user.userId, race)
      _    <- journal.append(GameEvent(user.userId, "race_selected",
                Json.obj("race" -> race.entryName.asJson)))
      _    <- renderer.show(user, content.screen("registration.preTravel"))
    } yield Registration

  private lazy val raceChoices: List[Choice] =
    Race.values.toList.map(r => Choice("RaceDescription", r.toString))

  private def getRaceDescription(user: User, raceName: String, renderer: Renderer): Task[StateType] =
    Race.withNameOption(raceName) match {
      case Some(r) =>
        val choices = List(
          content.choice("Travel", "registration.raceDescription.confirmLabel").copy(data = Map("race" -> r.entryName)),
          content.choice("Race", "registration.raceDescription.backLabel")
        )
        renderer.show(user, Screen(r.description, choices)).as(Registration)
      case None =>
        renderer.show(user, Screen(content.text("registration.raceSelect"), raceChoices)).as(Registration)
    }

  private def getRace(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("registration.raceSelect"), raceChoices))
      .as(Registration)

  private def showWelcome(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, content.screen("registration.welcome"))
      .as(Registration)
}

object RegistrationState {
  val starterItems: List[Item] = List(
    Item(-1L,  "Меч новобранца",            1L, Rarity.Gray, ItemType.Weapon,
      attack = 1, accuracy = 1, concentration = 0, armor = 0, defence = 0, evasion = 0),
    Item(-2L,  "Фляга начинающего исследователя", 1L, Rarity.Gray, ItemType.Flask,
      attack = 0, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0,
      flaskEffect = Some(pangea.model.item.FlaskEffect.HealPercent(25)),
      charges     = Some(8),
      maxCharges  = Some(8))
  )
}
