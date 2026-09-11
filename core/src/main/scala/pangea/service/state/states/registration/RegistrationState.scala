package pangea.service.state.states.registration

import io.circe.Json
import io.circe.jawn.decode
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Beat, Branch, Choice, Journal, Narrative, Players, Renderer, SceneContent, Screen, Target}
import pangea.model.GameEvent
import pangea.model.item.{FlaskEffect, Item, ItemDetails, ItemType, Rarity}
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

  /** Ветки пролога: какой из четырёх смертей игрок выбрал в «Кинжале в спину».
    * Ключ — суффикс битов `Pick_*` / `Wake_*` в scenes.yaml. */
  private val branches: Set[String] = Set("Gnome", "Orc", "Elf", "Human")

  /** Пролог из scenes.yaml (`registration.prologue`). Два бита переопределены:
    * `Pick_<ветка>` запоминает выбранную смерть в scene_data, а `Remember` строит
    * единственную кнопку «Всё равно помереть» по этой ветке — в Twine это делал
    * макрос по истории, у нас истории нет, есть сцена. */
  private lazy val prologue: Narrative = {
    val beats = content.beats("registration.prologue").map {
      case (key, beat) if key.startsWith("Pick_") =>
        key -> beat // текст и кнопка как в yaml; запись ветки — в маршруте ниже
      case ("Remember", beat) =>
        "Remember" -> Beat(beat.text, user =>
          readBranch(user).map { branch =>
            List(Choice(s"Wake_${branch.getOrElse("Human")}", content.text("registration.rememberLabel")))
          })
      case other => other
    }
    new Narrative(beats)
  }

  /** Маршруты `Pick_*` поверх повествования: сначала запоминаем ветку, потом
    * показываем бит. Без этого `Remember` не знал бы, в чьё тело просыпаться. */
  private lazy val pickRoutes: Map[String, Target] =
    branches.map { branch =>
      val key = s"Pick_$branch"
      key -> Target.Run { (user, ua, renderer) =>
        writeBranch(user, branch) *> (prologue.toRoutes(Registration)(key) match {
          case Target.Run(show) => show(user, ua, renderer)
          case _                => ZIO.succeed(Registration)
        })
      }
    }.toMap

  private lazy val branch: Branch = new Branch(
    routes = prologue.toRoutes(Registration) ++ pickRoutes ++ Map(
      "ConfirmRace"     -> Target.Run { (user, ua, renderer)  => confirmRace(user, ua, renderer) },
      "RaceDescription" -> Target.Run { (user, ua, renderer)  => getRaceDescription(user, ua.text, renderer) },
      "Race"            -> Target.Run { (user, _, renderer)   => getRace(user, renderer) }
    ),
    fallback = Target.Run { (user, _, renderer) => showWelcome(user, renderer) }
  )

  /** Финал: раса выбрана и подтверждена — герой получает стартовое снаряжение
    * и уходит в лабиринт. Это единственный выход из регистрации. */
  private def confirmRace(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      race <- ZIO.fromEither(decode[Race](ua.payload.get))
      _    <- heroDao.updateRace(user.userId, race)
      _    <- journal.append(GameEvent(user.userId, "race_selected",
                Json.obj("race" -> race.entryName.asJson)))
      hero <- heroDao.getHeroByUserId(user.userId)
      _    <- ZIO.whenCase(hero) { case Some(h) =>
                 ZIO.foreachDiscard(RegistrationState.starterItems) { item =>
                   itemRepo.persist(h.id, item)
                     .flatMap(itemWithId => inventoryRepo.addItem(h.id, itemWithId))
                     .orElse(ZIO.unit)
                 } *>
                 renderer.show(user, Screen(content.text("registration.startingEquipment"), Nil))
               }
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
    } yield StateType.Dungeon

  private def readBranch(user: User): Task[Option[String]] =
    heroDao.readSceneData(user.userId)
      .map(_.flatMap(_.hcursor.get[String](RegistrationState.BranchKey).toOption).filter(branches.contains))

  private def writeBranch(user: User, branch: String): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.obj(RegistrationState.BranchKey -> branch.asJson))

  private lazy val raceChoices: List[Choice] =
    Race.mortals.toList.map(r => Choice("RaceDescription", r.toString))

  private def getRaceDescription(user: User, raceName: String, renderer: Renderer): Task[StateType] =
    Race.withNameOption(raceName) match {
      case Some(r) =>
        val choices = List(
          content.choice("ConfirmRace", "registration.raceDescription.confirmLabel").copy(data = Map("race" -> r.entryName)),
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
  /** Ключ в scene_data, под которым живёт выбранная ветка пролога. */
  val BranchKey: String = "prologueBranch"

  val starterItems: List[Item] = List(
    Item(-1L,  "Меч новобранца",            1L, Rarity.Gray, ItemType.Weapon,
      attack = 1, accuracy = 1, energy = 0, armor = 0, defence = 0, evasion = 0),
    Item(-2L,  "Фляга начинающего исследователя", 1L, Rarity.Gray, ItemType.Flask,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 8, maxCharges = 8))
  )
}
