package pangea.service.state.states.registration

import io.circe.Json
import io.circe.jawn.decode
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Beat, Branch, Choice, Journal, Narrative, Players, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.model.GameEvent
import pangea.model.hero.LoreData
import pangea.model.item.{BeltSlot, FlaskKind, Grip, Item, ItemDetails, ItemType, Rarity}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.state.StateType.Registration
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.chat.Links
import pangea.service.state.states.InventoryState
import pangea.service.state.{FirstStepsQuest, NpcQuestLog, State, UserAction}
import zio.{Random, Task, ZIO}

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

  /** Пробуждение после портала (`registration.arrival`): падение на крышу
    * сторожки посреди лабиринта, тайник искателя, мысли вслух — и задание,
    * которое герой ставит себе сам. Отдельно от пролога: все его биты ведут к
    * порталу, а эти идут уже после выбора расы. */
  private lazy val arrival: Map[String, Target] =
    new Narrative(content.beats("registration.arrival")).toRoutes(Registration)

  /** Маршруты поверх пробуждения: обыск выдаёт тайник, «Встать» — задание. */
  private lazy val arrivalRoutes: Map[String, Target] = Map(
    "ArrivalSearch" -> Target.Run { (user, _, renderer) => search(user, renderer) },
    "ArrivalStand"  -> Target.Run { (user, _, renderer)  => stand(user, renderer) }
  )

  private lazy val branch: Branch = new Branch(
    routes = prologue.toRoutes(Registration) ++ arrival ++ pickRoutes ++ arrivalRoutes ++ Map(
      "ConfirmRace"     -> Target.Run { (user, ua, renderer)  => confirmRace(user, ua, renderer) },
      "RaceDescription" -> Target.Run { (user, ua, renderer)  => getRaceDescription(user, ua.text, renderer) },
      "Race"            -> Target.Run { (user, _, renderer)   => getRace(user, renderer) }
    ),
    fallback = Target.Run { (user, _, renderer) => showWelcome(user, renderer) }
  )

  /** Раса выбрана и подтверждена — герой падает на крышу сторожки в лабиринте
    * (см. [[arrival]]). Ветка пролога (какой смертью прошёл) переезжает из
    * scene_data в durable `lore_data`: сцена дальше занята пробуждением, а выбор
    * ещё пригодится сюжету. */
  private def confirmRace(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      race   <- ZIO.fromEither(ua.payload.toRight(new Throwable("ConfirmRace без payload")).flatMap(decode[Race]))
      branch <- readBranch(user)
      _      <- heroDao.updateRace(user.userId, race)
      lore   <- loreOf(user)
      _      <- heroDao.writeLoreData(user.userId, lore.copy(prologueBranch = branch).asJson)
      _      <- journal.append(GameEvent(user.userId, "race_selected",
                  Json.obj("race" -> race.entryName.asJson, "prologueBranch" -> branch.asJson)))
      _      <- heroDao.writeSceneData(user.userId, Json.obj())
      res    <- showArrival("Landing", user, ua, renderer)
    } yield res

  /** Обыск сторожки: тайник искателя — меч и щит в руки, пояс с флягой целителя
    * на себя, двуручный топор и лук или арбалет в сумку (см.
    * [[RegistrationState.kit]]). Выдаётся один раз: ВК, бывает, шлёт кнопку
    * дважды, и второй обыск только покажет сцену — с тем же луком или арбалетом,
    * что лежит в сумке. */
  private def search(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene  <- heroDao.readSceneData(user.userId)
      found   = scene.exists(_.hcursor.get[Boolean](RegistrationState.KitKey).contains(true))
      noun   <- if (found) ZIO.succeed(scene.flatMap(_.hcursor.get[String](RegistrationState.KitRangedKey).toOption)
                                   .getOrElse(RegistrationState.BowNoun))
                else grantKit(user)
      res    <- showSearch(user, noun, renderer)
    } yield res

  /** Сцена обыска: в тексте — тот стрелковый, что выпал (`{ranged}` — с большой
    * буквы, в перечне; `{rangedLower}` — в последней фразе). */
  private def showSearch(user: User, noun: String, renderer: Renderer): Task[StateType] =
    content.beats("registration.arrival").collectFirst { case ("ArrivalSearch", beat) => beat } match {
      case None => ZIO.succeed(Registration)
      case Some(beat) =>
        for {
          choices <- beat.buildChoices(user)
          text     = beat.text.replace("{ranged}", noun).replace("{rangedLower}", noun.toLowerCase)
          _       <- renderer.show(user, Screen(text, choices))
        } yield Registration
    }

  /** Выдать тайник; возвращает, что из стрелкового в нём оказалось. */
  private def grantKit(user: User): Task[String] =
    heroDao.getHeroByUserId(user.userId).flatMap {
      case None => ZIO.succeed(RegistrationState.BowNoun)
      case Some(h) =>
        for {
          seed   <- Random.nextLong
          kit     = RegistrationState.kit(Rng(seed))
          sword  <- itemRepo.persist(h.id, kit.sword)
          shield <- itemRepo.persist(h.id, kit.shield)
          belt   <- itemRepo.persist(h.id, kit.belt)
          axe    <- itemRepo.persist(h.id, kit.axe)
          ranged <- itemRepo.persist(h.id, kit.ranged)
          worn    = List(sword, shield, belt)
          eq      = h.equipment.copy(weapon = sword, leftHand = shield, belt = belt)
          fight   = worn.foldLeft(h.fightStats)((f, it) => InventoryState.applyDelta(f, it, Item.NoItem))
          _      <- heroDao.updateEquipmentAndFightStats(user.userId, eq, fight)
          _      <- inventoryRepo.addItem(h.id, axe).orElse(ZIO.unit)
          _      <- inventoryRepo.addItem(h.id, ranged).orElse(ZIO.unit)
          _      <- heroDao.writeSceneData(user.userId, Json.obj(
                      RegistrationState.KitKey       -> true.asJson,
                      RegistrationState.KitRangedKey -> kit.rangedNoun.asJson))
        } yield kit.rangedNoun
    }

  /** «Встать»: герой ставит себе задание «Встать на ноги» (см.
    * [[pangea.model.quest.FirstSteps]]) и уходит в лабиринт — к городу ему ещё
    * идти. Это единственный выход из регистрации. Задание пишется один раз,
    * сцена регистрации стирается. Следом за заданием — отдельное сообщение с
    * туториалом по игре и приглашением в общую беседу (см. [[Links]]). */
  private def stand(user: User, renderer: Renderer): Task[StateType] =
    for {
      lore   <- loreOf(user)
      _      <- ZIO.unless(lore.firstSteps)(heroDao.writeLoreData(user.userId, lore.copy(firstSteps = true).asJson))
      hero   <- heroDao.getHeroByUserId(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- ZIO.foreachDiscard(hero)(h => renderer.show(user, Screen(FirstStepsQuest.taken(content, h, quests), Nil)))
      _      <- renderer.show(user, Screen(content.format("registration.tutorial",
                  "tutorial" -> Links.Tutorial, "chat" -> Links.Chat), Nil))
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
    } yield StateType.Dungeon

  private def showArrival(key: String, user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    arrival.get(key) match {
      case Some(Target.Run(show)) => show(user, ua, renderer)
      case _                      => ZIO.succeed(Registration)
    }

  private def loreOf(user: User): Task[LoreData] =
    heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

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

  /** Отметка в scene_data: тайник в сторожке уже обыскан. */
  val KitKey: String = "kitFound"

  /** Что из стрелкового нашлось в тайнике — для повторного показа сцены. */
  val KitRangedKey: String = "kitRanged"

  val BowNoun: String      = "Лук"
  val CrossbowNoun: String = "Арбалет"

  /** Тайник искателя в сторожке, куда герой падает после портала. `rangedNoun` —
    * что из стрелкового выпало: лук или арбалет. */
  final case class Kit(sword: Item, shield: Item, axe: Item, belt: Item, ranged: Item, rangedNoun: String)

  /** Всё белое и первого уровня, статы — как у обычной добычи, а имена свои:
    * хозяин тайника — искатель. На поясе — фляга целителя: у белой как раз
    * четыре глотка, а у белого пояса одна ячейка — под неё. Из стрелкового —
    * поровну лук или арбалет. */
  def kit(rng: Rng): Kit = {
    val white = Rarity.White
    def named(item: Item, noun: String): Item = item.copy(name = s"${white.emoji} Простой $noun Искателя")
    val (sword, r1)  = ItemGenerator.createItemOfType(ItemType.Weapon, 1L, white, rng, Some(Grip.OneHanded))
    val (shield, r2) = ItemGenerator.createItemOfType(ItemType.Shield, 1L, white, r1)
    val (axe, r3)    = ItemGenerator.createItemOfType(ItemType.Weapon, 1L, white, r2, Some(Grip.TwoHanded))
    val (belt, r4)   = ItemGenerator.createItemOfType(ItemType.Belt, 1L, white, r3)
    val (bow, r5)    = r4.nextBoolean
    val (grip, noun) = if (bow) (Grip.Bow, BowNoun) else (Grip.Crossbow, CrossbowNoun)
    val (ranged, _)  = ItemGenerator.createItemOfType(ItemType.Weapon, 1L, white, r5, Some(grip))
    val flask        = BeltSlot.Flask(FlaskKind.Healer, white, FlaskKind.chargesFor(white))
    Kit(
      sword      = named(sword, "Меч"),
      shield     = named(shield, "Круглый Щит"),
      axe        = named(axe, "Двуручный Топор"),
      belt       = named(belt, "Пояс").copy(details = ItemDetails.Belt(List(flask))),
      ranged     = named(ranged, noun),
      rangedNoun = noun)
  }
}
