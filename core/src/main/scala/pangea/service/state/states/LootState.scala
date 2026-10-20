package pangea.service.state.states

import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Journal, Renderer, SceneContent, Screen, Target}
import pangea.model.GameEvent
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.artifact.ArtifactRepository
import pangea.service.artifact.{ArtifactIntake, Intake}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{InventoryFeedback, QuestSupport, SlotChoice, State, TakeAndEquip, UserAction}
import zio.{Task, ZIO}

/**
 * Промежуточный экран добычи после победы. `BattleState.victory` уже прокатал лут
 * (чистый `LootGenerator`) и положил его в scene_data; здесь мы «осматриваем добычу»:
 * показываем, что выпало, и двумя кнопками спрашиваем
 * «Забрать»/«Оставить». Забрать → серебро в кошелёк, предметы в инвентарь (переполнен →
 * предмет теряется). Оставить → добыча выбрасывается. Оба исхода ведут в Dungeon.
 * Если что-то из добычи встаёт в пустой слот, есть и третья кнопка — «Забрать и
 * надеть»: такие вещи сразу на героя, мимо сумки (см. [[TakeAndEquip]]). На
 * каждую вещь, которая встанет вместо надетой, — своя кнопка «Надеть: …»: всё
 * забирается, эта вещь надевается, снятое уходит в сумку.
 *
 * После группового боя добыча идёт по одному павшему за экран (`queue`): каждый
 * исход показывает следующего, пока очередь не опустеет, и только тогда — уход.
 */
case class LootState(
  heroDao:             HeroDao,
  inventoryRepository: InventoryRepository,
  itemRepository:      ItemRepository,
  journal:             Journal,
  content:             SceneContent,
  artifacts:           Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Take"     -> Target.Run { (user, _, renderer) => claimLoot(user, renderer, wear = false) },
      "TakeWear" -> Target.Run { (user, _, renderer) => claimLoot(user, renderer, wear = true) },
      // «Надеть: …» — и кнопки выбора кольца или руки после него
      "TakeSwap" -> Target.Run { (user, ua, renderer) => swapLoot(user, ua, renderer) },
      "SwapBack" -> Target.Run { (user, _, renderer) => showPreview(user, renderer) },
      "Leave"    -> Target.Run { (user, _, renderer) => leaveLoot(user, renderer) },
      "Continue" -> Target.Run { (user, _, renderer) => readLoot(user).flatMap(finish(user, _, renderer)) }
    ),
    fallback = Target.Run { (user, _, renderer) => readLoot(user).flatMap(finish(user, _, renderer)) }
  )

  // Добыча этого павшего разобрана. Если в очереди есть следующий — его экран
  // (в то же состояние переход не ведёт, поэтому `enter` зовём сами). Иначе —
  // куда уйти после добычи: экран сам рулит возвратом. Перед переходом кладём
  // eventData в scene_data (чтобы состояние-получатель прочитало свой прогресс),
  // иначе чистим. По умолчанию — обратно в лабиринт.
  private def finish(user: User, loot: LootData, renderer: Renderer): Task[StateType] =
    loot.queue match {
      case Nil =>
        heroDao.writeSceneData(user.userId, loot.eventData.getOrElse(Json.Null))
          .as(loot.returnState.getOrElse(StateType.Dungeon))
      case next :: rest =>
        val following = loot.copy(
          items = next.items, silvers = next.silvers, doubloons = next.doubloons,
          monsterName = Some(next.monsterName), queue = rest)
        heroDao.writeSceneData(user.userId, following.asJson) *> enter(user, renderer).as(StateType.Loot)
    }

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.GlobalMap, StateType.TreasureMobsFight, StateType.TreasureSchron, StateType.Loot, StateType.Girl,
        StateType.MarisaHunt, StateType.FlowerMeadow, StateType.MonsterCave, StateType.Caravan,
        StateType.MurlocElder, StateType.MurlocVillage)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)

      // серебро и дублоны забираются всегда и сразу, без выбора
      silverTotal = silvers(hero, loot).sum
      _        <- ZIO.when(silverTotal > 0L)(heroDao.updateSilver(user.userId, hero.silver + silverTotal))
      _        <- ZIO.when(loot.doubloons > 0L)(heroDao.updateDoubloons(user.userId, hero.doubloons + loot.doubloons))
      currency  = currencyLines(hero, loot)

      empty = loot.monsterName match {
        case Some(name) => content.format("loot.groupEmpty", "monster" -> name)
        case None       => content.text("loot.empty")
      }
      _ <- if (loot.items.isEmpty) {
             // выбирать нечего — только серебро/дублоны (или совсем пусто)
             val text = if (currency.isEmpty) empty
                        else header(loot) + "\n\n" + currency.mkString("\n")
             journal.append(GameEvent(user.userId, "loot_claimed",
               Json.obj("silver" -> silverTotal.asJson, "items" -> loot.items.map(_.name).asJson))) *>
               // В группе, пока павшие ещё есть, решать нечего — сообщение и сразу следующий.
               (if (loot.queue.nonEmpty) renderer.show(user, Screen(text, Nil)) *> finish(user, loot, renderer).unit
                else renderer.show(user, Screen(text, content.screen("loot.enter").choices)))
           } else
             // серебро/дублоны уже в кошельке; по предметам спрашиваем
             renderer.show(user, previewScreen(hero, loot))
    } yield ()

  /** Серебро с добычи: «Спаситель Марисы» получает на десятую больше. */
  private def silvers(hero: Hero, loot: LootData): List[Long] =
    loot.silvers.map(_ * Achievement.silverPct(hero) / 100L)

  private def currencyLines(hero: Hero, loot: LootData): List[String] =
    silvers(hero, loot).map(s => content.format("loot.silver", "amount" -> s.toString)) ++
      (if (loot.doubloons > 0L) List(content.format("loot.doubloons", "amount" -> loot.doubloons.toString)) else Nil)

  // В группе над добычей стоит имя павшего — чтобы было видно, чья она.
  private def header(loot: LootData): String = loot.monsterName match {
    case Some(name) => content.format("loot.groupHeader", "monster" -> name)
    case None       => content.text("loot.header")
  }

  /** Превью добычи: валюта, вещи с надетым для сравнения и кнопки. Валюта уже в
    * кошельке, строки о ней — только для глаз, поэтому превью можно показать ещё
    * раз ([[showPreview]]): после «Назад» с выбора кольца или отказа «сумка полна».
    * Сверху «Забрать», под ним «Забрать и надеть» — если что-то встаёт в пустой
    * слот — и «Надеть: …» на каждую вещь, которая встанет вместо надетой;
    * последней — «Оставить». */
  private def previewScreen(hero: Hero, loot: LootData): Screen = {
    val preview = currencyLines(hero, loot) ++ loot.items.map(it => itemLineWithEquipped(it, hero))
    val wearAll = Option.when(TakeAndEquip.pick(hero, loot.items).nonEmpty)(content.choice("TakeWear", "loot.takeWearLabel"))
    val swaps   = loot.items.zipWithIndex.collect { case (item, i) if TakeAndEquip.replaces(hero, item) =>
      content.choice("TakeSwap", "loot.swapLabel", "name" -> item.name).copy(data = swapData(i, item))
    }
    Screen(header(loot) + "\n\n" + preview.mkString("\n"),
      List(content.choice("Take", "loot.takeLabel")) ++ wearAll.toList ++ swaps ++ List(content.choice("Leave", "loot.leaveLabel")))
  }

  /** Показать превью ещё раз — без начисления серебра. Вещей уже нет — выбирать
    * нечего, идём дальше. */
  private def showPreview(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)
      next <- if (loot.items.isEmpty) finish(user, loot, renderer)
              else renderer.show(user, previewScreen(hero, loot)).as(StateType.Loot)
    } yield next

  /** Данные кнопки «Надеть: …»: номер вещи и её имя — старая кнопка не наденет
    * чужую вещь, если добыча на экране уже другая. */
  private def swapData(i: Int, item: Item): Map[String, String] = Map("i" -> i.toString, "n" -> item.name)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // «Забрать»: предметы в инвентарь (переполнен → предмет теряется). Серебро уже забрано в enter.
  // «Забрать и надеть» (`wear`): что встаёт в пустой слот, сразу на героя, мимо сумки.
  private def claimLoot(user: User, renderer: Renderer, wear: Boolean): Task[StateType] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)
      next <- claim(user, renderer, hero, loot, if (wear) TakeAndEquip.pick(hero, loot.items) else Nil, slot = None)
    } yield next

  // «Надеть: X»: всё забираем, а X — вместо надетой. Оба кольца или обе руки
  // заняты — сперва спрашиваем, что снять (та же кнопка, но с номером слота);
  // снятому нет места в сумке — не надеваем и показываем добычу снова. Кнопка со
  // старого экрана, где вещь уже другая, тоже просто показывает добычу.
  private def swapLoot(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      loot  <- readLoot(user)
      inv   <- inventoryRepository.get(hero.id).mapError(e => new Throwable(e.toString))
      slot   = payload(ua, "slot").flatMap(_.toIntOption).filter(s => s == 1 || s == 2)
      chosen = payload(ua, "i").flatMap(_.toIntOption).flatMap(i => loot.items.lift(i).map(i -> _))
                 .filter { case (_, item) => payload(ua, "n").contains(item.name) && TakeAndEquip.replaces(hero, item) }
      next  <- chosen match {
                 case None => showPreview(user, renderer)
                 case Some((i, item)) if slot.isEmpty && SlotChoice.needed(hero.equipment, item) =>
                   renderer.show(user, SlotChoice.screen(content, item, hero.equipment, "TakeSwap", swapData(i, item),
                     content.choice("SwapBack", "common.back"))).as(StateType.Loot)
                 case Some((i, item)) =>
                   TakeAndEquip.noRoom(hero, inv, item, slot) match {
                     case Some(key) =>
                       renderer.show(user, Screen(content.format(key, "name" -> item.name), Nil)) *> showPreview(user, renderer)
                     case None => claim(user, renderer, hero, loot, List(i), slot)
                   }
               }
    } yield next

  /** Забрать добычу: вещи с номерами `wearIdx` — сразу на героя (`slot` — для
    * одной вещи, см. `TakeAndEquip.wear`), остальное — в артефакты и сумку. */
  private def claim(user: User, renderer: Renderer, hero: Hero, loot: LootData,
                    wearIdx: List[Int], slot: Option[Int]): Task[StateType] =
    for {
      persisted <- ZIO.foreach(loot.items)(itemRepository.persist(hero.id, _))
      wornItems  = wearIdx.flatMap(persisted.lift)
      // Сперва надеваем: «Тайник» на надетом прибавит сумке места до того, как
      // в неё ляжет остальное, а снятое ляжет раньше добычи — место под него
      // проверено заранее.
      worn      <- TakeAndEquip.wear(heroDao, inventoryRepository, user, hero, wornItems, slot, Some(itemRepository))

      // Камни летят в Ларец Азата, травы и отвары — в Живую сумку; что не
      // взяли артефакты, идёт в сумку героя, как и раньше.
      kept     = persisted.zipWithIndex.collect { case (item, i) if !wearIdx.contains(i) => item }
      results <- ZIO.foreach(kept) { item =>
                   ArtifactIntake.accept(artifacts, inventoryRepository, hero.id, item).map(item -> _)
                 }

      _ <- journal.append(GameEvent(user.userId, "loot_claimed",
             Json.obj("silver" -> loot.silvers.sum.asJson, "items" -> loot.items.map(_.name).asJson,
                      "worn" -> wornItems.map(_.name).asJson, "removed" -> worn.removed.map(_.name).asJson)))

      takenLines = wornItems.map(itemLine) ++ results.collect {
        case (item, Intake.ToInventory)             => itemLine(item)
        case (item, Intake.ToArtifact(kind, free))  => ArtifactIntake.line(content, item, kind, free)
      }
      // У каждой непринятой вещи своя причина: сумка полна или пыли уже сотня.
      lostLines  = results.collect { case (item, Intake.Refused) => InventoryFeedback.refusalLine(content, item) }.distinct
      slots     <- InventoryFeedback.freeSlotsLine(inventoryRepository, content, hero.id)
      taken      = if (takenLines.isEmpty) content.text("loot.empty")
                   else content.text("loot.claimed") + "\n\n" + takenLines.mkString("\n")
      wornLine   = if (wornItems.isEmpty) ""
                   else "\n\n" + content.format("loot.worn", "names" -> wornItems.map(_.name).mkString(", ")) +
                     TakeAndEquip.takenOffLine(content, worn)
      full       = if (lostLines.isEmpty) "" else "\n\n" + lostLines.mkString("\n")
      _ <- renderer.show(user, Screen(taken + wornLine + full + "\n\n" + slots, Nil))
      // Двенадцатая вещь набора — за собранный набор своё достижение.
      _ <- QuestSupport.grantFullSets(heroDao, content, user, worn.hero, renderer)
      next <- finish(user, loot, renderer)
    } yield next

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  // «Оставить»: предметы выбрасываются (серебро уже забрано в enter).
  private def leaveLoot(user: User, renderer: Renderer): Task[StateType] =
    for {
      loot <- readLoot(user)
      _    <- journal.append(GameEvent(user.userId, "loot_left",
                Json.obj("silver" -> loot.silvers.sum.asJson, "items" -> loot.items.map(_.name).asJson)))
      _    <- renderer.show(user, Screen(content.text("loot.left"), Nil))
      next <- finish(user, loot, renderer)
    } yield next

  private def readLoot(user: User): Task[LootData] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LootData].toOption).getOrElse(LootData(Nil, Nil)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def itemLine(item: Item): String =
    if (item.itemType == ItemType.Trophy)
      s"🎁 ${item.displayTitle} (трофей)" // у трофеев нет редкости — кружка в заголовке не будет
    else if (item.itemType == ItemType.Gem || item.itemType == ItemType.Material || item.itemType == ItemType.RuneStone)
      s"🎁 ${item.displayTitle}" // описания камня, ингредиента, пыли и рунной плиты при выпадении не показываем — только в инвентаре
    else {
      val lines = item.statsLines
      val tail  = if (lines.isEmpty) "" else "\n" + lines.mkString("\n")
      s"🎁 ${item.displayTitle}$tail"
    }

  /** Карточка дропа + строки с надетым в том же слоте — чтобы сразу сравнить.
   *  Между дропом и надетым — тот же разделитель, что при находке предмета
   *  ([[Item.ComparisonSeparator]]). Трофеи не сравниваются. */
  private def itemLineWithEquipped(item: Item, hero: Hero): String = {
    val base = itemLine(item)
    if (item.itemType == ItemType.Trophy) base
    else {
      val equipped = hero.equipment.equippedFor(item.itemType)
                       .filter(_.itemType != ItemType.NoItem)
      if (equipped.isEmpty) base
      else {
        val blocks = equipped.map(_.equippedComparison("Надето"))
        base + "\n" + Item.ComparisonSeparator + "\n" + blocks.mkString("\n")
      }
    }
  }
}

object LootState {
  // Содержимое scene_data между victory и экраном добычи: непросохранённые предметы
  // (id = -1) и список серебряных выпадений. Плюс обобщённый «роутинг»:
  //   - doubloons   — дублоны к выдаче (премиум-валюта схрона);
  //   - returnState — куда уйти после добычи (default Dungeon); экран сам рулит;
  //   - eventData   — непрозрачный блоб для состояния-получателя (его пишут в
  //                   scene_data перед переходом, чтобы событие прочитало свой
  //                   прогресс — напр. ChainData цепочки боёв);
  //   - monsterName — чья это добыча (только после группового боя);
  //   - queue       — добыча остальных павших, по одному экрану на каждого;
  //   - won         — это добыча за ПОБЕДУ (пишет BattleState.applyVictory), а не
  //                   роутинг, положенный сценой перед боем. По ней бой узнаёт
  //                   свою же не доехавшую до экрана победу (см. BattleState.recover).
  final case class LootData(
    items:       List[Item],
    silvers:     List[Long],
    doubloons:   Long              = 0L,
    returnState: Option[StateType] = None,
    eventData:   Option[Json]      = None,
    monsterName: Option[String]    = None,
    queue:       List[MonsterLoot] = Nil,
    won:         Boolean           = false
  )
  object LootData {
    implicit val encoder: Encoder[LootData] = deriveEncoder[LootData]
    // Рукописный декодер: добыча лежит в scene_data, и новое поле не должно
    // стирать уже накатанную (см. память о производных декодерах).
    implicit val decoder: Decoder[LootData] = (c: HCursor) =>
      for {
        items       <- c.get[List[Item]]("items")
        silvers     <- c.get[List[Long]]("silvers")
        doubloons   <- c.getOrElse[Long]("doubloons")(0L)
        returnState <- c.getOrElse[Option[StateType]]("returnState")(None)
        eventData   <- c.getOrElse[Option[Json]]("eventData")(None)
        monsterName <- c.getOrElse[Option[String]]("monsterName")(None)
        queue       <- c.getOrElse[List[MonsterLoot]]("queue")(Nil)
        won         <- c.getOrElse[Boolean]("won")(false)
      } yield LootData(items, silvers, doubloons, returnState, eventData, monsterName, queue, won)
  }

  /** Добыча с одного павшего в группе: ждёт своей очереди на экран. */
  final case class MonsterLoot(monsterName: String, items: List[Item], silvers: List[Long], doubloons: Long)
  object MonsterLoot {
    implicit val encoder: Encoder[MonsterLoot] = deriveEncoder[MonsterLoot]
    implicit val decoder: Decoder[MonsterLoot] = (c: HCursor) =>
      for {
        monsterName <- c.getOrElse[String]("monsterName")("")
        items       <- c.getOrElse[List[Item]]("items")(Nil)
        silvers     <- c.getOrElse[List[Long]]("silvers")(Nil)
        doubloons   <- c.getOrElse[Long]("doubloons")(0L)
      } yield MonsterLoot(monsterName, items, silvers, doubloons)
  }
}
