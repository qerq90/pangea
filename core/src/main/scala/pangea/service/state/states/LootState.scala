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
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{InventoryFeedback, State, UserAction}
import zio.{Task, ZIO}

/**
 * Промежуточный экран добычи после победы. `BattleState.victory` уже прокатал лут
 * (чистый `LootGenerator`) и положил его в scene_data; здесь мы «осматриваем добычу»:
 * показываем, что выпало, и двумя кнопками спрашиваем
 * «Забрать»/«Оставить». Забрать → серебро в кошелёк, предметы в инвентарь (переполнен →
 * предмет теряется). Оставить → добыча выбрасывается. Оба исхода ведут в Dungeon.
 *
 * После группового боя добыча идёт по одному павшему за экран (`queue`): каждый
 * исход показывает следующего, пока очередь не опустеет, и только тогда — уход.
 */
case class LootState(
  heroDao:             HeroDao,
  inventoryRepository: InventoryRepository,
  itemRepository:      ItemRepository,
  journal:             Journal,
  content:             SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Take"     -> Target.Run { (user, _, renderer) => claimLoot(user, renderer) },
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
        StateType.MarisaHunt, StateType.FlowerMeadow, StateType.MurlocElder, StateType.MurlocVillage)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)

      // серебро и дублоны забираются всегда и сразу, без выбора; «Спаситель
      // Марисы» получает серебра на десятую больше.
      silvers     = loot.silvers.map(_ * Achievement.silverPct(hero) / 100L)
      silverTotal = silvers.sum
      _        <- ZIO.when(silverTotal > 0L)(heroDao.updateSilver(user.userId, hero.silver + silverTotal))
      _        <- ZIO.when(loot.doubloons > 0L)(heroDao.updateDoubloons(user.userId, hero.doubloons + loot.doubloons))
      silverLines = silvers.map { s => content.format("loot.silver", "amount" -> s.toString) }
      currencyLines = silverLines ++
        (if (loot.doubloons > 0L) List(content.format("loot.doubloons", "amount" -> loot.doubloons.toString)) else Nil)

      // В группе над добычей стоит имя павшего — чтобы было видно, чья она.
      header = loot.monsterName match {
        case Some(name) => content.format("loot.groupHeader", "monster" -> name)
        case None       => content.text("loot.header")
      }
      empty = loot.monsterName match {
        case Some(name) => content.format("loot.groupEmpty", "monster" -> name)
        case None       => content.text("loot.empty")
      }
      _ <- if (loot.items.isEmpty) {
             // выбирать нечего — только серебро/дублоны (или совсем пусто)
             val text = if (currencyLines.isEmpty) empty
                        else header + "\n\n" + currencyLines.mkString("\n")
             journal.append(GameEvent(user.userId, "loot_claimed",
               Json.obj("silver" -> silverTotal.asJson, "items" -> loot.items.map(_.name).asJson))) *>
               // В группе, пока павшие ещё есть, решать нечего — сообщение и сразу следующий.
               (if (loot.queue.nonEmpty) renderer.show(user, Screen(text, Nil)) *> finish(user, loot, renderer).unit
                else renderer.show(user, Screen(text, content.screen("loot.enter").choices)))
           } else {
             // серебро/дублоны уже в кошельке; по предметам спрашиваем «Забрать»/«Оставить»
             val preview = currencyLines ++ loot.items.map(it => itemLineWithEquipped(it, hero))
             val text    = header + "\n\n" + preview.mkString("\n")
             val choices = List(
               content.choice("Take", "loot.takeLabel"),
               content.choice("Leave", "loot.leaveLabel")
             )
             renderer.show(user, Screen(text, choices))
           }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // «Забрать»: предметы в инвентарь (переполнен → предмет теряется). Серебро уже забрано в enter.
  private def claimLoot(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)

      results <- ZIO.foreach(loot.items) { item =>
                   for {
                     persisted <- itemRepository.persist(hero.id, item)
                     added     <- inventoryRepository.addItem(hero.id, persisted).as(true)
                                    .catchAll(_ => ZIO.succeed(false))
                   } yield (persisted, added)
                 }

      _ <- journal.append(GameEvent(user.userId, "loot_claimed",
             Json.obj("silver" -> loot.silvers.sum.asJson, "items" -> loot.items.map(_.name).asJson)))

      takenLines = results.collect { case (item, true) => itemLine(item) }
      anyLost    = results.exists { case (_, added) => !added }
      slots     <- InventoryFeedback.freeSlotsLine(inventoryRepository, content, hero.id)
      taken      = if (takenLines.isEmpty) content.text("loot.empty")
                   else content.text("loot.claimed") + "\n\n" + takenLines.mkString("\n")
      full       = if (anyLost) "\n\n" + content.text("common.inventoryFull") else ""
      _ <- renderer.show(user, Screen(taken + full + "\n\n" + slots, Nil))
      next <- finish(user, loot, renderer)
    } yield next

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
    else if (item.itemType == ItemType.Gem || item.itemType == ItemType.Material)
      s"🎁 ${item.displayTitle}" // описания камня, ингредиента и пыли при выпадении не показываем — только в инвентаре
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
