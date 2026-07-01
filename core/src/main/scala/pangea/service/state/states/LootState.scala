package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Journal, Renderer, SceneContent, Screen, Target}
import pangea.model.GameEvent
import pangea.model.hero.Hero
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
 * показываем, что выпало, и двумя inline-кнопками внутри сообщения спрашиваем
 * «Забрать»/«Оставить». Забрать → золото в кошелёк, предметы в инвентарь (переполнен →
 * предмет теряется). Оставить → добыча выбрасывается. Оба исхода ведут в Dungeon.
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
      "Continue" -> Target.Run { (user, _, _) => readLoot(user).flatMap(finish(user, _)) }
    ),
    fallback = Target.Run { (user, _, _) => readLoot(user).flatMap(finish(user, _)) }
  )

  // Куда уйти после добычи: экран сам рулит возвратом. Перед переходом кладём
  // eventData в scene_data (чтобы состояние-получатель прочитало свой прогресс),
  // иначе чистим. По умолчанию — обратно в лабиринт.
  private def finish(user: User, loot: LootData): Task[StateType] =
    heroDao.writeSceneData(user.userId, loot.eventData.getOrElse(Json.Null))
      .as(loot.returnState.getOrElse(StateType.Dungeon))

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.TreasureMobsFight, StateType.TreasureSchron)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)

      // золото и дублоны забираются всегда и сразу, без выбора
      goldTotal = loot.golds.sum
      _        <- ZIO.when(goldTotal > 0L)(heroDao.updateGold(user.userId, hero.gold + goldTotal))
      _        <- ZIO.when(loot.doubloons > 0L)(heroDao.updateDoubloons(user.userId, hero.doubloons + loot.doubloons))
      goldLines = loot.golds.map { g => content.format("loot.gold", "amount" -> g.toString) }
      currencyLines = goldLines ++
        (if (loot.doubloons > 0L) List(content.format("loot.doubloons", "amount" -> loot.doubloons.toString)) else Nil)

      _ <- if (loot.items.isEmpty) {
             // выбирать нечего — только золото/дублоны (или совсем пусто)
             val text = if (currencyLines.isEmpty) content.text("loot.empty")
                        else content.text("loot.header") + "\n\n" + currencyLines.mkString("\n")
             journal.append(GameEvent(user.userId, "loot_claimed",
               Json.obj("gold" -> goldTotal.asJson, "items" -> loot.items.map(_.name).asJson))) *>
               renderer.show(user, Screen(text, content.screen("loot.enter").choices))
           } else {
             // золото/дублоны уже в кошельке; по предметам спрашиваем «Забрать»/«Оставить»
             val preview = currencyLines ++ loot.items.map(it => itemLineWithEquipped(it, hero))
             val text    = content.text("loot.header") + "\n\n" + preview.mkString("\n")
             val choices = List(
               content.choice("Take", "loot.takeLabel"),
               content.choice("Leave", "loot.leaveLabel")
             )
             renderer.show(user, Screen(text, choices))
           }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // «Забрать»: предметы в инвентарь (переполнен → предмет теряется). Золото уже забрано в enter.
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
             Json.obj("gold" -> loot.golds.sum.asJson, "items" -> loot.items.map(_.name).asJson)))

      takenLines = results.collect { case (item, true) => itemLine(item) }
      anyLost    = results.exists { case (_, added) => !added }
      slots     <- InventoryFeedback.freeSlotsLine(inventoryRepository, content, hero.id)
      taken      = if (takenLines.isEmpty) content.text("loot.empty")
                   else content.text("loot.claimed") + "\n\n" + takenLines.mkString("\n")
      full       = if (anyLost) "\n\n" + content.text("common.inventoryFull") else ""
      _ <- renderer.show(user, Screen(taken + full + "\n\n" + slots, Nil))
      next <- finish(user, loot)
    } yield next

  // «Оставить»: предметы выбрасываются (золото уже забрано в enter).
  private def leaveLoot(user: User, renderer: Renderer): Task[StateType] =
    for {
      loot <- readLoot(user)
      _    <- journal.append(GameEvent(user.userId, "loot_left",
                Json.obj("gold" -> loot.golds.sum.asJson, "items" -> loot.items.map(_.name).asJson)))
      _    <- renderer.show(user, Screen(content.text("loot.left"), Nil))
      next <- finish(user, loot)
    } yield next

  private def readLoot(user: User): Task[LootData] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LootData].toOption).getOrElse(LootData(Nil, Nil)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def itemLine(item: Item): String =
    if (item.itemType == ItemType.Trophy)
      s"🎁 ${item.name} Ур.${item.lvl} (трофей)" // у трофеев нет редкости
    else {
      val lines = item.statsLines
      val tail  = if (lines.isEmpty) "" else "\n" + lines.mkString("\n")
      s"🎁 ${item.name} Ур.${item.lvl}$tail"
    }

  /** Карточка дропа + строки с надетым в том же слоте — чтобы сразу сравнить.
   *  Статы выводим словами на отдельных строках (так читается). Трофеи не сравниваются. */
  private def itemLineWithEquipped(item: Item, hero: Hero): String = {
    val base = itemLine(item)
    if (item.itemType == ItemType.Trophy) base
    else {
      val equipped = hero.equipment.equippedFor(item.itemType)
                       .filter(_.itemType != ItemType.NoItem)
      if (equipped.isEmpty) base
      else {
        val blocks = equipped.map(_.equippedComparison("Надето"))
        base + "\n" + blocks.mkString("\n")
      }
    }
  }
}

object LootState {
  // Содержимое scene_data между victory и экраном добычи: непросохранённые предметы
  // (id = -1) и список золотых выпадений. Плюс обобщённый «роутинг»:
  //   - doubloons   — дублоны к выдаче (премиум-валюта схрона);
  //   - returnState — куда уйти после добычи (default Dungeon); экран сам рулит;
  //   - eventData   — непрозрачный блоб для состояния-получателя (его пишут в
  //                   scene_data перед переходом, чтобы событие прочитало свой
  //                   прогресс — напр. ChainData цепочки боёв).
  final case class LootData(
    items:       List[Item],
    golds:       List[Long],
    doubloons:   Long              = 0L,
    returnState: Option[StateType] = None,
    eventData:   Option[Json]      = None
  )
  object LootData {
    implicit val encoder: Encoder[LootData] = deriveEncoder[LootData]
    implicit val decoder: Decoder[LootData] = deriveDecoder[LootData]
  }
}
