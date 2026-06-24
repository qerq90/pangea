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
import pangea.service.state.{State, UserAction}
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
      "Continue" -> Target.Goto(StateType.Dungeon)
    ),
    fallback = Target.Goto(StateType.Dungeon)
  )

  override def targetStates: Set[StateType] = Set(StateType.Dungeon)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)

      // золото забирается всегда и сразу, без выбора
      goldTotal = loot.golds.sum
      _        <- ZIO.when(goldTotal > 0L)(heroDao.updateGold(user.userId, hero.gold + goldTotal))
      goldLines = loot.golds.map { g => content.format("loot.gold", "amount" -> g.toString) }

      _ <- if (loot.items.isEmpty) {
             // выбирать нечего — только золото (или совсем пусто)
             val text = if (goldLines.isEmpty) content.text("loot.empty")
                        else content.text("loot.header") + "\n\n" + goldLines.mkString("\n")
             journal.append(GameEvent(user.userId, "loot_claimed",
               Json.obj("gold" -> goldTotal.asJson, "items" -> loot.items.map(_.name).asJson))) *>
               renderer.show(user, Screen(text, content.screen("loot.enter").choices))
           } else {
             // золото уже в кошельке; по предметам спрашиваем «Забрать»/«Оставить»
             val preview = goldLines ++ loot.items.map(itemLine)
             val text    = content.text("loot.header") + "\n\n" + preview.mkString("\n")
             val choices = List(
               content.choice("Take", "loot.takeLabel"),
               content.choice("Leave", "loot.leaveLabel")
             )
             renderer.show(user, Screen(text, choices, inline = true))
           }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // «Забрать»: предметы в инвентарь (переполнен → предмет теряется). Золото уже забрано в enter.
  private def claimLoot(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)

      itemLines <- ZIO.foreach(loot.items) { item =>
                     for {
                       persisted <- itemRepository.persist(hero.id, item)
                       added     <- inventoryRepository.addItem(hero.id, persisted).as(true)
                                      .catchAll(_ => ZIO.succeed(false))
                     } yield
                       if (added) itemLine(persisted)
                       else content.format("loot.itemLost", "name" -> persisted.name)
                   }

      _ <- journal.append(GameEvent(user.userId, "loot_claimed",
             Json.obj("gold" -> loot.golds.sum.asJson, "items" -> loot.items.map(_.name).asJson)))

      text = if (itemLines.isEmpty) content.text("loot.empty")
             else content.text("loot.claimed") + "\n\n" + itemLines.mkString("\n")
      _ <- renderer.show(user, Screen(text, Nil))
    } yield StateType.Dungeon

  // «Оставить»: предметы выбрасываются (золото уже забрано в enter).
  private def leaveLoot(user: User, renderer: Renderer): Task[StateType] =
    for {
      loot <- readLoot(user)
      _    <- journal.append(GameEvent(user.userId, "loot_left",
                Json.obj("gold" -> loot.golds.sum.asJson, "items" -> loot.items.map(_.name).asJson)))
      _    <- renderer.show(user, Screen(content.text("loot.left"), Nil))
    } yield StateType.Dungeon

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
      val stats = List(
        Option.when(item.attack > 0)(s"Атака +${item.attack}"),
        Option.when(item.accuracy > 0)(s"Точность +${item.accuracy}"),
        Option.when(item.concentration > 0)(s"Концентрация +${item.concentration}"),
        Option.when(item.armor > 0)(s"Броня +${item.armor}"),
        Option.when(item.defence > 0)(s"Защита +${item.defence}"),
        Option.when(item.evasion > 0)(s"Уклонение +${item.evasion}"),
        Option.when(item.hp > 0)(s"HP +${item.hp}")
      ).flatten
      val tail = if (stats.nonEmpty) " — " + stats.mkString(", ") else ""
      s"🎁 ${item.name} [${item.rarity}] Ур.${item.lvl}$tail"
    }
}

object LootState {
  // Содержимое scene_data между victory и экраном добычи: непросохранённые предметы
  // (id = -1) и список золотых выпадений.
  final case class LootData(items: List[Item], golds: List[Long])
  object LootData {
    implicit val encoder: Encoder[LootData] = deriveEncoder[LootData]
    implicit val decoder: Decoder[LootData] = deriveDecoder[LootData]
  }
}
