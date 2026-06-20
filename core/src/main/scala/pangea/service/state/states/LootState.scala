package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Journal, Renderer, SceneContent, Screen, Target}
import pangea.model.GameEvent
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/**
 * Промежуточный экран добычи после победы. `BattleState.victory` уже прокатал лут
 * (чистый `LootGenerator`) и положил его в scene_data; здесь мы «осматриваем добычу»:
 * предметы кладём в инвентарь (переполнен → предмет теряется), золото начисляем,
 * показываем итог и кнопку «Продолжить» → Dungeon.
 */
case class LootState(
  heroDao:             HeroDao,
  inventoryRepository: InventoryRepository,
  itemRepository:      ItemRepository,
  journal:             Journal,
  content:             SceneContent
) extends State {

  private val branch = new Branch(
    routes   = Map("Continue" -> Target.Goto(StateType.Dungeon)),
    fallback = Target.Goto(StateType.Dungeon)
  )

  override def targetStates: Set[StateType] = Set(StateType.Dungeon)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- heroDao.getHeroByUserId(user.userId)
                .flatMap(ZIO.fromOption(_))
                .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
      raw  <- heroDao.readSceneData(user.userId)
      loot  = raw.flatMap(_.as[LootData].toOption).getOrElse(LootData(Nil, Nil))

      // золото
      goldTotal = loot.golds.sum
      _        <- ZIO.when(goldTotal > 0L)(heroDao.updateGold(user.userId, hero.gold + goldTotal))

      // предметы: персист (реальный id) → в инвентарь; переполнен → теряется
      itemLines <- ZIO.foreach(loot.items) { item =>
                     for {
                       persisted <- itemRepository.persist(hero.id, item)
                       added     <- inventoryRepository.addItem(hero.id, persisted).as(true)
                                      .catchAll {
                                        case InventoryRepoError.NoMorePlaceForItems => ZIO.succeed(false)
                                        case _                                      => ZIO.succeed(false)
                                      }
                     } yield
                       if (added) itemLine(persisted)
                       else content.format("loot.itemLost", "name" -> persisted.name)
                   }

      goldLines = loot.golds.map { g => content.format("loot.gold", "amount" -> g.toString) }
      lines     = goldLines ++ itemLines

      _ <- journal.append(GameEvent(user.userId, "loot_claimed",
             Json.obj("gold" -> goldTotal.asJson, "items" -> loot.items.map(_.name).asJson)))

      text = if (lines.isEmpty) content.text("loot.empty")
             else content.text("loot.header") + "\n\n" + lines.mkString("\n")
      _ <- renderer.show(user, Screen(text, content.screen("loot.enter").choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

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
        Option.when(item.evasion > 0)(s"Уклонение +${item.evasion}")
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
