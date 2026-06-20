package pangea.service.state.states.events.item

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Journal, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.GameEvent
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.repository.item.ItemRepository
import pangea.service.state.states.InventoryState
import pangea.service.state.states.events.item.FoundItemState.FoundItemData
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

case class FoundItemState(
  heroDao:             HeroDao,
  inventoryRepository: InventoryRepository,
  itemRepository:      ItemRepository,
  journal:             Journal,
  content:             SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "TakeItem"     -> Target.Run { (user, _, renderer) => takeItem(user, renderer) },
      "DontTakeItem" -> Target.Run { (user, _, renderer) => dontTakeItem(user, renderer) }
    ),
    fallback = Target.Run { (user, _, renderer) => showFoundScreen(user, renderer).as(StateType.FoundItem) }
  )

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- heroDao
        .getHeroByUserId(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
      seed              <- Random.nextLong
      rng0               = Rng(seed)
      (rarity, rng1)     = ItemGenerator.rarityForLevel(hero.dungeonLevel, rng0)
      item              <- itemRepository.generate(hero.id, hero.dungeonLevel.toLong, rarity, rng1)
      _                 <- heroDao.writeSceneData(user.userId, FoundItemData(item).asJson)
      _                 <- journal.append(GameEvent(user.userId, "item_found",
                             Json.obj("name" -> item.name.asJson, "rarity" -> item.rarity.toString.asJson,
                                      "hero_lvl" -> hero.lvl.asJson, "seed" -> seed.asJson)))
      _                 <- showFoundScreen(user, renderer)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showFoundScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- heroDao
        .getHeroByUserId(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
      raw  <- heroDao.readSceneData(user.userId)
      item  = raw.flatMap(_.as[FoundItemData].toOption).map(_.item)
      text  = item.fold(content.text("foundItem.header")) { i =>
        content.text("foundItem.header") + "\n\n" + InventoryState.itemText(i, hero.equipment, 1, 1)
      }
      _    <- renderer.show(user, Screen(text, content.screen("foundItem.enter").choices))
    } yield ()

  private def takeItem(user: User, renderer: Renderer): Task[StateType] =
    for {
      raw  <- heroDao.readSceneData(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No scene_data for user ${user.userId}"))
      item <- ZIO.fromEither(raw.as[FoundItemData].map(_.item))
      hero <- heroDao
        .getHeroByUserId(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
      added <- inventoryRepository
        .addItem(hero.id, item)
        .as(true)
        .tapSomeError { case InventoryRepoError.NoMorePlaceForItems =>
          renderer.show(user, Screen(content.text("foundItem.inventoryFull"), Nil))
        }
        .catchAll(_ => ZIO.succeed(false))
      _ <- journal.append(GameEvent(user.userId,
              if (added) "item_taken" else "item_cant_take",
              Json.obj("name" -> item.name.asJson)))
      _ <- ZIO.when(added)(renderer.show(user, Screen(content.format("foundItem.taken",    "name" -> item.name), Nil)))
      _ <- ZIO.when(!added)(renderer.show(user, Screen(content.format("foundItem.cantTake", "name" -> item.name), Nil)))
    } yield StateType.Dungeon

  private def dontTakeItem(user: User, renderer: Renderer): Task[StateType] =
    journal.append(GameEvent(user.userId, "item_left", Json.obj())) *>
      renderer.show(user, Screen(content.text("foundItem.left"), Nil))
        .as(StateType.Dungeon)
}

object FoundItemState {
  case class FoundItemData(item: Item)
  object FoundItemData {
    implicit val encoder: Encoder[FoundItemData] = deriveEncoder[FoundItemData]
    implicit val decoder: Decoder[FoundItemData] = deriveDecoder[FoundItemData]
  }
}
