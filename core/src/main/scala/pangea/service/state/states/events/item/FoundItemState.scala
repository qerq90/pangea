package pangea.service.state.states.events.item

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Journal, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.GameEvent
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.artifact.ArtifactRepository
import pangea.service.artifact.{ArtifactIntake, Intake}
import pangea.service.state.states.InventoryState
import pangea.service.state.states.events.item.FoundItemState.FoundItemData
import pangea.service.state.{InventoryFeedback, QuestSupport, SlotChoice, State, TakeAndEquip, UserAction}
import zio.{Random, Task, ZIO}

case class FoundItemState(
  heroDao:             HeroDao,
  inventoryRepository: InventoryRepository,
  itemRepository:      ItemRepository,
  journal:             Journal,
  content:             SceneContent,
  artifacts:           Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "TakeItem"     -> Target.Run { (user, ua, renderer) => takeItem(user, ua, renderer, wear = false) },
      "TakeWearItem" -> Target.Run { (user, ua, renderer) => takeItem(user, ua, renderer, wear = true) },
      // «Назад» с выбора кольца или руки — снова находка
      "FoundBack"    -> Target.Run { (user, _, renderer) => showFoundScreen(user, renderer).as(StateType.FoundItem) },
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
      // Редкость не зависит от этажа — только серый, белый, зелёный. Уровень
      // предмета при этом по-прежнему от этажа.
      (rarity, rng1)     = ItemGenerator.foundItemRarity(rng0)
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
        content.text("foundItem.header") + "\n\n" + InventoryState.itemText(i, hero.equipment, Some(hero))
      }
      // Надеть можно — в пустой слот или вместо надетого — «Забрать и надеть»
      // второй кнопкой, под «Забрать».
      wearBtn = item.filter(TakeAndEquip.canWear(hero, _)).map(_ => content.choice("TakeWearItem", "foundItem.takeWearLabel"))
      (take, rest) = content.screen("foundItem.enter").choices.partition(_.id == "TakeItem")
      _    <- renderer.show(user, Screen(text, take ++ wearBtn.toList ++ rest))
    } yield ()

  private def takeItem(user: User, ua: UserAction, renderer: Renderer, wear: Boolean): Task[StateType] =
    for {
      raw  <- heroDao.readSceneData(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No scene_data for user ${user.userId}"))
      item <- ZIO.fromEither(raw.as[FoundItemData].map(_.item))
      hero <- heroDao
        .getHeroByUserId(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
      // Надеть уже нельзя (кнопка со старого экрана) — находка просто ложится в сумку.
      next <- if (wear && TakeAndEquip.fits(hero, item)) wearItem(user, hero, item, None, renderer)
              else if (wear && TakeAndEquip.replaces(hero, item)) swapItem(user, ua, hero, item, renderer)
              else intoBag(user, hero, item, renderer).as(StateType.Dungeon)
    } yield next

  // Вместо надетой: оба кольца или обе руки заняты — сперва спрашиваем, что снять
  // (та же кнопка, но с номером слота); снятому нет места в сумке — не надеваем,
  // находка остаётся на экране.
  private def swapItem(user: User, ua: UserAction, hero: Hero, item: Item, renderer: Renderer): Task[StateType] = {
    val slot = payload(ua, "slot").flatMap(_.toIntOption).filter(s => s == 1 || s == 2)
    if (slot.isEmpty && SlotChoice.needed(hero.equipment, item))
      renderer.show(user, SlotChoice.screen(content, item, hero.equipment, "TakeWearItem", Map.empty,
        content.choice("FoundBack", "common.back"))).as(StateType.FoundItem)
    else
      inventoryRepository.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
        TakeAndEquip.noRoom(hero, inv, item, slot) match {
          case Some(key) =>
            renderer.show(user, Screen(content.format(key, "name" -> item.name), Nil)) *>
              showFoundScreen(user, renderer).as(StateType.FoundItem)
          case None => wearItem(user, hero, item, slot, renderer)
        }
      }
  }

  // «Забрать и надеть»: находка сразу на героя, мимо сумки; снятое — в сумку.
  private def wearItem(user: User, hero: Hero, item: Item, slot: Option[Int], renderer: Renderer): Task[StateType] =
    for {
      worn <- TakeAndEquip.wear(heroDao, inventoryRepository, user, hero, List(item), slot, Some(itemRepository))
      _    <- journal.append(GameEvent(user.userId, "item_taken",
                Json.obj("name" -> item.name.asJson, "worn" -> true.asJson,
                         "removed" -> worn.removed.map(_.name).asJson)))
      // Снятое легло в сумку — скажем, сколько в ней осталось места.
      bag  <- if (worn.removed.isEmpty) ZIO.succeed("")
              else InventoryFeedback.freeSlotsLine(inventoryRepository, content, hero.id).map("\n" + _)
      _    <- renderer.show(user, Screen(
                content.format("foundItem.takenWorn", "name" -> item.name) + TakeAndEquip.takenOffLine(content, worn) + bag,
                Nil))
      // Двенадцатая вещь набора — за собранный набор своё достижение.
      _    <- QuestSupport.grantFullSets(heroDao, content, user, worn.hero, renderer)
    } yield StateType.Dungeon

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def intoBag(user: User, hero: Hero, item: Item, renderer: Renderer): Task[Unit] =
    for {
      intake <- ArtifactIntake.accept(artifacts, inventoryRepository, hero.id, item)
      added   = intake != Intake.Refused
      _ <- journal.append(GameEvent(user.userId,
              if (added) "item_taken" else "item_cant_take",
              Json.obj("name" -> item.name.asJson)))
      // Ларец и Живая сумка могут перехватить находку — тогда и место считаем в них.
      where <- InventoryFeedback.intakeLine(inventoryRepository, content, hero.id, item, intake)
      _ <- if (added)
             renderer.show(user, Screen(content.format("foundItem.taken", "name" -> item.name) + "\n" + where, Nil))
           else
             renderer.show(user, Screen(where, Nil))
    } yield ()

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
