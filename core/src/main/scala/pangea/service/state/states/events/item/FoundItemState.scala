package pangea.service.state.states.events.item

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{jawn, Decoder, Encoder}
import pangea.dao.hero.HeroDao
import pangea.generator.item.ItemGenerator
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.item.Rarity.Blue
import pangea.model.state.StateType
import pangea.model.state.StateType.{Dungeon, FoundItem}
import pangea.model.user.User
import pangea.repository.event.EventRepository
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.service.sender.Api
import pangea.service.state.states.events.item.FoundItemState.FoundItemData
import pangea.service.state.states.events.item.keyboard.FoundItemKeyboard
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class FoundItemState(
  api: Api,
  eventRepo: EventRepository,
  heroDao: HeroDao,
  inventoryRepository: InventoryRepository
) extends State {
  private lazy val eventType = this.getClass.getSimpleName

  override def enter(user: User): Task[Unit] =
    for {
      hero <- heroDao
        .getHeroByUserId(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))

      data = FoundItemData(generateItem(hero)).asJson
      _ <- eventRepo.startEvent(user.userId, eventType, data)
      _ <- api.sendMessage(
        user,
        "Вы находите предмет",
        List.empty,
        Some(FoundItemKeyboard.keyboard)
      )
    } yield ()

  private def generateItem(hero: Hero): Item =
    ItemGenerator.createItem(hero.lvl, Blue)

  private def getAction(userAction: UserAction): Action =
    userAction.payload match {
      case Some(payload) => jawn.decode[Action](payload).toOption.get
      case None          => Action.Text
    }

  override def action(user: User, action: UserAction): Task[StateType] =
    getAction(action) match {
      case Action.TakeItem     => takeItem(user)
      case Action.DontTakeItem => dontTakeItem(user)
      case Action.Text         => ZIO.succeed(FoundItem)
    }

  private def takeItem(user: User) =
    for {
      eventData <- eventRepo.getEvent(user.userId, eventType)
      item      <- ZIO.fromEither(eventData.data.as[FoundItemData].map(_.item))
      hero <- heroDao
        .getHeroByUserId(user.userId)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))

      _ <- ZIO.logInfo(s"hero: ${hero}")

      added <- inventoryRepository
        .addItem(hero.id, item)
        .as(true)
        .tapSomeError { case InventoryRepoError.NoMorePlaceForItems =>
          api.sendMessage(
            user,
            s"В инвентаре нет места, вы уходите ни с чем.",
            List.empty,
            None
          )
        }
        .catchAll(_ => ZIO.succeed(false))

      _ <- ZIO.logInfo(s"added: $added")
      _ <- ZIO.when(added)(
        api.sendMessage(
          user,
          s"Вы получили ${item.name}: ${item.asJson.noSpaces}",
          List.empty,
          None
        )
      )
      _ <- ZIO.when(!added)(
        api.sendMessage(
          user,
          s"Вы не смогли забрать ${item.name}, вы уходите ни с чем.",
          List.empty,
          None
        )
      )
      _ <- eventRepo.endEvent(user.userId, eventType)
    } yield Dungeon

  private def dontTakeItem(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "Вы уходите, так и не забрав найденный предмет",
        List.empty,
        None
      ) *> eventRepo
      .endEvent(user.userId, eventType)
      .as(Dungeon)
}

object FoundItemState {
  case class FoundItemData(item: Item)

  object FoundItemData {
    implicit val encoder: Encoder[FoundItemData] = deriveEncoder[FoundItemData]
    implicit val decoder: Decoder[FoundItemData] = deriveDecoder[FoundItemData]
  }
}
