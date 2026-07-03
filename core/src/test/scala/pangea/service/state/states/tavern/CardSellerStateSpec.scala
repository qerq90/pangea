package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.ItemType
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

object CardSellerStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Активное предложение при now=0 (TestClock): присутствует час, цена 150 дублонов.
  private val Price = 150L
  private val offer = CardSellerData(offerUntil = Some(3600000L), price = Some(Price), nextRollAt = Some(3600000L))

  private def makeState(doubloons: Long, canAdd: Boolean) = {
    val hero = TestFixtures.hero(userId, state = StateType.CardSeller).copy(doubloons = doubloons)
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      _        <- heroDao.writeCardSellerData(userId, offer.asJson)
      invRepo   = if (canAdd) TestInventoryRepository.accepting else TestInventoryRepository.full
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (CardSellerState(heroDao, invRepo, TestItemRepository.make, content), heroDao, invRepo, renderer)
  }

  private def sellerData(heroDao: TestHeroDao) =
    heroDao.readCardSellerData(userId).map(_.flatMap(_.as[CardSellerData].toOption).getOrElse(CardSellerData.empty))

  override def spec = suite("CardSellerState")(

    test("диалог: интро → предложение → цена с числом") {
      for {
        quad                    <- makeState(doubloons = 200L, canAdd = true)
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        _                       <- state.action(testUser, tap("WhatIsIt"), renderer)
        _                       <- state.action(testUser, tap("HowMuch"), renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.head.text.contains("Псс")) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("HowMuch"))) &&
              assertTrue(screens.last.text.contains(Price.toString)) &&
              assertTrue(screens.last.choices.map(_.id).toSet == Set("BuyCard", "DeclineCard"))
    },

    test("покупка при достатке дублонов и месте → карта в сумке, дублоны списаны, кулдаун 20ч") {
      for {
        quad                          <- makeState(doubloons = 200L, canAdd = true)
        (state, heroDao, invRepo, renderer) = quad
        result                        <- state.action(testUser, tap("BuyCard"), renderer)
        hero                          <- heroDao.getHeroByUserId(userId)
        data                          <- sellerData(heroDao)
        items                          = invRepo.snapshot
      } yield assertTrue(result == StateType.Tavern) &&
              assertTrue(hero.exists(_.doubloons == 200L - Price)) &&
              assertTrue(items.size == 1) &&
              assertTrue(items.head.itemType == ItemType.TreasureMap) &&
              assertTrue(data.offerUntil.isEmpty) &&
              assertTrue(data.nextRollAt.contains(CardSellerData.PurchaseCooldownMs)) // now=0 → 0+20ч
    },

    test("не хватает дублонов → сообщение, дублоны не списаны, остаётся у продавца") {
      for {
        quad                          <- makeState(doubloons = 100L, canAdd = true)
        (state, heroDao, invRepo, renderer) = quad
        result                        <- state.action(testUser, tap("BuyCard"), renderer)
        hero                          <- heroDao.getHeroByUserId(userId)
        screens                       <- renderer.sentScreens
      } yield assertTrue(result == StateType.CardSeller) &&
              assertTrue(hero.exists(_.doubloons == 100L)) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(screens.last.text.contains("Разбогатеешь")) &&
              assertTrue(screens.last.choices.map(_.id) == List("NeedDoubloons"))
    },

    test("полная сумка → не списывает дублоны, уводит в таверну с сообщением") {
      for {
        quad                          <- makeState(doubloons = 200L, canAdd = false)
        (state, heroDao, _, renderer)  = quad
        result                        <- state.action(testUser, tap("BuyCard"), renderer)
        hero                          <- heroDao.getHeroByUserId(userId)
        data                          <- sellerData(heroDao)
        screens                       <- renderer.sentScreens
      } yield assertTrue(result == StateType.Tavern) &&
              assertTrue(hero.exists(_.doubloons == 200L)) &&
              assertTrue(data.offerUntil.isDefined) &&            // предложение осталось
              assertTrue(screens.last.text.contains("переполнена"))
    },

    test("отказ → возвращает в таверну") {
      for {
        quad                    <- makeState(doubloons = 200L, canAdd = true)
        (state, _, _, renderer)  = quad
        result                  <- state.action(testUser, tap("DeclineCard"), renderer)
      } yield assertTrue(result == StateType.Tavern)
    }
  )
}
