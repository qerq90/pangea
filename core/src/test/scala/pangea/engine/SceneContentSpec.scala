package pangea.engine

import pangea.model.vk.keyboard.ButtonColor
import pangea.service.sender.vk.VkRenderer
import zio.ZIO
import zio.test._

object SceneContentSpec extends ZIOSpecDefault {

  override def spec = suite("SceneContent button colors")(

    test("choice из простой строки → primary") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(c.choice("Sell", "merchant.sellLabel").color == ChoiceColor.Primary)
    },

    test("choice из объекта {label, color} → negative") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        choice = c.choice("Leave", "loot.leaveLabel")
      } yield assertTrue(choice.label == "Оставить") &&
              assertTrue(choice.color == ChoiceColor.Negative)
    },

    test("choice с подстановкой args в метку") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(c.choice("Buy", "merchant.buyLabel", "n" -> "1").label == "Купить 1")
    },

    test("choices из yaml-блока тоже несут color (Сбежать → negative)") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        flee = c.screen("battle.enter").choices.find(_.id == "Flee").get
      } yield assertTrue(flee.color == ChoiceColor.Negative)
    },

    test("VkRenderer маппит ChoiceColor.Negative → ButtonColor.Negative") {
      assertTrue(VkRenderer.toButtonColor(ChoiceColor.Negative) == ButtonColor.Negative) &&
      assertTrue(VkRenderer.toButtonColor(ChoiceColor.Primary) == ButtonColor.Primary)
    },

    // Валюты различаются только смайликом, поэтому их легко перепутать местами
    // при массовой замене: серебро — 🪙 (монета), дублоны — 🟡 (золотой кружок).
    test("серебро помечено монетой, дублоны — золотым кружком") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        silverFound    = c.format("loot.silver", "amount" -> "10")
        doubloonsFound = c.format("loot.doubloons", "amount" -> "2")
      } yield assertTrue(silverFound.startsWith("🪙")) &&
              assertTrue(doubloonsFound.startsWith("🟡")) &&
              assertTrue(!silverFound.contains("🟡")) &&
              assertTrue(!doubloonsFound.contains("🪙")) &&
              // 💰 больше не используется ни для одной валюты
              assertTrue(!c.text("globalMap.enter.text").contains("💰"))
    }
  )
}
