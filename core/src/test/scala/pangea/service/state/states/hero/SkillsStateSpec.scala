package pangea.service.state.states.hero

import pangea.engine.SceneContent
import pangea.model.hero.Equipment
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object SkillsStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def weaponWith(skill: Skill, id: Long): Item =
    Item(id, "Оружие", 1L, Rarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Weapon(skill))

  private def passiveItem(itemType: ItemType, kind: PassiveKind, id: Long): Item =
    Item(id, "Предмет", 1L, Rarity.Gray, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Passive(kind))

  // Нагрудник с активным навыком (не пассивкой) — для тестов активных умений.
  private def armorSkillItem(skill: Skill, id: Long): Item =
    Item(id, "Нагрудник", 1L, Rarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(skill))

  private def makeState(equipment: Equipment) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(equipment = equipment))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SkillsState(heroDao, content), heroDao, renderer)

  override def spec = suite("SkillsState")(

    test("enter без снаряжения с умениями → пустой список, только Назад") {
      for {
        t <- makeState(TestFixtures.emptyEquipment)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.map(_.id) == List("BackFromSkills"))
    },

    test("enter с оружием и нагрудником → 2 кнопки активных умений с их названиями") {
      val eq = TestFixtures.emptyEquipment.copy(
        weapon     = weaponWith(Skill.SweepingStrike, id = 10L),
        chestPlate = armorSkillItem(Skill.MinorHeal, id = 20L)
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
        labels   = screens.last.choices.map(_.label)
      } yield assertTrue(ids.contains("ActiveSkill_10")) &&
              assertTrue(ids.contains("ActiveSkill_20")) &&
              assertTrue(labels.contains(Skill.SweepingStrike.label)) &&
              assertTrue(labels.contains(Skill.MinorHeal.label))
    },

    test("enter с пассивным кольцом → кнопка пассивки с её названием") {
      val eq = TestFixtures.emptyEquipment.copy(
        firstRing = passiveItem(ItemType.Ring, PassiveKind.Jeweler, id = 30L)
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.exists(c =>
        c.id == "PassiveSkill_Jeweler" && c.label == PassiveKind.Jeweler.label))
    },

    test("два предмета с одной пассивкой в разных слотах → одна кнопка (дубли не стакаются)") {
      val eq = TestFixtures.emptyEquipment.copy(
        boots = passiveItem(ItemType.Boots, PassiveKind.Stash, id = 40L),
        pants = passiveItem(ItemType.Pants, PassiveKind.Stash, id = 41L)
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.count(_.id == "PassiveSkill_Stash") == 1)
    },

    test("клик по активному умению → показывает полное описание с энергией и перезарядкой") {
      val eq = TestFixtures.emptyEquipment.copy(weapon = weaponWith(Skill.SweepingStrike, id = 10L))
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.action(testUser, tap("ActiveSkill_10"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(Skill.SweepingStrike.label)) &&
              assertTrue(screens.last.text.contains("Перезарядка")) &&
              assertTrue(screens.last.choices.map(_.id) == List("SkillsList"))
    },

    test("клик по пассивке → показывает её описание") {
      val eq = TestFixtures.emptyEquipment.copy(
        firstRing = passiveItem(ItemType.Ring, PassiveKind.Jeweler, id = 30L))
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.action(testUser, tap("PassiveSkill_Jeweler"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(PassiveKind.Jeweler.describe))
    },

    test("SkillsList (Назад с детального экрана) → снова список") {
      val eq = TestFixtures.emptyEquipment.copy(weapon = weaponWith(Skill.SweepingStrike, id = 10L))
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.action(testUser, tap("ActiveSkill_10"), renderer)
        _       <- state.action(testUser, tap("SkillsList"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.exists(_.id == "ActiveSkill_10"))
    },

    test("BackFromSkills → переход в HeroStats") {
      for {
        t <- makeState(TestFixtures.emptyEquipment)
        (state, _, renderer) = t
        result <- state.action(testUser, tap("BackFromSkills"), renderer)
      } yield assertTrue(result == StateType.HeroStats)
    },

    test("11 умений/пассивок → 8 кнопок на первой странице + След., остальные на второй") {
      val eq = Equipment(
        helmet           = passiveItem(ItemType.Helmet, PassiveKind.Stealthy, id = 1L),
        shoulderPads     = passiveItem(ItemType.ShoulderPads, PassiveKind.Taxidermist, id = 2L),
        chestPlate       = armorSkillItem(Skill.MinorHeal, id = 3L),
        bracelets        = passiveItem(ItemType.Bracelets, PassiveKind.Blending, id = 4L),
        gloves           = passiveItem(ItemType.Gloves, PassiveKind.Terrifying, id = 5L),
        pants            = passiveItem(ItemType.Pants, PassiveKind.Glittering, id = 6L),
        boots            = passiveItem(ItemType.Boots, PassiveKind.Reinforced, id = 7L),
        amulet           = passiveItem(ItemType.Amulet, PassiveKind.Focused, id = 8L),
        firstRing        = passiveItem(ItemType.Ring, PassiveKind.Jeweler, id = 9L),
        secondRing       = passiveItem(ItemType.Ring, PassiveKind.Marauder, id = 10L),
        belt             = Item.NoItem,
        flask            = Item.NoItem,
        weapon           = weaponWith(Skill.SweepingStrike, id = 11L),
        additionalWeapon = Item.NoItem
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _        <- state.enter(testUser, renderer)
        screens1 <- renderer.sentScreens
        page1     = screens1.last.choices
        _        <- state.action(testUser, tap("SkillsNext"), renderer)
        screens2 <- renderer.sentScreens
        page2     = screens2.last.choices
      } yield assertTrue(page1.count(_.id.startsWith("ActiveSkill_")) + page1.count(_.id.startsWith("PassiveSkill_")) == 8) &&
              assertTrue(page1.exists(_.id == "SkillsNext")) &&
              assertTrue(!page1.exists(_.id == "SkillsPrev")) &&
              assertTrue(page2.count(_.id.startsWith("ActiveSkill_")) + page2.count(_.id.startsWith("PassiveSkill_")) == 3) &&
              assertTrue(page2.exists(_.id == "SkillsPrev")) &&
              assertTrue(!page2.exists(_.id == "SkillsNext"))
    }
  )
}
