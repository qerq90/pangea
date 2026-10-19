package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.inventory.Inventory
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository}
import zio.ZIO
import zio.test._

object TakeAndEquipSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def item(id: Long, itemType: ItemType, lvl: Long = 1L, rarity: Rarity = Rarity.White,
                   grip: Option[Grip] = None, details: ItemDetails = ItemDetails.Plain): Item =
    Item(id, s"Вещь $id", lvl, rarity, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 3, defence = 0, evasion = 0, details = details, grip = grip)

  private def sword(id: Long)    = item(id, ItemType.Weapon, grip = Some(Grip.OneHanded))
  private def twoHand(id: Long)  = item(id, ItemType.Weapon, grip = Some(Grip.TwoHanded))
  private def shield(id: Long)   = item(id, ItemType.Shield)
  private def ring(id: Long, rarity: Rarity = Rarity.White) = item(id, ItemType.Ring, rarity = rarity)

  private val hero = TestFixtures.hero(userId)
  private def wearing(f: pangea.model.hero.Equipment => pangea.model.hero.Equipment) =
    hero.copy(equipment = f(hero.equipment))

  override def spec = suite("TakeAndEquip")(

    test("пустой слот и уровень по силам — встаёт; занятый слот или уровень выше — нет") {
      val helmet = item(1L, ItemType.Helmet)
      assertTrue(TakeAndEquip.fits(hero, helmet)) &&
      assertTrue(!TakeAndEquip.fits(hero, helmet.copy(lvl = 2L))) &&
      assertTrue(!TakeAndEquip.fits(wearing(_.copy(helmet = item(2L, ItemType.Helmet))), helmet))
    },

    test("фляга, трофей и роза в пустой слот не встают") {
      val flask  = item(1L, ItemType.Flask)
      val trophy = item(2L, ItemType.Trophy)
      val rose   = item(3L, ItemType.AdditionalWeapon, details = ItemDetails.Rose(pangea.model.item.RoseKind.values.head, 1, 1))
      assertTrue(!TakeAndEquip.fits(hero, flask)) &&
      assertTrue(!TakeAndEquip.fits(hero, trophy)) &&
      assertTrue(!TakeAndEquip.fits(hero, rose))
    },

    test("кольцо: встаёт, пока свободен хоть один из двух слотов") {
      val one  = wearing(_.copy(firstRing = ring(2L)))
      val both = wearing(_.copy(firstRing = ring(2L), secondRing = ring(3L)))
      assertTrue(TakeAndEquip.fits(hero, ring(1L))) &&
      assertTrue(TakeAndEquip.fits(one, ring(1L))) &&
      assertTrue(!TakeAndEquip.fits(both, ring(1L)))
    },

    test("руки: одноручное и щит — в свободную руку, но не рядом с двуручным; двуручное — только в пустые руки") {
      val swordInRight = wearing(_.copy(weapon = sword(2L)))
      val handsFull    = wearing(_.copy(weapon = sword(2L), leftHand = shield(3L)))
      val twoHanded    = wearing(_.copy(weapon = twoHand(2L)))
      assertTrue(TakeAndEquip.fits(swordInRight, shield(1L))) &&
      assertTrue(TakeAndEquip.fits(swordInRight, sword(1L))) &&
      assertTrue(!TakeAndEquip.fits(handsFull, shield(1L))) &&
      assertTrue(!TakeAndEquip.fits(twoHanded, shield(1L))) &&
      assertTrue(!TakeAndEquip.fits(swordInRight, twoHand(1L))) &&
      assertTrue(TakeAndEquip.fits(hero, twoHand(1L)))
    },

    test("pick: из двух вещей на один слот — более редкая, при равной редкости — старше уровнем") {
      val gray   = item(1L, ItemType.Helmet, rarity = Rarity.Gray)
      val green  = item(2L, ItemType.Helmet, rarity = Rarity.Green)
      val boots  = item(3L, ItemType.Boots)
      val levelled = hero.copy(lvl = 5L)
      val low    = item(4L, ItemType.Gloves, lvl = 2L)
      val high   = item(5L, ItemType.Gloves, lvl = 4L)
      assertTrue(TakeAndEquip.pick(hero, List(gray, green, boots)).sorted == List(1, 2)) &&
      assertTrue(TakeAndEquip.pick(levelled, List(low, high)) == List(1))
    },

    test("pick: два кольца — оба, если свободны оба слота; одно свободное — более редкое") {
      val rings = List(ring(1L), ring(2L, Rarity.Blue))
      val one   = wearing(_.copy(firstRing = ring(3L)))
      assertTrue(TakeAndEquip.pick(hero, rings).sorted == List(0, 1)) &&
      assertTrue(TakeAndEquip.pick(one, rings) == List(1))
    },

    test("pick: щит и меч к мечу в правой — в левую встаёт одна вещь") {
      val swordInRight = wearing(_.copy(weapon = sword(9L)))
      assertTrue(TakeAndEquip.pick(swordInRight, List(shield(1L), sword(2L).copy(rarity = Rarity.Green))) == List(1))
    },

    test("replaces: слот занят — встаёт вместо надетой; пустой слот, уровень выше или фляга — нет") {
      val helmet = item(1L, ItemType.Helmet)
      val busy   = wearing(_.copy(helmet = item(2L, ItemType.Helmet)))
      assertTrue(TakeAndEquip.replaces(busy, helmet) && !TakeAndEquip.fits(busy, helmet)) &&
      assertTrue(!TakeAndEquip.replaces(hero, helmet)) &&
      assertTrue(!TakeAndEquip.replaces(busy, helmet.copy(lvl = 2L))) &&
      assertTrue(!TakeAndEquip.replaces(wearing(_.copy(flask = item(3L, ItemType.Flask))), item(4L, ItemType.Flask))) &&
      // и так, и так — «можно надеть сразу»
      assertTrue(TakeAndEquip.canWear(busy, helmet) && TakeAndEquip.canWear(hero, helmet))
    },

    test("noRoom: снятому нужно место в сумке; двуручное поверх двух одноручных — на обе вещи") {
      def bag(n: Int, cap: Long = 20L) =
        Inventory(1L, hero.id, cap, Inventory.Items((1 to n).toList.map(i => item(100L + i, ItemType.Boots))))
      val busy  = wearing(_.copy(helmet = item(2L, ItemType.Helmet)))
      val armed = wearing(_.copy(weapon = sword(2L), leftHand = shield(3L)))
      assertTrue(TakeAndEquip.noRoom(busy, bag(19), item(1L, ItemType.Helmet), None).isEmpty) &&
      assertTrue(TakeAndEquip.noRoom(busy, bag(20), item(1L, ItemType.Helmet), None).contains("common.swapNoRoom")) &&
      assertTrue(TakeAndEquip.noRoom(armed, bag(18), twoHand(1L), None).isEmpty) &&
      assertTrue(TakeAndEquip.noRoom(armed, bag(19), twoHand(1L), None).contains("common.swapNoRoom"))
    },

    test("noRoom: замена снимает последний «Тайник» и ужимает сумку — свой отказ") {
      val stash = wearing(_.copy(boots = item(2L, ItemType.Boots, details = ItemDetails.Passive(PassiveKind.Stash))))
      val inv   = Inventory(1L, hero.id, 30L, Inventory.Items((1 to 25).toList.map(i => item(100L + i, ItemType.Gloves))))
      assertTrue(TakeAndEquip.noRoom(stash, inv, item(1L, ItemType.Boots), None).contains("equipment.stashBlocked"))
    },

    test("SlotChoice: спрашиваем при двух кольцах и двух занятых руках, но не про двуручное") {
      val rings     = wearing(_.copy(firstRing = ring(2L), secondRing = ring(3L)))
      val oneRing   = wearing(_.copy(firstRing = ring(2L)))
      val handsFull = wearing(_.copy(weapon = sword(2L), leftHand = shield(3L)))
      val twoHanded = wearing(_.copy(weapon = twoHand(2L)))
      assertTrue(SlotChoice.needed(rings.equipment, ring(1L))) &&
      assertTrue(!SlotChoice.needed(oneRing.equipment, ring(1L))) &&
      assertTrue(SlotChoice.needed(handsFull.equipment, shield(1L))) &&
      assertTrue(!SlotChoice.needed(handsFull.equipment, twoHand(1L))) &&
      assertTrue(!SlotChoice.needed(twoHanded.equipment, shield(1L)))
    },

    test("SlotChoice.screen: по кнопке на слот с номером в данных, последней — «Назад»") {
      val rings = wearing(_.copy(firstRing = ring(2L), secondRing = ring(3L)))
      for {
        content <- ZIO.attempt(SceneContent.load())
        screen   = SlotChoice.screen(content, ring(1L), rings.equipment, "Pick", Map("i" -> "0"),
                     content.choice("Back", "common.back"))
      } yield assertTrue(screen.choices.map(_.id) == List("Pick", "Pick", "Back")) &&
              assertTrue(screen.choices.take(2).map(_.data) == List(Map("i" -> "0", "slot" -> "1"), Map("i" -> "0", "slot" -> "2"))) &&
              assertTrue(screen.text.contains("Вещь 2") && screen.text.contains("Вещь 3"))
    },

    test("wear со слотом: второе кольцо сменилось, снятое — в сумке и в Worn.removed") {
      val rings = wearing(_.copy(firstRing = ring(2L), secondRing = ring(3L)))
      for {
        heroDao <- TestHeroDao.withHero(userId, rings)
        invRepo  = TestInventoryRepository.accepting
        worn    <- TakeAndEquip.wear(heroDao, invRepo, testUser, rings, List(ring(1L)), Some(2))
        saved   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(saved.exists(h => h.equipment.firstRing.id == 2L && h.equipment.secondRing.id == 1L)) &&
              assertTrue(worn.removed.map(_.id) == List(3L)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(3L)) &&
              // броня снятого кольца ушла, новое дало свою
              assertTrue(saved.exists(_.fightStats.armor == rings.fightStats.armor))
    },

    test("wear: снаряжение и статы сохранены, «Тайник» прибавил сумке места, в сумку ничего не легло") {
      val stash  = item(7L, ItemType.Boots, details = ItemDetails.Passive(PassiveKind.Stash))
      val helmet = item(8L, ItemType.Helmet)
      for {
        heroDao <- TestHeroDao.withHero(userId, hero)
        invRepo  = TestInventoryRepository.accepting
        worn    <- TakeAndEquip.wear(heroDao, invRepo, testUser, hero, List(stash, helmet))
        saved   <- heroDao.getHeroByUserId(userId)
        inv     <- invRepo.get(hero.id)
      } yield assertTrue(saved.exists(_.equipment.boots.id == 7L)) &&
              assertTrue(saved.exists(_.equipment.helmet.id == 8L)) &&
              assertTrue(saved.exists(_.fightStats.armor == hero.fightStats.armor + 6L)) &&
              assertTrue(worn.hero.equipment == saved.map(_.equipment).getOrElse(hero.equipment)) &&
              assertTrue(worn.removed.isEmpty) &&
              assertTrue(inv.maxItems == 20L + PassiveKind.Stash.ExtraSlots) &&
              assertTrue(invRepo.snapshot.isEmpty)
    }
  )
}
