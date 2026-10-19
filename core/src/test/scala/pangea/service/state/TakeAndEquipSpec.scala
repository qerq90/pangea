package pangea.service.state

import pangea.model.item.{Grip, Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository}
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
              assertTrue(worn.equipment == saved.map(_.equipment).getOrElse(hero.equipment)) &&
              assertTrue(inv.maxItems == 20L + PassiveKind.Stash.ExtraSlots) &&
              assertTrue(invRepo.snapshot.isEmpty)
    }
  )
}
