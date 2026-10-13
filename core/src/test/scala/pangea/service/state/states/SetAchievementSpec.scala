package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.hero.{Achievement, AchievementRates, Equipment, Hero}
import pangea.model.item.{Item, ItemSet, ItemType, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Достижения за собранные наборы: все двенадцать вещей одного набора разом —
  * своё достижение и +3% к главному стату набора навсегда. */
object SetAchievementSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def selectItem(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}$itemId"}"""))
  private def selectSlot(idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"${EquipmentState.SlotPrefix}$idx"}"""))
  private def wearItem(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"EquipWear","id":"$itemId"}"""))

  /** Индекс слота оружия в [[EquipmentState.slots]]. */
  private val WeaponSlotIdx = 12

  private def piece(id: Long, t: ItemType, set: ItemSet): Item =
    Item(id, "Предмет", 1L, Rarity.Blue, t,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, set = Some(set))

  /** Всё, кроме оружия, — из набора `set`; `n` первых слотов, не больше одиннадцати. */
  private def wearing(set: ItemSet, n: Int): Equipment = {
    val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet, ItemType.Ring, ItemType.Ring,
      ItemType.Belt)
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = piece(800L + i, t, set)
      i match {
        case 0  => eq.copy(helmet = it)
        case 1  => eq.copy(shoulderPads = it)
        case 2  => eq.copy(chestPlate = it)
        case 3  => eq.copy(bracelets = it)
        case 4  => eq.copy(gloves = it)
        case 5  => eq.copy(pants = it)
        case 6  => eq.copy(boots = it)
        case 7  => eq.copy(amulet = it)
        case 8  => eq.copy(firstRing = it)
        case 9  => eq.copy(secondRing = it)
        case _  => eq.copy(belt = it)
      }
    }
  }

  private def hero(eq: Equipment): Hero = TestFixtures.hero(userId).copy(equipment = eq)

  /** Надеть оружие набора из сумки: что сказали и какие достижения у героя. */
  private def equipFromBag(eq: Equipment, set: ItemSet, already: List[String] = Nil) =
    for {
      dao   <- TestHeroDao.withHero(userId, hero(eq).copy(achievements = already))
      inv    = TestInventoryRepository.withItems(List(piece(50L, ItemType.Weapon, set)))
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      state  = InventoryState(dao, inv, TestItemRepository.make, c, None)
      _     <- state.action(testUser, selectItem(50L), r)
      _     <- state.action(testUser, tap("Equip"), r)
      said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      after <- dao.getHeroByUserId(userId).map(_.get)
    } yield (said, after)

  override def spec = suite("Достижения за собранные наборы")(

    test("у каждого набора своё достижение со своим именем и +3% к его стату") {
      val all = ItemSet.values.toList.map(Achievement.forSet)
      assertTrue(all.distinct.size == ItemSet.values.size) &&
      assertTrue(all.map(_.title) == List("Живая крепость", "Укротитель пламени", "Кровопийца",
        "Зоркий глаз", "Свой среди крыс", "Повелитель мёртвых")) &&
      assertTrue(all.map(_.bonusLine) == List("+3% к защите", "+3% к атаке", "+3% к уклонению",
        "+3% к точности", "+3% к броне", "+3% к интеллекту")) &&
      assertTrue(AchievementRates.SetStatPct == 3L && Achievement.FullSetPieces == 12)
    },

    test("прибавки ложатся на свои статы и держатся без набора") {
      val bare = TestFixtures.hero(userId).copy(
        baseStats  = TestFixtures.hero(userId).baseStats.copy(int = 100),
        fightStats = FightStats(atk = 1000, hp = 100, armor = 0, defence = 1000,
                                evasion = 1000, accuracy = 1000, energy = 0),
        equipment  = TestFixtures.emptyEquipment.copy(chestPlate =
                       Item(70L, "Кираса", 1L, Rarity.Blue, ItemType.ChestPlate,
                         attack = 0, accuracy = 0, energy = 0, armor = 1000, defence = 0, evasion = 0)))
      val all  = bare.copy(achievements = ItemSet.values.toList.map(Achievement.forSet(_).entryName))
      val was  = bare.effectiveFightStats(0L)
      val now  = all.effectiveFightStats(0L)
      assertTrue(was.atk == 1000L && now.atk == 1030L) &&
      assertTrue(was.defence == 1000L && now.defence == 1030L) &&
      assertTrue(was.evasion == 1000L && now.evasion == 1030L) &&
      assertTrue(was.accuracy == 1000L && now.accuracy == 1030L) &&
      assertTrue(bare.maxArmor == 1000L && all.maxArmor == 1030L) &&
      assertTrue(bare.effectiveBaseStats(0L).int == 100L && all.effectiveBaseStats(0L).int == 103L) &&
      // одно достижение — один стат: защиту «Живая крепость» не трогает атака
      assertTrue(bare.copy(achievements = List(Achievement.StoneGuardSet.entryName))
        .effectiveFightStats(0L).atk == 1000L)
    },

    test("двенадцатая вещь набора из сумки приносит достижение, одиннадцатая — нет") {
      for {
        full    <- equipFromBag(wearing(ItemSet.Necromancer, 11), ItemSet.Necromancer)
        partial <- equipFromBag(wearing(ItemSet.Necromancer, 10), ItemSet.Necromancer)
        mixed   <- equipFromBag(wearing(ItemSet.Ghoul, 11), ItemSet.Necromancer)
      } yield assertTrue(full._1.contains("Вы получили достижение «Повелитель мёртвых». +3% к интеллекту.")) &&
              assertTrue(full._2.hasAchievement(Achievement.NecromancerSet)) &&
              assertTrue(!partial._1.contains("Вы получили достижение") && partial._2.achievements.isEmpty) &&
              // одиннадцать вещей одного набора и двенадцатая чужая — ни один не собран
              assertTrue(mixed._2.achievements.isEmpty)
    },

    test("достижение выдаётся один раз: уже полученное не повторяется") {
      for {
        again <- equipFromBag(wearing(ItemSet.Rat, 11), ItemSet.Rat,
                   already = List(Achievement.RatSet.entryName))
      } yield assertTrue(!again._1.contains("Вы получили достижение")) &&
              assertTrue(again._2.achievements == List(Achievement.RatSet.entryName))
    },

    test("с экрана снаряжения набор собирается так же") {
      for {
        dao   <- TestHeroDao.withHero(userId, hero(wearing(ItemSet.Hunter, 11)))
        inv    = TestInventoryRepository.withItems(List(piece(60L, ItemType.Weapon, ItemSet.Hunter)))
        r     <- TestRenderer.make
        c     <- ZIO.attempt(SceneContent.load())
        state  = EquipmentState(dao, inv, c)
        _     <- state.action(testUser, selectSlot(WeaponSlotIdx), r)
        _     <- state.action(testUser, wearItem(60L), r)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(said.contains("Вы получили достижение «Зоркий глаз»")) &&
              assertTrue(after.hasAchievement(Achievement.HunterSet))
    }
  )
}
