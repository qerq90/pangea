package pangea.service.state.states.artifact

import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.artifact.{ArtifactKind, HeroArtifacts}
import pangea.model.hero.HeroId
import pangea.model.item.{Item, ItemType, MaterialKind, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.artifact.{ArtifactIntake, Intake}
import pangea.service.state.UserAction
import pangea.test._
import zio.ZIO
import zio.test._

/** Лавка Фета и её артефакты: Ларец Азата ловит камни, Живая сумка — травы и
  * отвары, оба собираются за дублоны и работают на зарядах. */
object ArtifactSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val heroId   = HeroId(1L)

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def gem(id: Long, kind: pangea.model.item.GemKind = pangea.model.item.GemKind.Ruby, grade: Int = 1): Item =
    GemGenerator.item(kind, grade).copy(id = id)

  private def herb(id: Long, kind: MaterialKind): Item = MaterialGenerator.item(kind).copy(id = id)

  private def sword(id: Long): Item =
    Item(id, "Меч", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private def shop(doubloons: Long, repo: TestArtifactRepository = TestArtifactRepository.empty) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(doubloons = doubloons))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (FetShopState(heroDao, repo, content), heroDao, repo, renderer)

  private def chest(kind: ArtifactKind, repo: TestArtifactRepository, inventory: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      invRepo   = TestInventoryRepository.withItems(inventory)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (ArtifactState(kind, heroDao, invRepo, TestItemRepository.make, repo, content), invRepo, renderer)

  override def spec = suite("Артефакты Фета")(

    suite("Лавка")(

      test("покупка за 100 дублонов: ступень 1, пятнадцать мест и полные заряды") {
        for {
          t <- shop(doubloons = 150L)
          (state, heroDao, repo, renderer) = t
          _       <- state.action(testUser, tap("FetBuyYes", "kind" -> "Casket"), renderer)
          hero    <- heroDao.getHeroByUserId(userId)
          casket   = repo.snapshot.of(ArtifactKind.Casket)
        } yield assertTrue(casket.tier == 1 && casket.capacity == 15 && casket.charges == HeroArtifacts.MaxCharges) &&
                assertTrue(hero.get.doubloons == 50L)
      },

      test("три улучшения по 100 доводят до шестидесяти мест, четвёртого нет") {
        val repo = TestArtifactRepository.of(casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1))
        for {
          t <- shop(doubloons = 400L, repo)
          (state, heroDao, _, renderer) = t
          _       <- state.action(testUser, tap("FetBuyYes", "kind" -> "Casket"), renderer)
          _       <- state.action(testUser, tap("FetBuyYes", "kind" -> "Casket"), renderer)
          _       <- state.action(testUser, tap("FetBuyYes", "kind" -> "Casket"), renderer)
          hero    <- heroDao.getHeroByUserId(userId)
          casket   = repo.snapshot.of(ArtifactKind.Casket)
          _       <- state.action(testUser, tap("FetGoods", "kind" -> "Casket"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(casket.tier == HeroArtifacts.MaxTier && casket.capacity == 60) &&
                assertTrue(hero.get.doubloons == 100L) &&
                // Собран полностью — кнопки улучшения больше нет.
                assertTrue(screens.last.choices.map(_.id) == List("FetShop"))
      },

      test("нет дублонов — нет и артефакта") {
        for {
          t <- shop(doubloons = 99L)
          (state, _, repo, renderer) = t
          _       <- state.action(testUser, tap("FetBuyYes", "kind" -> "LivingBag"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(!repo.snapshot.of(ArtifactKind.LivingBag).owned) &&
                assertTrue(screens.last.text.contains("100"))
      }
    ),

    suite("Добыча")(

      test("камень летит в ларец, трава — в сумку, меч остаётся в сумке героя") {
        val repo = TestArtifactRepository.of(
          casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1),
          bag    = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1))
        val inv  = TestInventoryRepository.accepting
        for {
          stone <- ArtifactIntake.accept(Some(repo), inv, heroId, gem(10L))
          grass <- ArtifactIntake.accept(Some(repo), inv, heroId, herb(11L, MaterialKind.Chamomile))
          blade <- ArtifactIntake.accept(Some(repo), inv, heroId, sword(12L))
        } yield assertTrue(stone == Intake.ToArtifact(ArtifactKind.Casket, 14)) &&
                assertTrue(grass == Intake.ToArtifact(ArtifactKind.LivingBag, 14)) &&
                assertTrue(blade == Intake.ToInventory && inv.snapshot.map(_.id) == List(12L)) &&
                assertTrue(repo.snapshot.of(ArtifactKind.Casket).items.data.map(_.id) == List(10L))
      },

      test("ларца нет или он полон — камень идёт в сумку, как раньше") {
        val full = TestArtifactRepository.of(casket =
          TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, items = (1L to 15L).toList.map(gem(_))))
        val inv  = TestInventoryRepository.accepting
        for {
          none  <- ArtifactIntake.accept(None, inv, heroId, gem(20L))
          stone <- ArtifactIntake.accept(Some(full), inv, heroId, gem(21L))
        } yield assertTrue(none == Intake.ToInventory && stone == Intake.ToInventory) &&
                assertTrue(inv.snapshot.map(_.id) == List(20L, 21L))
      }
    ),

    suite("Ларец Азата")(

      test("меню показывает сборку, места и заряды") {
        val repo = TestArtifactRepository.of(casket =
          TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 2, charges = 7, items = List(gem(1L))))
        for {
          t <- chest(ArtifactKind.Casket, repo)
          (state, _, renderer) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(screens.last.text.contains("2/4") && screens.last.text.contains("1/30")) &&
                assertTrue(screens.last.text.contains("7/25")) &&
                assertTrue(screens.last.choices.map(_.id) ==
                  List("ArtifactPut", "ArtifactTake", "ArtifactMagic", "LeaveArtifact"))
      },

      test("положить камень и забрать обратно") {
        val repo = TestArtifactRepository.of(casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1))
        for {
          t <- chest(ArtifactKind.Casket, repo, inventory = List(gem(5L), sword(6L)))
          (state, inv, renderer) = t
          _      <- state.action(testUser, tap("ArtifactPut"), renderer)
          list   <- renderer.sentScreens
          _      <- state.action(testUser, tap("ArtPut_5"), renderer)
          stored  = (repo.snapshot.of(ArtifactKind.Casket).items.data.map(_.id), inv.snapshot.map(_.id))
          _      <- state.action(testUser, tap("ArtTake_5"), renderer)
        } yield // меч в список не попал: ларец берёт только камни
                assertTrue(list.last.choices.map(_.id).exists(_.startsWith("ArtPut_5"))) &&
                assertTrue(!list.last.choices.map(_.id).contains("ArtPut_6")) &&
                assertTrue(stored == (List(5L), List(6L))) &&
                assertTrue(repo.snapshot.of(ArtifactKind.Casket).items.data.isEmpty) &&
                assertTrue(inv.snapshot.map(_.id).sorted == List(5L, 6L))
      },

      test("камень на крышке: три одинаковых камня → один категорией выше, минус заряд") {
        val three = List(gem(1L), gem(2L), gem(3L))
        val repo  = TestArtifactRepository.of(casket =
          TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, charges = 5, items = three))
        for {
          t <- chest(ArtifactKind.Casket, repo)
          (state, _, renderer) = t
          _      <- state.action(testUser, tap("ArtifactMagic"), renderer)
          casket  = repo.snapshot.of(ArtifactKind.Casket)
        } yield assertTrue(casket.items.data.size == 1) &&
                assertTrue(casket.items.data.head.gem.exists(g => g.grade == 2 && g.kind == pangea.model.item.GemKind.Ruby)) &&
                assertTrue(casket.charges == 4)
      },

      test("нечего плавить и нет зарядов — говорим об этом, ничего не тратя") {
        val idle = TestArtifactRepository.of(casket =
          TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, charges = 3, items = List(gem(1L))))
        val dead = TestArtifactRepository.of(casket =
          TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, charges = 0, items = List(gem(1L), gem(2L), gem(3L))))
        for {
          t <- chest(ArtifactKind.Casket, idle)
          (state, _, renderer) = t
          _        <- state.action(testUser, tap("ArtifactMagic"), renderer)
          nothing  <- renderer.sentScreens
          d <- chest(ArtifactKind.Casket, dead)
          (state2, _, renderer2) = d
          _        <- state2.action(testUser, tap("ArtifactMagic"), renderer2)
          noCharge <- renderer2.sentScreens
        } yield assertTrue(idle.snapshot.of(ArtifactKind.Casket).charges == 3) &&
                assertTrue(nothing.exists(_.text.contains("не нашлось"))) &&
                assertTrue(dead.snapshot.of(ArtifactKind.Casket).items.data.size == 3) &&
                assertTrue(noCharge.exists(_.text.contains(HeroArtifacts.RechargeSilver.toString)))
      }
    ),

    test("кристалл живой сумки варит отвар из трав") {
      val herbs = List(herb(1L, MaterialKind.Chamomile), herb(2L, MaterialKind.Calendula), herb(3L, MaterialKind.Nettle))
      val repo  = TestArtifactRepository.of(bag = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, charges = 2, items = herbs))
      for {
        t <- chest(ArtifactKind.LivingBag, repo)
        (state, _, renderer) = t
        _   <- state.action(testUser, tap("ArtifactMagic"), renderer)
        bag  = repo.snapshot.of(ArtifactKind.LivingBag)
      } yield assertTrue(bag.items.data.count(_.itemType == ItemType.Brew) == 2) &&
              assertTrue(bag.items.data.forall(_.material.forall(!_.isHerb))) &&
              assertTrue(bag.charges == 1)
    },

    test("рюкзак: кнопки артефактов появляются только у владельца") {
      val repo = TestArtifactRepository.of(casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1))
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        invRepo   = TestInventoryRepository.accepting
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = BackpackState(heroDao, invRepo, repo, content)
        _        <- state.enter(testUser, renderer)
        screens  <- renderer.sentScreens
        toCasket <- state.action(testUser, tap("OpenCasket"), renderer)
        toBag    <- state.action(testUser, tap("OpenBag"), renderer)
      } yield assertTrue(screens.last.choices.map(_.id) == List("OpenBag", "OpenCasket", "BackFromBackpack")) &&
              assertTrue(toCasket == StateType.Casket && toBag == StateType.Inventory)
    },

    suite("Миниатюрный шкаф")(

      test("три места на ступень, любые вещи со слотом и никакой магии") {
        val repo = TestArtifactRepository.of(wardrobe = TestArtifactRepository.artifact(ArtifactKind.Wardrobe, tier = 1))
        val dust = MaterialGenerator.item(MaterialKind.dustOf(pangea.model.item.GemKind.Ruby)).copy(id = 9L)
        for {
          t <- chest(ArtifactKind.Wardrobe, repo, inventory = List(sword(5L), dust))
          (state, inv, renderer) = t
          _       <- state.enter(testUser, renderer)
          menu    <- renderer.sentScreens.map(_.last)
          _       <- state.action(testUser, tap("ArtifactPut"), renderer)
          list    <- renderer.sentScreens.map(_.last)
          _       <- state.action(testUser, tap("ArtPut_5"), renderer)
          stored   = repo.snapshot.of(ArtifactKind.Wardrobe)
        } yield assertTrue(menu.text.contains("1/4") && menu.text.contains("0/3") && !menu.text.contains("Заряды")) &&
                // магии у шкафа нет — и кнопки тоже
                assertTrue(menu.choices.map(_.id) == List("ArtifactPut", "ArtifactTake", "LeaveArtifact")) &&
                // невесомая пыль слотов не занимает, поэтому в шкаф не идёт
                assertTrue(list.choices.map(_.id).contains("ArtPut_5") && !list.choices.map(_.id).contains("ArtPut_9")) &&
                assertTrue(stored.items.data.map(_.id) == List(5L) && stored.capacity == 3) &&
                assertTrue(inv.snapshot.map(_.id) == List(9L))
      },

      test("шкаф ничего не ловит с добычи сам") {
        val repo = TestArtifactRepository.of(wardrobe = TestArtifactRepository.artifact(ArtifactKind.Wardrobe, tier = 4))
        val inv  = TestInventoryRepository.accepting
        for {
          blade <- ArtifactIntake.accept(Some(repo), inv, heroId, sword(7L))
        } yield assertTrue(blade == Intake.ToInventory) &&
                assertTrue(repo.snapshot.of(ArtifactKind.Wardrobe).items.data.isEmpty)
      },

      test("покупка у Фета: 100 дублонов, три места, дальше по три за улучшение") {
        for {
          t <- shop(doubloons = 200L)
          (state, _, repo, renderer) = t
          _     <- state.action(testUser, tap("FetBuyYes", "kind" -> "Wardrobe"), renderer)
          first  = repo.snapshot.of(ArtifactKind.Wardrobe)
          _     <- state.action(testUser, tap("FetBuyYes", "kind" -> "Wardrobe"), renderer)
          second = repo.snapshot.of(ArtifactKind.Wardrobe)
        } yield assertTrue(first.tier == 1 && first.capacity == 3 && first.charges == 0) &&
                assertTrue(second.tier == 2 && second.capacity == 6)
      }
    ),

    test("цены, ступени и заряды") {
      assertTrue(HeroArtifacts.StepPriceDoubloons == 100L && HeroArtifacts.MaxTier == 4) &&
      assertTrue(ArtifactKind.Casket.slotsPerTier == 15 && ArtifactKind.LivingBag.slotsPerTier == 15) &&
      assertTrue(ArtifactKind.Wardrobe.slotsPerTier == 3 && !ArtifactKind.Wardrobe.hasMagic) &&
      assertTrue(HeroArtifacts.MaxCharges == 25 && HeroArtifacts.RechargeSilver == 5000L)
    }
  )
}
