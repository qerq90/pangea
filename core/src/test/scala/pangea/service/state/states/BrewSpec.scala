package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{CubeCraft, MaterialGenerator}
import pangea.model.hero.{AzatState, Hero}
import pangea.model.item._
import pangea.model.state.StateType
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{HerbLore, UserAction}
import pangea.service.state.states.tavern.InnkeeperState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Отвары: рецепты в кубе и что даёт каждый глоток. Из простых трав выходит
  * по две склянки, из редких второго ранга — по одной. */
object BrewSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def selectItem(id: Long): UserAction =
    UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}$id"}"""))
  private def pickTrauma(name: String): UserAction =
    UserAction("", Some(s"""{"action":"CureTraumaPick","trauma":"$name"}"""))
  private def pickRace(race: pangea.model.monster.Race): UserAction =
    UserAction("", Some(s"""{"action":"SentencePick","race":"${race.entryName}"}"""))

  private def herb(kind: MaterialKind, id: Long): Item = MaterialGenerator.item(kind).copy(id = id)
  private def brew(kind: BrewKind, id: Long): Item     = BrewKind.item(kind).copy(id = id)

  private def flask(charges: Int, max: Int = 6): Item =
    Item(1L, "Фляга", 1L, Rarity.Gray, ItemType.Flask,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = charges, maxCharges = max))

  private val baseHero = TestFixtures.hero(userId)

  private def inventory(hero: Hero, items: List[Item]) =
    for {
      dao <- TestHeroDao.withHero(userId, hero)
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (InventoryState(dao, inv, TestItemRepository.make, c), dao, inv, r)

  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Отвары из трав")(

    // ── Куб ───────────────────────────────────────────────────────────────────
    test("три травы по рецепту → отвар; каждый рецепт — своя тройка, порядок не важен") {
      val rng = Rng(7L)
      val boneSetter = List(herb(MaterialKind.Nettle, 1L), herb(MaterialKind.Chamomile, 2L), herb(MaterialKind.Calendula, 3L))
      val result     = CubeCraft.craft(boneSetter, charges = 5, rng)
      // Перегонке нужна готовая склянка — она варится не из одних трав (см. отдельный тест).
      val fromHerbs  = BrewKind.values.filter(_.base.isEmpty)
      val all        = fromHerbs.map(k => CubeCraft.craft(k.recipe.zipWithIndex.map { case (h, i) => herb(h, i.toLong + 1L) }, charges = 5, rng))
      // один рецепт — две порции, заряд один
      assertTrue(result.chargesUsed == 1 && result.items.size == 2) &&
        assertTrue(result.items.forall(_.brew.contains(BrewKind.BoneSetter))) &&
        assertTrue(result.items.head.itemType == ItemType.Brew && !ItemType.equippable.contains(ItemType.Brew)) &&
        // все восемь рецептов варятся
        assertTrue(all.zip(fromHerbs).forall { case (r, k) => r.chargesUsed == 1 && r.items.head.brew.contains(k) }) &&
        // рецепты не совпадают между собой
        assertTrue(fromHerbs.map(_.recipe.toSet).distinct.size == fromHerbs.size)
    },

    test("редкая трава в рецепте → одна склянка вместо двух") {
      val rng = Rng(7L)
      def brewFrom(kind: BrewKind) =
        CubeCraft.craft(kind.recipe.zipWithIndex.map { case (h, i) => herb(h, i.toLong + 1L) }, charges = 5, rng)
      val mix    = brewFrom(BrewKind.MushroomMix)
      val call   = brewFrom(BrewKind.WolfCall)
      val mirror = brewFrom(BrewKind.MirrorBrew)
      val simple = brewFrom(BrewKind.BoneSetter)
      assertTrue(List(mix, call, mirror).forall(r => r.chargesUsed == 1 && r.items.size == 1)) &&
      assertTrue(mix.items.head.brew.contains(BrewKind.MushroomMix)) &&
      assertTrue(call.items.head.brew.contains(BrewKind.WolfCall)) &&
      assertTrue(mirror.items.head.brew.contains(BrewKind.MirrorBrew)) &&
      assertTrue(simple.items.size == 2) &&
      // все три — из трав второго ранга, и каждая трава своя
      assertTrue(List(BrewKind.MushroomMix, BrewKind.WolfCall, BrewKind.MirrorBrew)
        .flatMap(_.recipe.filter(_.herbRank == 2)) ==
        List(MaterialKind.GlaiveMushroom, MaterialKind.WolfHops, MaterialKind.MirageFlower))
    },

    test("перегонка: шнапс с травами даёт самогон, из одних трав он не выходит") {
      val rng    = Rng(7L)
      val herbs  = BrewKind.Moonshine.recipe.zipWithIndex.map { case (h, i) => herb(h, i.toLong + 1L) }
      val withIt = CubeCraft.craft(brew(BrewKind.Schnapps, 9L) :: herbs, charges = 5, rng)
      val plain  = CubeCraft.craft(herbs, charges = 5, rng)
      // кристалл Живой сумки умеет и перегонку
      val inBag  = CubeCraft.brewHerbs(brew(BrewKind.Schnapps, 9L) :: herbs, charges = 5, rng)
      assertTrue(BrewKind.Moonshine.base.contains(BrewKind.Schnapps)) &&
      assertTrue(withIt.chargesUsed == 1 && withIt.items.count(_.brew.contains(BrewKind.Moonshine)) == 1) &&
      // склянку шнапса перегонка забрала
      assertTrue(!withIt.items.exists(_.brew.contains(BrewKind.Schnapps))) &&
      assertTrue(inBag.items.exists(_.brew.contains(BrewKind.Moonshine))) &&
      // без шнапса эти травы уходят на обычные отвары, самогона нет
      assertTrue(!plain.items.exists(_.brew.contains(BrewKind.Moonshine)))
    },

    test("волчье пиво: одна склянка и +10% к выносливости на час") {
      val rng  = Rng(7L)
      val beer = CubeCraft.craft(
        BrewKind.WolfBeer.recipe.zipWithIndex.map { case (h, i) => herb(h, i.toLong + 1L) }, charges = 5, rng)
      for {
        t <- inventory(baseHero, List(brew(BrewKind.WolfBeer, 1L)))
        (state, dao, _, r) = t
        _    <- state.action(testUser, selectItem(1L), r)
        _    <- state.action(testUser, tap("DrinkBrew"), r)
        hero <- dao.getHeroByUserId(userId).map(_.get)
        now  <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      } yield assertTrue(beer.items.size == 1 && beer.items.head.brew.contains(BrewKind.WolfBeer)) &&
              // выносливость до сих пор не бустил ни один отвар
              assertTrue(hero.statBoosts.vitFactor(now) > 1.0) &&
              assertTrue(hero.statBoosts.strFactor(now) == 1.0 && hero.statBoosts.agiFactor(now) == 1.0)
    },

    test("пузырьковый нектар: плёнка ложится без срока и ждёт своего часа") {
      for {
        t <- inventory(baseHero, List(brew(BrewKind.Bubble, 1L)))
        (state, dao, inv, r) = t
        _    <- state.action(testUser, selectItem(1L), r)
        _    <- state.action(testUser, tap("DrinkBrew"), r)
        said <- texts(r)
        hero <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(hero.statBoosts.hasActive(BrewRates.BubbleBoost, 0L)) &&
              assertTrue(said.contains("плёнкой") && inv.snapshot.isEmpty) &&
              // и ничего не прибавляет к характеристикам
              assertTrue(hero.statBoosts.vitFactor(0L) == 1.0)
    },

    test("зелье приговора: сперва называют род, и только потом склянка пустеет") {
      for {
        t <- inventory(baseHero, List(brew(BrewKind.Sentence, 1L)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, selectItem(1L), r)
        _     <- state.action(testUser, tap("DrinkBrew"), r)
        ask   <- r.sentScreens.map(_.last)
        // пока род не назван, склянка на месте
        kept   = inv.snapshot.size
        _     <- state.action(testUser, pickRace(pangea.model.monster.Race.Orc), r)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        now   <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        said  <- texts(r)
      } yield assertTrue(ask.text.contains("Назовите род") && ask.choices.count(_.id == "SentencePick") == 8) &&
              assertTrue(ask.choices.flatMap(_.row).groupBy(identity).forall(_._2.size <= 5)) &&
              assertTrue(kept == 1 && inv.snapshot.isEmpty) &&
              assertTrue(hero.statBoosts.hasActive(BrewRates.SentenceBoost + pangea.model.monster.Race.Orc.entryName, now)) &&
              assertTrue(!hero.statBoosts.hasActive(BrewRates.SentenceBoost + pangea.model.monster.Race.Orc.entryName, now + BrewRates.SentenceMs)) &&
              assertTrue(said.contains("приговор") || said.contains("след"))
    },

    test("зеркальный настой выпивается и оставляет копии до ближайшего боя") {
      for {
        t <- inventory(baseHero, List(brew(BrewKind.MirrorBrew, 1L)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, selectItem(1L), r)
        card  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("DrinkBrew"), r)
        said  <- texts(r)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(card.choices.map(_.id).contains("DrinkBrew")) &&
              assertTrue(hero.weaponDust.mirrors == BrewRates.MirrorCopies) &&
              assertTrue(said.contains("отражения") && inv.snapshot.isEmpty)
    },

    test("волчий зов держится час и ничего не прибавляет к характеристикам") {
      for {
        t <- inventory(baseHero, List(brew(BrewKind.WolfCall, 1L)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, selectItem(1L), r)
        _     <- state.action(testUser, tap("DrinkBrew"), r)
        said  <- texts(r)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        now   <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      } yield assertTrue(hero.statBoosts.hasActive(BrewRates.WolfCallBoost, now)) &&
              assertTrue(!hero.statBoosts.hasActive(BrewRates.WolfCallBoost, now + BrewRates.WolfCallMs)) &&
              assertTrue(hero.statBoosts.strFactor(now) == 1.0 && hero.statBoosts.agiFactor(now) == 1.0) &&
              assertTrue(said.contains("запах идёт от вас") && inv.snapshot.isEmpty) &&
              // снять его можно и досрочно — так он и сгорает на встрече
              assertTrue(!hero.statBoosts.without(BrewRates.WolfCallBoost).hasActive(BrewRates.WolfCallBoost, now))
    },

    test("грибную смесь не пьют: у неё нет кнопки «Выпить»") {
      for {
        t <- inventory(baseHero, List(brew(BrewKind.MushroomMix, 1L)))
        (state, _, inv, r) = t
        _    <- state.action(testUser, selectItem(1L), r)
        card <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("DrinkBrew"), r)
      } yield assertTrue(!card.choices.map(_.id).contains("DrinkBrew")) &&
              assertTrue(card.text.contains("бросают под ноги")) &&
              // склянка на месте: выпить её не вышло
              assertTrue(inv.snapshot.size == 1)
    },

    test("травы под три разных отвара в одной куче → три разных отвара, а не три одинаковых") {
      val rng  = Rng(7L)
      val kinds = List[BrewKind](BrewKind.BoneSetter, BrewKind.LivingWater, BrewKind.Invigorating)
      val pool  = kinds.zipWithIndex.flatMap { case (k, i) => k.recipe.zipWithIndex.map { case (h, j) => herb(h, (i * 10 + j + 1).toLong) } }
      val result = CubeCraft.craft(pool, charges = 50, rng)
      // и все десять рецептов разом
      val all    = BrewKind.rank1.toList.zipWithIndex.flatMap { case (k, i) => k.recipe.zipWithIndex.map { case (h, j) => herb(h, (i * 10 + j + 1).toLong) } }
      val allRes = CubeCraft.craft(all, charges = 50, rng)
      assertTrue(result.chargesUsed == 3 && result.items.size == 6 && result.items.flatMap(_.brew).toSet == kinds.toSet) &&
        assertTrue(allRes.chargesUsed == BrewKind.rank1.size && allRes.items.flatMap(_.brew).toSet == BrewKind.rank1.toSet)
    },

    test("не по рецепту (две травы, чужой набор) — куб гудит; шесть трав — два отвара") {
      val rng   = Rng(7L)
      val two   = CubeCraft.craft(List(herb(MaterialKind.Nettle, 1L), herb(MaterialKind.Chamomile, 2L)), charges = 5, rng)
      val wrong = CubeCraft.craft(List(herb(MaterialKind.Nettle, 1L), herb(MaterialKind.Nettle, 2L), herb(MaterialKind.Nettle, 3L)), charges = 5, rng)
      val six   = CubeCraft.craft(
        List(herb(MaterialKind.Nettle, 1L), herb(MaterialKind.Chamomile, 2L), herb(MaterialKind.Calendula, 3L),
             herb(MaterialKind.Sage, 4L), herb(MaterialKind.Chamomile, 5L), herb(MaterialKind.Nettle, 6L)), charges = 5, rng)
      assertTrue(two.chargesUsed == 0 && wrong.chargesUsed == 0) &&
        assertTrue(six.chargesUsed == 2 && six.items.size == 4 &&
                   six.items.flatMap(_.brew).toSet == Set[BrewKind](BrewKind.BoneSetter, BrewKind.LivingWater))
    },

    // ── Инвентарь ─────────────────────────────────────────────────────────────
    test("карточка отвара: описание, рецепт, «Выпить»; у сонного дурмана и шнапса кнопки нет") {
      for {
        t <- inventory(baseHero, List(brew(BrewKind.LivingWater, 10L), brew(BrewKind.SleepingDope, 11L), brew(BrewKind.Schnapps, 12L)))
        (state, _, _, r) = t
        _     <- state.action(testUser, selectItem(10L), r)
        water <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, selectItem(11L), r)
        dope  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, selectItem(12L), r)
        booze <- r.sentScreens.map(_.last)
        // Рецепта в карточке нет: из чего сварен отвар — не дело описания.
      } yield assertTrue(water.text.contains("Живая вода") && !water.text.contains("Рецепт")) &&
              // фляги нет — заправлять нечего, кнопки нет
              assertTrue(water.choices.map(_.id) == List("Drop", "InventoryList")) &&
              assertTrue(dope.text.contains("Сонный дурман") && dope.text.contains("валят с ног быка")) &&
              assertTrue(dope.choices.map(_.id) == List("Drop", "InventoryList") && booze.choices.map(_.id) == List("Drop", "InventoryList")) &&
              assertTrue(booze.text.contains("400 серебра"))
    },

    test("живая вода заправляет флягу целителя; полную — не тратится; без фляги — не тратится") {
      val withFlask = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(flask = flask(charges = 1)))
      for {
        t <- inventory(withFlask, List(brew(BrewKind.LivingWater, 10L), brew(BrewKind.LivingWater, 11L)))
        (state, dao, inv, r) = t
        _   <- state.action(testUser, selectItem(11L), r)
        _   <- state.action(testUser, tap("RefillByBrew"), r)
        h1  <- dao.getHeroByUserId(userId).map(_.get)
        after <- r.sentScreens.map(_.last)
        _   <- state.action(testUser, tap("RefillByBrew"), r)   // фляга уже полна
        log <- texts(r)
        t2 <- inventory(baseHero, List(brew(BrewKind.LivingWater, 10L)))
        (state2, _, inv2, r2) = t2
        _   <- state2.action(testUser, selectItem(10L), r2)
        _   <- state2.action(testUser, tap("RefillByBrew"), r2)
        log2 <- texts(r2)
      } yield assertTrue(h1.equipment.flask.details == ItemDetails.Flask(FlaskEffect.HealPercent(25), 6, 6)) &&
              assertTrue(inv.snapshot.size == 1 && after.choices.exists(_.id == "RefillByBrew")) && // остались на карточке стопки
              assertTrue(log.contains("полна до краёв") && log.contains("и так полна") && inv.snapshot.size == 1) &&
              assertTrue(log2.contains("Фляги нет") && inv2.snapshot.size == 1)
    },

    test("живая вода заправляет флягу кузнеца, но не дымную; дымную заправляет сонный дурман; кнопка только по толку") {
      def flaskOf(kind: FlaskKind) = pangea.generator.item.FlaskGenerator.item(kind, Rarity.Blue).copy(id = 1L).copy(
        details = ItemDetails.Flask(kind.effect, 1, 6))
      def withFlask(kind: FlaskKind) = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(flask = flaskOf(kind)))
      for {
        t <- inventory(withFlask(FlaskKind.Smith), List(brew(BrewKind.LivingWater, 10L), brew(BrewKind.SleepingDope, 11L)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, selectItem(11L), r)
        dope  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, selectItem(10L), r)
        water <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("RefillByBrew"), r)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        t2 <- inventory(withFlask(FlaskKind.Smoke), List(brew(BrewKind.LivingWater, 10L), brew(BrewKind.SleepingDope, 11L)))
        (state2, dao2, inv2, r2) = t2
        _     <- state2.action(testUser, selectItem(10L), r2)
        water2 <- r2.sentScreens.map(_.last)
        _     <- state2.action(testUser, tap("RefillByBrew"), r2)   // не по толку — не тратится
        _     <- state2.action(testUser, selectItem(11L), r2)
        dope2 <- r2.sentScreens.map(_.last)
        _     <- state2.action(testUser, tap("RefillByBrew"), r2)
        h2    <- dao2.getHeroByUserId(userId).map(_.get)
        log2  <- texts(r2)
      } yield assertTrue(!dope.choices.exists(_.id == "RefillByBrew") && water.choices.exists(_.id == "RefillByBrew")) &&
              assertTrue(h.equipment.flask.details == ItemDetails.Flask(FlaskKind.Smith.effect, 6, 6) && inv.snapshot.map(_.id) == List(11L)) &&
              assertTrue(!water2.choices.exists(_.id == "RefillByBrew") && dope2.choices.exists(_.id == "RefillByBrew")) &&
              assertTrue(log2.contains("такую флягу не заправить")) &&
              assertTrue(h2.equipment.flask.details == ItemDetails.Flask(FlaskKind.Smoke.effect, 6, 6) && inv2.snapshot.map(_.id) == List(10L))
    },

    test("ядовитая и кровавая смазки: мажут оружие на ближайший бой и заправляют флягу своего толка") {
      val poisonFlask = pangea.generator.item.FlaskGenerator.item(FlaskKind.Poison, Rarity.Blue).copy(id = 1L,
        details = ItemDetails.Flask(FlaskKind.Poison.effect, 2, 6))
      val armed = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(flask = poisonFlask,
        weapon = Item(50L, "Меч", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)))
      for {
        t <- inventory(armed, List(brew(BrewKind.VenomSalve, 10L), brew(BrewKind.VenomSalve, 11L), brew(BrewKind.BloodSalve, 12L)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, selectItem(11L), r)
        card  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("CoatWeapon"), r)
        h1    <- dao.getHeroByUserId(userId).map(_.get)
        _     <- state.action(testUser, tap("RefillByBrew"), r)      // остались на карточке стопки — вторая мазь идёт во флягу
        h2    <- dao.getHeroByUserId(userId).map(_.get)
        left   = inv.snapshot.map(_.id)
        _     <- state.action(testUser, selectItem(12L), r)
        blood <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("CoatWeapon"), r)
        h3    <- dao.getHeroByUserId(userId).map(_.get)
        log   <- texts(r)
        // без оружия мазать нечего
        t2 <- inventory(baseHero, List(brew(BrewKind.BloodSalve, 12L)))
        (state2, dao2, inv2, r2) = t2
        _     <- state2.action(testUser, selectItem(12L), r2)
        _     <- state2.action(testUser, tap("CoatWeapon"), r2)
        h4    <- dao2.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(card.choices.map(_.id) == List("CoatWeapon", "RefillByBrew", "Drop", "InventoryList")) &&
              assertTrue(h1.weaponDust.coat.contains(pangea.model.hero.WeaponCoat.Poison) && log.contains("смазано ядом")) &&
              assertTrue(h2.equipment.flask.details == ItemDetails.Flask(FlaskKind.Poison.effect, 6, 6)) &&
              assertTrue(left == List(12L)) &&
              // кровавая: фляга яда ей не по толку — кнопки заправки нет, мажет поверх яда
              assertTrue(blood.choices.map(_.id) == List("CoatWeapon", "Drop", "InventoryList")) &&
              assertTrue(h3.weaponDust.coat.contains(pangea.model.hero.WeaponCoat.Bleed) && log.contains("кровавой мазью")) &&
              assertTrue(h4.weaponDust.coat.isEmpty && inv2.snapshot.size == 1)
    },

    test("бодрящий сбор даёт +5 быстрых отдыхов") {
      for {
        t <- inventory(baseHero, List(brew(BrewKind.Invigorating, 10L)))
        (state, dao, inv, r) = t
        _    <- dao.writeAzatData(userId, AzatState(instantRests = 2).asJson)
        _    <- state.action(testUser, selectItem(10L), r)
        _    <- state.action(testUser, tap("DrinkBrew"), r)
        azat <- dao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).get)
        log  <- texts(r)
      } yield assertTrue(azat.instantRests == 7 && inv.snapshot.isEmpty && log.contains("+5 быстрых отдыхов"))
    },

    test("настой ясного глаза: +10% ловкости на час, складывается с зельем Густаво") {
      val boosted = baseHero.copy(statBoosts = pangea.model.stats.StatBoosts.none.add(
        pangea.model.stats.StatBoost("gustavo:agi", pangea.model.stats.ParamsBuff(0, 0, 15, 0), 999999999L), 0L))
      for {
        t <- inventory(boosted, List(brew(BrewKind.ClearEye, 10L)))
        (state, dao, _, r) = t
        _   <- state.action(testUser, selectItem(10L), r)
        _   <- state.action(testUser, tap("DrinkBrew"), r)
        h   <- dao.getHeroByUserId(userId).map(_.get)
        log <- texts(r)
      } yield assertTrue(h.statBoosts.hasActive("herb:agi", 0L) && h.statBoosts.agiFactor(0L) == 1.25) &&
              assertTrue(h.statBoosts.remainingMs("herb:agi", 0L).contains(BrewRates.BoostDurationMs)) &&
              assertTrue(log.contains("+10% к «Ловкость» на час"))
    },

    test("костоправный отвар: выбор травмы, тяжёлая только перечислена; лечит выбранную") {
      val hurt = baseHero.copy(traumaUntil = Some(999999999L),
        traumaNames = List(Trauma.BruisedLeg.name, Trauma.SplitSkull.name, Trauma.BrokenNose.name))
      for {
        t <- inventory(hurt, List(brew(BrewKind.BoneSetter, 10L)))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, selectItem(10L), r)
        _     <- state.action(testUser, tap("DrinkBrew"), r)
        ask   <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, pickTrauma(Trauma.BrokenNose.name), r)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        log   <- texts(r)
      } yield assertTrue(ask.text.contains("Какую травму лечить") && ask.text.contains("Тяжёлые отвар не берёт: Расколотый череп")) &&
              assertTrue(ask.choices.filter(_.id == "CureTraumaPick").map(_.label) == List(Trauma.BruisedLeg.name, Trauma.BrokenNose.name)) &&
              assertTrue(h.traumaNames == List(Trauma.BruisedLeg.name, Trauma.SplitSkull.name) && h.traumaUntil.isDefined) &&
              assertTrue(inv.snapshot.isEmpty && log.contains("травма «Сломанный нос» прошла"))
    },

    test("костоправный отвар: без травм и с одними тяжёлыми не тратится; тяжёлую подсунуть нельзя") {
      val heavy = baseHero.copy(traumaUntil = Some(999999999L), traumaNames = List(Trauma.SplitSkull.name))
      for {
        t <- inventory(baseHero, List(brew(BrewKind.BoneSetter, 10L)))
        (state, _, inv, r) = t
        _   <- state.action(testUser, selectItem(10L), r)
        _   <- state.action(testUser, tap("DrinkBrew"), r)
        log <- texts(r)
        t2 <- inventory(heavy, List(brew(BrewKind.BoneSetter, 10L)))
        (state2, dao2, inv2, r2) = t2
        _   <- state2.action(testUser, selectItem(10L), r2)
        _   <- state2.action(testUser, tap("DrinkBrew"), r2)
        _   <- state2.action(testUser, pickTrauma(Trauma.SplitSkull.name), r2)
        h2  <- dao2.getHeroByUserId(userId).map(_.get)
        log2 <- texts(r2)
      } yield assertTrue(log.contains("Травм нет") && inv.snapshot.size == 1) &&
              assertTrue(log2.contains("Все ваши травмы тяжёлые") && inv2.snapshot.size == 1) &&
              assertTrue(h2.traumaNames == List(Trauma.SplitSkull.name))
    },

    // ── Достижение ────────────────────────────────────────────────────────────
    test("«Зельевар I»: сварил по одному каждый отвар первого ранга → достижение и +2 к интеллекту, один раз") {
      import pangea.model.hero.{Achievement, AzatState, CubeStatus}
      import pangea.service.state.states.temple.CubeState
      def cubeWith(items: List[Item], h: Hero) =
        for {
          dao <- TestHeroDao.withHero(userId, h)
          _   <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 50, cubeItems = items).asJson)
          r   <- TestRenderer.make
          c   <- ZIO.attempt(SceneContent.load())
        } yield (CubeState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c), dao, r)
      def herbsFor(k: BrewKind) = k.recipe.zipWithIndex.map { case (h, j) => herb(h, (j + 1).toLong) }
      // травы под все рецепты, кроме последнего, одной кучей — куб сварит их за одну активацию
      val allButLast = BrewKind.rank1.init.toList.zipWithIndex.flatMap { case (k, i) =>
        k.recipe.zipWithIndex.map { case (h, j) => herb(h, (i * 10 + j + 1).toLong) }
      }
      for {
        t <- cubeWith(allButLast, baseHero)
        (state, dao, r) = t
        _     <- state.action(testUser, tap("CubeActivate"), r)
        h1    <- dao.getHeroByUserId(userId).map(_.get)
        lore1 <- HerbLore.readLore(dao, userId)
        // последний рецепт — и достижение
        azat  <- dao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).get)
        _     <- dao.writeAzatData(userId, azat.copy(cubeItems = herbsFor(BrewKind.rank1.last)).asJson)
        _     <- state.action(testUser, tap("CubeActivate"), r)
        h2    <- dao.getHeroByUserId(userId).map(_.get)
        log   <- texts(r)
        // повторная варка достижение не дублирует
        _     <- dao.writeAzatData(userId, azat.copy(cubeItems = herbsFor(BrewKind.rank1.last)).asJson)
        _     <- state.action(testUser, tap("CubeActivate"), r)
        h3    <- dao.getHeroByUserId(userId).map(_.get)
        log3  <- texts(r)
        intBefore = h1.effectiveBaseStats(0L).int
        intAfter  = h2.effectiveBaseStats(0L).int
      } yield assertTrue(lore1.brewed.size == BrewKind.rank1.size - 1 && !h1.hasAchievement(Achievement.Brewer1)) &&
              assertTrue(h2.hasAchievement(Achievement.Brewer1)) &&
              assertTrue(log.contains("достижение «Зельевар I»") && log.contains("+2 к интеллекту")) &&
              assertTrue(intAfter == intBefore + 2L) &&
              assertTrue(h3.achievements.count(_ == Achievement.Brewer1.entryName) == 1 &&
                         log3.split("Зельевар I").length == 2)
    },

    // ── Трактирщик ────────────────────────────────────────────────────────────
    test("шнапс из красавки: у Трактирщика кнопка со счётом, продаётся весь разом по 400") {
      val items = List(brew(BrewKind.Schnapps, 10L), brew(BrewKind.Schnapps, 11L), brew(BrewKind.SleepingDope, 12L))
      for {
        dao <- TestHeroDao.withHero(userId, baseHero.copy(silver = 50L))
        inv  = TestInventoryRepository.withItems(items)
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        state = InnkeeperState(dao, inv, c)
        _     <- state.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        out   <- state.action(testUser, tap("SellSchnapps"), r)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        log   <- texts(r)
        after <- r.sentScreens.map(_.last)
      } yield assertTrue(menu.choices.exists(ch => ch.id == "SellSchnapps" && ch.label == "Продать шнапс (2 шт)")) &&
              assertTrue(out == StateType.Innkeeper && h.silver == 50L + 800L) &&
              assertTrue(inv.snapshot.map(_.id) == List(12L)) &&   // дурман остался
              assertTrue(log.contains("получено 🪙 800")) &&
              assertTrue(!after.choices.exists(_.id == "SellSchnapps"))
    }
  )
}
