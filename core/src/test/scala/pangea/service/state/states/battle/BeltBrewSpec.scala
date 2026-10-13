package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{BeltSlot, BrewKind, BrewRates, FlaskKind, Item, ItemDetails, ItemType, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Отвары в ячейках пояса: в бою их достают из того же отдельного сообщения,
  * что фляги и пыль. Боевые отвары делают то, что делали склянки; прочие —
  * то же, что сделал бы глоток с карточки, только посреди сечи. */
object BeltBrewSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def useSlot(idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"BeltUse","slot":"$idx"}"""))
  private def pick(idx: Int, what: String): UserAction =
    UserAction("", Some(s"""{"action":"BeltPick","slot":"$idx","pick":"$what"}"""))

  /** Пояс с ячейками: синий держит три. */
  private def belt(slots: BeltSlot*): Item =
    Item(9L, "Пояс", 1L, ItemRarity.Blue, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(slots.toList))

  private def weapon: Item =
    Item(5L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Герой с пробитым HP: лечить есть что, а в ответ ему не бьют — ход
    * расходником раунд не завершает. */
  private def hero(slots: BeltSlot*): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 50, hp = 10L, armor = 0, defence = 0,
                              evasion = 9999, accuracy = 9999, energy = 0),
      baseStats  = h.baseStats.copy(str = 1, vit = 100),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon, belt = belt(slots: _*)))
  }

  private def monster(hp: Long): Monster =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  private def battle(h: Hero, hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster), h, Nil)

  private def makeState(h: Hero, b: SoloPveBattle,
                        inv: TestInventoryRepository = TestInventoryRepository.accepting) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, inv, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def slots(h: Hero): List[BeltSlot] = h.equipment.belt.details match {
    case b: ItemDetails.Belt => b.slots
    case _                   => Nil
  }

  override def spec = suite("Отвары на поясе")(

    test("зверобойный отвар лечит из ячейки; ячейка пустеет, но помнит отвар") {
      val h = hero(BeltSlot.Brew(BrewKind.WortDecoction))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.fightStats.hp > 10L) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.WortDecoction, spent = true))) &&
              // брать из ячейки больше нечего — в списке пояса её нет
              assertTrue(BattleState.beltUsable(after).isEmpty) &&
              assertTrue(screens.contains(BrewKind.WortDecoction.label))
    },

    test("яростная настойка кладёт временный баф атаки на пять раундов") {
      val h = hero(BeltSlot.Brew(BrewKind.FuryTincture))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        after <- battleOf(dao)
        buffs  = after.heroBattleState.buffs
      } yield assertTrue(buffs.size == 1) &&
              assertTrue(buffs.head.atk > 0L) &&
              assertTrue(buffs.head.turnsLeft.contains(BrewRates.PotionBuffRounds))
    },

    test("жгучая вытяжка травит удары, дубильный настой чинит броню") {
      val hurt = hero(BeltSlot.Brew(BrewKind.StingingExtract), BeltSlot.Brew(BrewKind.TanningBrew))
      val dented = hurt.copy(fightStats = hurt.fightStats.copy(armor = 0L),
        equipment = hurt.equipment.copy(chestPlate =
          Item(6L, "Нагрудник", 1L, ItemRarity.Blue, ItemType.ChestPlate,
            attack = 0, accuracy = 0, energy = 0, armor = 200, defence = 0, evasion = 0)))
      for {
        t <- makeState(dented, battle(dented, 1000L))
        (state, dao, r) = t
        _      <- state.action(testUser, useSlot(0), r)
        poison <- battleOf(dao)
        // второй отвар — уже следующий раунд: бьём, чтобы раунд закрылся
        _      <- state.action(testUser, tap("Attack"), r)
        _      <- state.action(testUser, useSlot(1), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(poison.effects.heroPoisonousAttacks) &&
              assertTrue(after.fightStats.armor > 0L)
    },

    test("костоправный отвар спрашивает, какую травму снять, и снимает её") {
      val hurt = hero(BeltSlot.Brew(BrewKind.BoneSetter)).copy(
        traumaUntil = Some(999999999L),
        traumaNames = List(Trauma.BruisedLeg.name, Trauma.SplitSkull.name))
      for {
        t <- makeState(hurt, battle(hurt, 1000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        ask   <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, pick(0, Trauma.BruisedLeg.name), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield // вопрос — отдельным сообщением с кнопками внутри; тяжёлую не предлагают
              assertTrue(ask.inline && ask.choices.map(_.label) == List(Trauma.BruisedLeg.name)) &&
              // на вопрос ход не ушёл: склянка цела, пока травму не назвали
              assertTrue(after.traumaNames == List(Trauma.SplitSkull.name)) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.BoneSetter, spent = true))) &&
              assertTrue(log.contains(Trauma.BruisedLeg.name))
    },

    test("лечить нечего — склянка цела, и раунд тоже") {
      val h = hero(BeltSlot.Brew(BrewKind.BoneSetter))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _      <- state.action(testUser, useSlot(0), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        fight  <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.BoneSetter))) &&
              assertTrue(!fight.consumableUsedThisRound) &&
              assertTrue(screens.contains("Лечить нечего"))
    },

    test("устаревший ответ склянку не тратит: отвар спросит снова") {
      val hurt = hero(BeltSlot.Brew(BrewKind.BoneSetter)).copy(
        traumaUntil = Some(999999999L), traumaNames = List(Trauma.BruisedLeg.name))
      for {
        t <- makeState(hurt, battle(hurt, 1000L))
        (state, dao, r) = t
        // такой травмы на герое нет — кнопка из старого сообщения
        _       <- state.action(testUser, pick(0, Trauma.BrokenNose.name), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        fight   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.BoneSetter))) &&
              assertTrue(after.traumaNames == List(Trauma.BruisedLeg.name)) &&
              assertTrue(!fight.consumableUsedThisRound) &&
              assertTrue(screens.contains("откройте пояс заново"))
    },

    test("грибная смесь из ячейки достаётся всем, кто стоит в поле") {
      val h = hero(BeltSlot.Brew(BrewKind.MushroomMix))
      for {
        t <- makeState(h, battle(h, 100000L, 100000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        after <- battleOf(dao)
        blow   = BrewRates.MushroomDamagePerLvl * 10L
      } yield assertTrue(after.monsterCurrentHp == 100000L - blow) &&
              assertTrue(after.group.others.head.currentHp == 100000L - blow) &&
              assertTrue(after.effects.monsterPoison.isDefined)
    },

    test("живая вода заправляет флягу, что лежит в соседней ячейке") {
      val h = hero(BeltSlot.Brew(BrewKind.LivingWater),
                   BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, 1))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        full   = FlaskKind.chargesFor(ItemRarity.Gray)
      } yield assertTrue(slots(after) == List(
                BeltSlot.Brew(BrewKind.LivingWater, spent = true),
                BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, full)))
    },

    test("пустая ячейка номеров не сдвигает: кнопка фляги знает своё место") {
      val h = hero(BeltSlot.Dust(MaterialKind.RubyDust),
                   BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, 2))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, _, r) = t
        // горсть ушла на оружие — первая ячейка опустела
        _    <- state.action(testUser, useSlot(0), r)
        list <- r.sentScreens.map(_.last)
      } yield assertTrue(list.choices.size == 1) &&
              assertTrue(list.text.contains(FlaskKind.Healer.label)) &&
              // кнопка несёт НАСТОЯЩИЙ номер ячейки, а не номер строки в списке
              assertTrue(list.choices.head.data.get("slot").contains("1"))
    },

    test("после боя пустая ячейка сама берёт такой же отвар из сумки") {
      val h     = hero(BeltSlot.Brew(BrewKind.WortDecoction))
      val spare = BrewKind.item(BrewKind.WortDecoction).copy(id = 42L)
      val inv   = TestInventoryRepository.withItems(List(spare))
      for {
        // моб с 1 HP: добьём его атакой, бой кончится — пояс доберёт из сумки
        t <- makeState(h, battle(h, 1L), inv)
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.WortDecoction))) &&
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(screens.contains("На пояс легло из сумки"))
    },

    test("шнапс, самогон, сонный дурман и приговор в ячейку не ложатся") {
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.Schnapps)).isEmpty) &&
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.Moonshine)).isEmpty) &&
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.SleepingDope)).isEmpty) &&
      // приговор пьют до боя: с кем встретиться, решают не посреди сечи
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.Sentence)).isEmpty) &&
      // а всё прочее ложится
      assertTrue(BrewKind.values.filter(_.beltable)
        .forall(k => BeltSlot.of(BrewKind.item(k)).contains(BeltSlot.Brew(k))))
    }
  )
}
