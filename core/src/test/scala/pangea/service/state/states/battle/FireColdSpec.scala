package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, Burn, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Grip, Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.{TestRandom, _}

/** Огонь и холод гасят друг друга — на мобе и на герое. Холод по горящему
  * гасит пламя и сам не ложится; огонь по замёрзшему растапливает лёд и сам не
  * занимается; оба в одном ударе — только пар. */
object FireColdSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Одноручный меч с камнем в гнезде. */
  private def sword(id: Long, gem: GemKind): Item =
    Item(id, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      grip = Some(Grip.OneHanded), sockets = List(Some(Gem(gem, 1))))

  private def hero(right: Item = Item.NoItem, left: Item = Item.NoItem): Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 50L, hp = 500000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = base.baseStats.copy(vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = right, leftHand = left))
  }

  private def mob(race: Race): Monster =
    Monster(0L, 10L, race, Rarity.Common,
      FightStats(atk = 50, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  /** Один ход: `key` — что жмёт герой; броски подаются заранее. Возвращает бой
    * после хода и весь лог. */
  private def turn(h: Hero, b: SoloPveBattle, key: String, ints: List[Int], longs: List[Long]) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeActiveBattle(userId, b.asJson)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
      state = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
      _   <- TestRandom.feedInts(ints: _*) *> TestRandom.feedLongs(longs: _*)
      _   <- state.action(testUser, tap(key), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after, log)

  private def against(race: Race, h: Hero, effects: BattleEffects): SoloPveBattle =
    SoloPveBattle.from(mob(race), h, squad = false).copy(effects = effects)

  override def spec = suite("Огонь и холод")(

    test("холод по горящему мобу: пламя гаснет, мороз не ложится") {
      val h = hero(sword(101L, GemKind.Sapphire))
      for {
        t <- turn(h, against(Race.Orc, h, BattleEffects.empty.copy(monsterBurn = Some(Burn(6)))), "Attack",
               ints = List(60, 1), longs = List(100L))
        (after, log) = t
      } yield assertTrue(after.effects.monsterBurn.isEmpty && after.effects.monsterColdDefenceCut == 0) &&
              assertTrue(log.contains("Холод гасит пламя"))
    },

    test("огонь по замёрзшему мобу: лёд тает, пламя не занимается") {
      val h = hero(sword(101L, GemKind.Ruby))
      for {
        t <- turn(h, against(Race.Orc, h, BattleEffects.empty.copy(monsterColdDefenceCut = 10)), "Attack",
               ints = List(60, 1), longs = List(100L))
        (after, log) = t
      } yield assertTrue(after.effects.monsterColdDefenceCut == 0 && after.effects.monsterBurn.isEmpty) &&
              assertTrue(log.contains("Пламя растапливает лёд"))
    },

    test("рубин в одном мече, сапфир в другом — так можно; сработали оба в одном ударе — только пар") {
      val h = hero(sword(101L, GemKind.Ruby), sword(102L, GemKind.Sapphire))
      for {
        clean   <- turn(h, against(Race.Orc, h, BattleEffects.empty), "Attack", ints = List(60, 1, 1), longs = List(100L))
        burning <- turn(h, against(Race.Orc, h, BattleEffects.empty.copy(monsterBurn = Some(Burn(6)))), "Attack",
                     ints = List(60, 1, 1), longs = List(100L))
      } yield assertTrue(h.gems.weaponElements.size == 2) &&
              assertTrue(clean._1.effects.monsterBurn.isEmpty && clean._1.effects.monsterColdDefenceCut == 0) &&
              assertTrue(clean._2.contains("только пар")) &&
              // что на цели уже было, то и осталось
              assertTrue(burning._1.effects.monsterBurn.isDefined && burning._1.effects.monsterColdDefenceCut == 0)
    },

    test("морозный удар гнома по горящему герою: пламя на нём гаснет, мороз не ложится") {
      val h = hero()
      for {
        t <- turn(h, against(Race.Gnome, h, BattleEffects.empty.copy(heroBurn = Some(Burn(6)))), "Wait",
               ints = List(60, 1), longs = List(100L))
        (after, log) = t
      } yield assertTrue(after.effects.heroBurn.isEmpty && after.effects.heroColdDefenceCut == 0) &&
              assertTrue(log.contains("Холод гасит пламя на вас"))
    },

    test("поджог демона по замёрзшему герою: лёд тает, пламя не занимается") {
      val h = hero()
      for {
        t <- turn(h, against(Race.Demon, h, BattleEffects.empty.copy(heroColdDefenceCut = 10)), "Wait",
               ints = List(60, 1), longs = List(100L))
        (after, log) = t
      } yield assertTrue(after.effects.heroColdDefenceCut == 0 && after.effects.heroBurn.isEmpty) &&
              assertTrue(log.contains("Пламя растапливает лёд на вас"))
    }
  )
}
