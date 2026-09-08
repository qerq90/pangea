package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.ItemSet
import pangea.model.monster.{Elemental, Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** «Каменный страж» в бою: четыре оборонительных порога.
 *
 *  Каждый тест гоняет ОДИН и тот же ход дважды и сравнивает результат. Сравнение
 *  идёт с СОСЕДНИМ порогом (4 против 2, 6 против 4 и т.д.), а не с голым героем:
 *  иначе в разницу подмешался бы порог 2 (+5% к защите), который тоже режет урон.
 *  Так проверяется вклад ровно одного порога, а не абсолютные числа урона,
 *  которые ездят от любой правки баланса.
 */
object StoneGuardBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val bossLvl = 2L

  /** Герой, по которому моб гарантированно попадает (уклонения нет), а сам он
   *  переживает удар босса. `pieces` — сколько предметов «Каменного стража»
   *  надето (0 — набора нет). Броню даём прокачкой у Мастера Горна: предметы
   *  набора в этих тестах пустые, чтобы мерить бонусы набора, а не их железо. */
  private def hero(pieces: Int, hp: Long = 500000L, armor: Long = 0L, maxArmor: Long = 0L): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 15L, // BossLvL = 2
      fightStats = FightStats(atk = 1, hp = hp, armor = armor, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      masterHornBoosts = TestFixtures.hero(userId).masterHornBoosts.copy(armor = maxArmor),
      equipment  = TestFixtures.wearingSet(ItemSet.StoneGuard, pieces)
    )

  /** Бой с огненным элементалем; `turn` — какая способность применится следующей. */
  private def lairBattle(h: Hero, turn: Int, orbs: Int = 0, armorPct: Long): SoloPveBattle = {
    val stats   = Elemental.Fire.stats(bossLvl)
    val monster = Monster(0L, bossLvl, Race.Elemental, Rarity.Legendary, stats)
    SoloPveBattle.from(monster, h).copy(
      elementalKind        = Some(Elemental.Fire.entryName),
      elementalTurn        = turn,
      fireOrbs             = orbs,
      monsterCurrentEnergy = 300L,
      monsterCurrentArmor  = stats.armor * armorPct / 100L
    )
  }

  /** Бой с обычным (не стихийным) мобом: он бьёт сталью, а не огнём. */
  private def plainBattle(h: Hero): SoloPveBattle = {
    val stats   = FightStats(atk = 400, hp = 100000, armor = 0, defence = 0,
                             evasion = 0, accuracy = 1, energy = 0)
    SoloPveBattle.from(Monster(0L, 5L, Race.Human, Rarity.Common, stats), h)
  }

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  /** Броски одного хода: попадание героя, затем попадание моба (уклонение героя
   *  здесь 5%, так что 90 — гарантированный удар в ответ). Подавать ТОЛЬКО одним
   *  вызовом: повторный feedInts кладёт значения в НАЧАЛО очереди и ломает
   *  порядок. `extra` — броски после удара моба (поджог, скилл, травма). */
  private def seedTurn(extra: Int*) =
    // Два long-броска за ход — разброс урона героя и разброс урона моба. Второй
    // обязателен: с пустой очередью TestRandom катает его сам, и урон моба
    // начинает гулять от прогона к прогону.
    TestRandom.feedInts(60 +: 90 +: extra: _*) *> TestRandom.feedLongs(100L, 100L)

  /** Один ход «Атака» и итоговые статы героя. */
  private def strike(h: Hero, battle: SoloPveBattle, seed: ZIO[Any, Nothing, Unit]) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- seed
      _       <- state.action(testUser, tap("Attack"), r)
      updated <- dao.getHeroByUserId(userId).map(_.get)
      screens <- r.sentScreens
    } yield (updated, screens.map(_.text).mkString("\n"))

  override def spec = suite("Каменный страж в бою")(

    // ── Порог 4: стихийный урон ──────────────────────────────────────────────
    test("порог 4: удар элементаля по HP слабее на 20% — он бьёт стихией, а не сталью") {
      // Босс пропускает ход (turn = 3), брони на нём нет — остаётся чистый удар.
      def run(pieces: Int) =
        strike(hero(pieces), lairBattle(hero(pieces), turn = 3, armorPct = 0L), seedTurn(100))
          .map { case (h, _) => 500000L - h.fightStats.hp }
      for {
        plain    <- run(2)
        guarded  <- run(4)
      } yield assertTrue(plain > 0L) &&
              // поджог в этом ходу не проходит (бросок 100), урон чистый
              assertTrue(guarded == plain * 80L / 100L)
    },

    test("порог 4: смерч из сфер тоже режется — весь урон элементаля стихийный") {
      def run(pieces: Int) =
        strike(hero(pieces), lairBattle(hero(pieces), turn = 1, orbs = 2, armorPct = 0L), seedTurn(100, 100))
          .map { case (h, _) => 500000L - h.fightStats.hp }
      for {
        plain   <- run(2)
        guarded <- run(4)
      } yield assertTrue(plain > 0L) && assertTrue(guarded < plain)
    },

    test("порог 4 не трогает обычного моба: сталь стихией не считается") {
      def run(pieces: Int) =
        strike(hero(pieces), plainBattle(hero(pieces)), seedTurn(100))
          .map { case (h, _) => 500000L - h.fightStats.hp }
      for {
        plain   <- run(2)
        guarded <- run(4)
      } yield assertTrue(plain > 0L) && assertTrue(guarded == plain)
    },

    // ── Порог 6: расход брони ────────────────────────────────────────────────
    test("порог 6: броня тает на 20% медленнее, а HP при этом не страдает") {
      // Брони заведомо больше, чем урон за ход: весь удар уходит в неё.
      def run(pieces: Int) = {
        val h = hero(pieces, armor = 100000L, maxArmor = 100000L)
        strike(h, plainBattle(h), seedTurn(100))
          .map { case (u, _) => (100000L - u.fightStats.armor, u.fightStats.hp) }
      }
      for {
        plain   <- run(4)
        guarded <- run(6)
        (plainSpent, plainHp)     = plain
        (guardedSpent, guardedHp) = guarded
      } yield assertTrue(plainSpent > 0L) &&
              assertTrue(guardedSpent == plainSpent * 80L / 100L) &&
              // броня прикрыла столько же — в HP не протекло ни у того, ни у другого
              assertTrue(plainHp == 500000L && guardedHp == 500000L)
    },

    // ── Порог 10: сопротивление поджогу ──────────────────────────────────────
    test("порог 10: бросок, который поджёг бы обычного героя, о стража гаснет") {
      // Шанс поджога 50%; с набором остаётся 20%. Бросок 30 — между ними.
      def run(pieces: Int) =
        for {
          t <- makeState(hero(pieces), lairBattle(hero(pieces), turn = 3, armorPct = 0L))
          (state, dao, r) = t
          // 60 — удар героя, 90 — попадание моба, 30 — бросок поджога
          _      <- TestRandom.feedInts(60, 90, 30) *> TestRandom.feedLongs(100L, 100L)
          _      <- state.action(testUser, tap("Attack"), r)
          after  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        } yield after.effects.heroBurn.isDefined
      for {
        plain   <- run(8)
        guarded <- run(10)
      } yield assertTrue(plain) && assertTrue(!guarded)
    },

    // ── Порог 12: спасение бронёй ────────────────────────────────────────────
    test("порог 12: провал ниже 30% HP поднимает 30% брони") {
      // Броня выбита в ноль, HP на волосок выше порога: удар босса роняет под него.
      val full  = hero(12, maxArmor = 10000L)
      val limit = full.effectiveMaxHp(0L) * ItemSet.StoneGuard.LowHpThresholdPct / 100L
      def run(pieces: Int) = {
        val h = hero(pieces, hp = limit + 100L, maxArmor = 10000L)
        strike(h, lairBattle(h, turn = 3, armorPct = 0L), seedTurn(100))
          .map { case (u, log) => (u.fightStats.armor, log) }
      }
      for {
        plain    <- run(10)
        guarded  <- run(12)
        (plainArmor, _)      = plain
        (guardedArmor, gLog) = guarded
      } yield assertTrue(plainArmor == 0L) && // до порога 12 броня так и осталась выбитой
              assertTrue(guardedArmor == 10000L * ItemSet.StoneGuard.ArmorRestorePct / 100L) &&
              assertTrue(gLog.contains("Камень отзывается на вашу рану"))
    },

    test("порог 12 не срабатывает, пока герой и так сидит ниже порога") {
      val full  = hero(12, maxArmor = 10000L)
      val limit = full.effectiveMaxHp(0L) * ItemSet.StoneGuard.LowHpThresholdPct / 100L
      // Уже под порогом до удара — пересечения нет, брони не будет.
      val h = hero(12, hp = limit - 1000L, maxArmor = 10000L)
      for {
        r <- strike(h, lairBattle(h, turn = 3, armorPct = 0L), seedTurn(100))
        (u, log) = r
      } yield assertTrue(u.fightStats.armor == 0L) &&
              assertTrue(!log.contains("Камень отзывается на вашу рану"))
    }
  )
}
