package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item, ItemType, Rarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Каменный элементаль: круг из пяти способностей и особенности камня — голая
 *  сталь его почти не берёт, огонь плавит, а бьёт он по броне и HP раздельно. */
object StoneElementalBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val bossLvl = 2L // герой 15 уровня → BossLvL = (15−1)/5 = 2

  /** Оружие с камнем нужной стихии (или голое, если камня нет). */
  private def weapon(gem: Option[GemKind]): Item =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = List(gem.map(Gem(_, 1))))

  /** Герой, который бьёт без промаха и переживает удары босса. */
  private def hero(hp: Long = 500000L, armor: Long = 0L, gem: Option[GemKind] = None): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 15L,
      fightStats = FightStats(atk = 100, hp = hp, armor = armor, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon(gem))
    )

  /** Косорукий герой: моб уклоняется почти всегда, так что его HP и броня
   *  остаются нетронутыми — это нужно, чтобы проверить «чинить нечего». */
  private def clumsyHero: Hero = hero().copy(
    fightStats = FightStats(atk = 100, hp = 500000L, armor = 0, defence = 0,
                            evasion = 0, accuracy = 1, energy = 0))

  /** Бой с каменным; `turn` — какая способность применится следующей. */
  private def lairBattle(
      h: Hero,
      turn: Int,
      charges: Int = 0,
      energy: Long = 200L,   // максимум каменного: 100 × BossLvL
      hpPct: Long = 100L,
      armorPct: Long = 100L
  ): SoloPveBattle = {
    val stats   = MiniBoss.StoneElemental.stats(bossLvl)
    val monster = Monster(0L, bossLvl, Race.Elemental, MobRarity.Legendary, stats)
    SoloPveBattle.from(monster, h).copy(
      bossKind        = Some(MiniBoss.StoneElemental.entryName),
      bossTurn        = turn,
      bossCharges     = charges,
      monsterCurrentEnergy = energy,
      monsterCurrentHp     = stats.hp * hpPct / 100L,
      monsterCurrentArmor  = stats.armor * armorPct / 100L
    )
  }

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  private def battleAfter(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Броски одного хода: удар героя, попадание моба, затем `extra` (проки стихий,
    * бросок травмы). Подавать ТОЛЬКО одним вызовом: повторный feedInts кладёт
    * значения в НАЧАЛО очереди. Два long — разбросы урона героя и моба. */
  private def seedTurn(extra: Int*) =
    TestRandom.feedInts(60 +: 90 +: extra: _*) *> TestRandom.feedLongs(100L, 100L)

  private def strike(h: Hero, battle: SoloPveBattle, seed: ZIO[Any, Nothing, Unit]) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- seed
      _       <- state.action(testUser, tap("Attack"), r)
      updated <- dao.getHeroByUserId(userId).map(_.get)
      after   <- battleAfter(dao)
      screens <- r.sentScreens
    } yield (updated, after, screens.map(_.text).mkString)

  override def spec = suite("Каменный элементаль")(

    // ── Способности по кругу ──────────────────────────────────────────────────
    test("всплеск: половина атаки в урон и списанная энергия") {
      val h = hero()
      for {
        r <- strike(h, lairBattle(h, turn = 0, energy = 20L), seedTurn(100))
        (u, after, log) = r
      } yield assertTrue(log.contains("Резко вылетевший из элементаля камень")) &&
              assertTrue(u.fightStats.hp < 500000L) &&
              // 7 × BossLvL = 14 энергии за всплеск, остаток дособерёт реген раунда
              assertTrue(after.monsterCurrentEnergy == 20L - 14L + MiniBoss.StoneElemental.energyRegen(bossLvl))
    },

    test("валун: первые два копятся, третий сразу уходит в россыпь") {
      val h = hero()
      for {
        first  <- strike(h, lairBattle(h, turn = 1, charges = 0), seedTurn(100))
        second <- strike(h, lairBattle(h, turn = 1, charges = 1), seedTurn(100))
        third  <- strike(h, lairBattle(h, turn = 1, charges = 2), seedTurn(100))
      } yield assertTrue(first._2.bossCharges == 1) &&
              assertTrue(first._3.contains("груда камней соединилась в один Валун")) &&
              assertTrue(second._2.bossCharges == 2) &&
              assertTrue(third._2.bossCharges == 0) &&
              assertTrue(third._3.contains("россыпью мелких камней"))
    },

    test("россыпь сбивает с ног: защита и уклонение героя срезаны на 3 раунда") {
      val h = hero()
      for {
        r <- strike(h, lairBattle(h, turn = 1, charges = 2), seedTurn(100))
        (_, after, log) = r
      } yield assertTrue(after.effects.heroStunnedTurns == MiniBoss.StoneElemental.BurstDebuffTurns) &&
              assertTrue(after.effects.heroStunned) &&
              assertTrue(log.contains("сбивает Вас с ног"))
    },

    test("россыпь с шансом 20% даёт травму, как при смерти") {
      val h = hero()
      for {
        // 5 — бросок травмы прошёл (≤ 20). Каменный героя не поджигает, значит
        // бросок поджога в очереди не тратится и травме достаётся третье число.
        withT <- strike(h, lairBattle(h, turn = 1, charges = 2), seedTurn(5))
        // 100 — бросок травмы не прошёл
        noT   <- strike(h, lairBattle(h, turn = 1, charges = 2), seedTurn(100))
      } yield assertTrue(withT._3.contains("вы получили травму")) &&
              assertTrue(withT._1.traumaNames.nonEmpty) &&
              assertTrue(!noT._3.contains("вы получили травму")) &&
              assertTrue(noT._1.traumaNames.isEmpty)
    },

    test("восстановление: чинит 20% брони и 5% HP, но только если есть что чинить") {
      val h = hero()
      for {
        hurt  <- strike(h, lairBattle(h, turn = 2, hpPct = 50L, armorPct = 50L), seedTurn(100))
        // Тот же побитый камень, но ход пропускной: разница брони — ровно починка.
        idle  <- strike(h, lairBattle(h, turn = 4, hpPct = 50L, armorPct = 50L), seedTurn(100))
        // Целый камень: герой мажет, значит ни HP, ни броня не тронуты.
        whole <- strike(clumsyHero, lairBattle(clumsyHero, turn = 2), seedTurn(100))
        stats  = MiniBoss.StoneElemental.stats(bossLvl)
      } yield assertTrue(hurt._3.contains("возвращаются на своё законное место")) &&
              assertTrue(hurt._2.monsterCurrentArmor - idle._2.monsterCurrentArmor == stats.armor * 20L / 100L) &&
              assertTrue(hurt._2.monsterCurrentHp - idle._2.monsterCurrentHp == stats.hp * 5L / 100L) &&
              // целому чинить нечего — умение молчит, но очередь всё равно едет
              assertTrue(!whole._3.contains("возвращаются на своё законное место")) &&
              assertTrue(whole._2.bossTurn == 3)
    },

    test("вязкая земля: точность и уклонение героя срезаны на 3 раунда") {
      val h = hero()
      for {
        r <- strike(h, lairBattle(h, turn = 3), seedTurn(100))
        (_, after, log) = r
      } yield assertTrue(after.effects.heroGroundedTurns == MiniBoss.StoneElemental.GroundTurns) &&
              assertTrue(log.contains("Земля под Вашими ногами служит не Вам"))
    },

    test("способности идут строго по кругу из пяти и возвращаются к началу") {
      val h = hero()
      for {
        last <- strike(h, lairBattle(h, turn = 4), seedTurn(100))
      } yield assertTrue(MiniBoss.StoneElemental.abilities == 5) &&
              assertTrue(last._2.bossTurn == 0) // после пропуска круг начинается заново
    },

    test("без энергии способность не применяется, но очередь едет дальше") {
      val h = hero()
      for {
        r <- strike(h, lairBattle(h, turn = 1, energy = 0L), seedTurn(100))
        (_, after, log) = r
      } yield assertTrue(after.bossCharges == 0) &&
              assertTrue(!log.contains("груда камней")) &&
              assertTrue(after.bossTurn == 2)
    },

    // ── Особенности камня ─────────────────────────────────────────────────────
    test("голая сталь берёт камень лишь на 20%, а огонь бьёт в полтора раза сильнее") {
      // Броню снимаем: иначе весь удар тонет в её запасе и разницы не видно.
      def dealt(gem: Option[GemKind], procRoll: Int) = {
        val h = hero(gem = gem)
        strike(h, lairBattle(h, turn = 4, armorPct = 0L), seedTurn(procRoll)).map { case (_, after, _) =>
          MiniBoss.StoneElemental.stats(bossLvl).hp - after.monsterCurrentHp
        }
      }
      for {
        plain <- dealt(None, 100)                 // оружие без камней: прок не катается
        fire  <- dealt(Some(GemKind.Ruby), 100)   // рубин — стихия огня, прок не прошёл
      } yield assertTrue(plain > 0L) &&
              // 0.4 против 1.5: огненное оружие бьёт камень в разы больнее голой стали
              assertTrue(fire > plain * 3L) &&
              assertTrue(MiniBoss.StoneElemental.plainDamageTakenMult == 0.4) &&
              assertTrue(MiniBoss.StoneElemental.damageTakenMult(pangea.model.battle.Element.Fire) == 1.5)
    },

    // Регрессия: в лог шёл СЫРОЙ урон, до сопротивления камня. Игрок читал
    // «357 урона», а брони снималось 70 — и это выглядело как обман.
    test("в логе стоит урон, который реально прошёл, а не заявленный") {
      val h = hero()
      for {
        r <- strike(h, lairBattle(h, turn = 4), seedTurn(100))
        (_, after, log) = r
        dealtArmor = MiniBoss.StoneElemental.stats(bossLvl).armor - after.monsterCurrentArmor
        dealtHp    = MiniBoss.StoneElemental.stats(bossLvl).hp - after.monsterCurrentHp
        shown      = """Вы наносите (\d+) урона""".r.findFirstMatchIn(log).map(_.group(1).toLong)
      } yield assertTrue(shown.contains(dealtArmor + dealtHp)) &&
              // голую сталь камень берёт на 20% — заявленный удар был впятеро больше
              assertTrue(shown.exists(_ < h.fightStats.atk))
    },

    test("его удар бьёт раздельно: 90% в броню и 30% в HP — броня не спасает") {
      val h = hero(armor = 500000L)
      for {
        r <- strike(h, lairBattle(h, turn = 4), seedTurn(100))
        (u, _, _) = r
        lostHp    = 500000L - u.fightStats.hp
        lostArmor = 500000L - u.fightStats.armor
      } yield assertTrue(lostHp > 0L) && // HP уходит, несмотря на полную броню
              assertTrue(lostArmor > lostHp) &&
              // 90 и 30 от одного и того же урона: броня тает ровно втрое быстрее
              assertTrue(lostArmor == lostHp * 3L)
    },

    test("без брони его удар целиком уходит в HP") {
      val bare  = hero(armor = 0L)
      val armed = hero(armor = 500000L)
      for {
        naked  <- strike(bare, lairBattle(bare, turn = 4), seedTurn(100))
        heavy  <- strike(armed, lairBattle(armed, turn = 4), seedTurn(100))
        bareHp  = 500000L - naked._1.fightStats.hp
        heavyHp = 500000L - heavy._1.fightStats.hp
      } yield assertTrue(bareHp > 0L) &&
              // голому достаётся весь удар, а не 30% от него
              assertTrue(bareHp > heavyHp * 3L) &&
              assertTrue(naked._1.fightStats.armor == 0L)
    },

    test("на остатках брони доля HP растёт: что броня не покрыла, добирает здоровье") {
      // Брони меньше, чем её 90%-ная доля удара, — часть перетекает в HP.
      val thin = hero(armor = 100L)
      for {
        r <- strike(thin, lairBattle(thin, turn = 4), seedTurn(100))
        (u, _, _) = r
        lostHp    = 500000L - u.fightStats.hp
      } yield assertTrue(u.fightStats.armor == 0L) && // тонкая броня снялась вся
              // в HP ушло больше 30% удара: непокрытое бронёй добралось до здоровья
              assertTrue(lostHp > 0L) &&
              assertTrue(lostHp + 100L >= (MiniBoss.StoneElemental.stats(bossLvl).atk * 30L / 100L))
    },

    test("пока оружие без стихий, в конце раунда висит подсказка про поглощённый удар") {
      def hintWith(gem: Option[GemKind]) = {
        val h = hero(gem = gem)
        strike(h, lairBattle(h, turn = 4, armorPct = 0L), seedTurn(100)).map(_._3)
      }
      for {
        bare  <- hintWith(None)
        fiery <- hintWith(Some(GemKind.Ruby))
      } yield assertTrue(bare.contains("Камень поглощает удар")) &&
              // подсказка идёт последней строкой лога, уже после хода моба
              assertTrue(bare.indexOf("Камень поглощает удар") > bare.indexOf("Вы наносите")) &&
              // со стихией в оружии подсказка не нужна — сопротивление не работает
              assertTrue(!fiery.contains("Камень поглощает удар"))
    },

    test("подожжённый камень бьёт слабее, теряет валун и часть потолка брони") {
      val h = hero(gem = Some(GemKind.Ruby))
      for {
        // Порядок бросков: удар героя (60), прок огня (1 — прошёл, ≤30),
        // затем попадание моба (90).
        r <- strike(h, lairBattle(h, turn = 4, charges = 2),
               TestRandom.feedInts(60, 1, 90) *> TestRandom.feedLongs(100L, 100L))
        (_, after, log) = r
      } yield assertTrue(after.effects.monsterBurn.isDefined) &&
              // Поджог случается в фазу игрока, а конец раунда сразу тикает счётчик.
              assertTrue(after.effects.monsterWeakenedTurns == MiniBoss.StoneElemental.BurnedTurns - 1) &&
              assertTrue(after.effects.monsterMaxArmorCut == MiniBoss.StoneElemental.BurnedMaxArmorCut) &&
              assertTrue(after.bossCharges == 1) && // один валун расплавился
              assertTrue(log.contains("расплавило один из Каменных Валунов")) &&
              assertTrue(log.contains("Камень плывёт от жара"))
    },

    test("выше просевшего потолка броня уже не чинится") {
      val h = hero()
      val stats = MiniBoss.StoneElemental.stats(bossLvl)
      // Потолок срезан поджогами на 1000, брони выбито ровно столько же.
      val battle = lairBattle(h, turn = 2, armorPct = 50L)
        .pipe(b => b.copy(effects = b.effects.copy(monsterMaxArmorCut = 1000L)))
      for {
        r <- strike(h, battle, seedTurn(100))
        (_, after, _) = r
      } yield assertTrue(after.monsterCurrentArmor <= stats.armor - 1000L)
    },

    test("каменный горит: он не огонь, и поджог на нём держится") {
      assertTrue(!MiniBoss.StoneElemental.immuneToBurn) &&
      assertTrue(MiniBoss.FireElemental.immuneToBurn)
    },

    // Регрессия: условие поджога стояло на «есть минибосс», а не на «это огненный»,
    // из-за чего обычные удары камня (и Джо) поджигали героя чужим огнём.
    test("обычная атака камня НЕ поджигает героя — огонь только у огненного") {
      val h = hero()
      def burnedAfter(seed: Int) =
        strike(h, lairBattle(h, turn = 4, armorPct = 0L), seedTurn(seed))
          .map { case (_, after, log) => (after.effects.heroBurn, log) }
      for {
        low  <- burnedAfter(1)   // бросок, который у огненного поджёг бы наверняка
        high <- burnedAfter(100)
      } yield assertTrue(MiniBoss.StoneElemental.heroIgniteChancePct == 0L) &&
              assertTrue(MiniBoss.FireElemental.heroIgniteChancePct == 50L) &&
              assertTrue(low._1.isEmpty) && assertTrue(high._1.isEmpty) &&
              assertTrue(!low._2.contains("Удар элементаля поджёг вас"))
    },

    test("шипов у камня нет: об него не обжигаются") {
      val h = hero()
      for {
        r <- strike(h, lairBattle(h, turn = 4), seedTurn(100))
        (_, after, log) = r
      } yield assertTrue(!log.contains("Раскалённая броня")) &&
              assertTrue(after.effects.heroBurn.isEmpty)
    },

    test("дроп: его ингредиент — магический камень, а вещи из «Каменного стража»") {
      assertTrue(MiniBoss.StoneElemental.ingredient == pangea.model.item.MaterialKind.MagicStone) &&
      assertTrue(MiniBoss.StoneElemental.set == pangea.model.item.ItemSet.StoneGuard)
    }
  )

  // Локальный `pipe` для читаемой донастройки боя (Scala 2.13 без импорта цепочек).
  private implicit class PipeOps[A](private val a: A) extends AnyVal {
    def pipe[B](f: A => B): B = f(a)
  }
}
