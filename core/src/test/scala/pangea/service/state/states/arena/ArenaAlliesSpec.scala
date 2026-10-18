package pangea.service.state.states.arena

import pangea.engine.SceneContent
import pangea.model.arena.{ArenaAlly, ArenaFight, ArenaSide, ArenaStatus}
import pangea.model.battle.{BattleAlly, BattleEffects, Bleed, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.ItemSet
import pangea.model.squad.AllyKind
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.battle.BattleState
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Союзники на песке: крысы из-под ног (и в групповом бою — отряды). Свой
  * отряд бьётся в твой ход, чужой у тебя — «мобы», урон по ним ложится их
  * стороне, а сторона падает, только когда пали все. */
object ArenaAlliesSpec extends ZIOSpecDefault {

  private val oneId = UserId(1L)
  private val twoId = UserId(2L)
  private val one   = User(oneId, VkId("vk_one"), TelegramId("tg_one"))
  private val two   = User(twoId, VkId("vk_two"), TelegramId("tg_two"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))
  private def typed(text: String): UserAction = UserAction(text, None)

  private def content = ZIO.attempt(SceneContent.load())

  /** Боец: бьёт наверняка, чтобы ход не зависел от бросков. */
  private def fighter(id: UserId, heroId: Long, agi: Long, int: Long, hp: Long = 100000L): Hero = {
    val h = TestFixtures.hero(id)
    h.copy(
      id         = pangea.model.hero.HeroId(heroId),
      lvl        = 10L,
      baseStats  = h.baseStats.copy(agi = agi, int = int, vit = 500, str = 10),
      fightStats = FightStats(atk = 50L, hp = hp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 1000),
      equipment  = TestFixtures.emptyEquipment)
  }

  /** Крыса соперника: хлипкая, кусает слабо. */
  private def rat(pos: Int, hp: Long = 5L): BattleAlly =
    BattleAlly(AllyKind.Rat, pos, hp = hp, armor = 0L, energy = 0L,
      stats = FightStats(atk = 3, hp = hp.max(50L), armor = 0, defence = 0, evasion = 0, accuracy = 50, energy = 0), lvl = 10L)

  private def arena(heroes: (UserId, Hero)*) =
    for {
      dao   <- TestHeroDao.make
      _     <- ZIO.foreachDiscard(heroes) { case (id, h) => dao.insertHero(h.copy(userId = id)) }
      fights = TestArenaDao.empty
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (ArenaState(dao, fights, c, sched), BattleState(dao, TestInventoryRepository.accepting,
              TestItemRepository.make, c, sched, Some(fights)), dao, fights, sched, r)

  /** Двое на песке: первый записался по коду (сторона a), второй вписал код. */
  private def started(first: Hero, second: Hero) =
    for {
      t <- arena(oneId -> first, twoId -> second)
      (state, _, _, fights, _, r) = t
      _ <- state.action(one, tap("ArenaByCode"), r)
      _ <- state.action(two, typed(fights.snapshot.head.code), r)
      _ <- state.action(one, tap("ArenaPoke"), r)
    } yield t

  private def battleOf(dao: TestHeroDao, id: UserId): Task[SoloPveBattle] =
    dao.readActiveBattle(id).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def fight(fights: TestArenaDao): ArenaFight = fights.snapshot.head

  /** Переписать сторону второго в строке боя. */
  private def foeSide(fights: TestArenaDao)(f: ArenaSide => ArenaSide): Task[Unit] = {
    val now = fight(fights)
    fights.update(now.withSide(f(now.b.get)), 0L)
  }

  private val quick = fighter(oneId, 1L, agi = 20, int = 20)
  private val slow  = fighter(twoId, 2L, agi = 5, int = 5)

  override def spec = suite("Союзники на песке")(

    test("крыса на старте: выходит на песок рядом с героем, а сопернику видна «мобом» под своим номером") {
      val ratter = quick.copy(equipment = TestFixtures.wearingSet(ItemSet.Rat, 12))
      for {
        t <- started(ratter, slow)
        (_, battle, dao, fights, _, r) = t
        mine   <- battleOf(dao, oneId)
        _      <- battle.action(one, tap("Wait"), r)
        _      <- battle.action(two, tap("ArenaPoke"), r)
        theirs <- battleOf(dao, twoId)
        side    = fight(fights).a
      // за ход может выскочить и вторая (порог 6) — она получит следующий номер
      } yield assertTrue(side.allies.headOption.map(a => (a.id, a.ally.kind, a.ally.position)).contains((1, AllyKind.Rat, 2))) &&
              assertTrue(side.allies.map(_.id) == side.allies.indices.map(_ + 1).toList) &&
              assertTrue(mine.group.allies.map(a => (a.kind, a.home)) == List((AllyKind.Rat, 1))) &&
              assertTrue(theirs.group.entries.exists { case (place, s) => place == 2 && s.arenaUnit.contains(1) })
    },

    test("чужая крыса у меня — «моб»: бью её, рана ложится на сторону соперника, а сама она в мой ход не кусает") {
      for {
        t <- started(quick, slow)
        (_, battle, dao, fights, _, r) = t
        _      <- foeSide(fights)(_.copy(allies = List(ArenaAlly(1, rat(2, hp = 1000L)))))
        _      <- battle.action(one, tap("ArenaPoke"), r)
        _      <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        _      <- battle.action(one, aimed("Attack", 2), r)
        hurt    = fight(fights).b.flatMap(_.allies.headOption).map(_.ally.hp)
        me     <- dao.getHeroByUserId(oneId).map(_.get)
      } yield assertTrue(hurt.exists(_ < 1000L)) &&
              // соперник цел — били его крысу
              assertTrue(me.fightStats.hp == quick.fightStats.hp)
    },

    test("сторона падает только целиком: соперник пал, его крыса бьётся за него; добита крыса — победа") {
      val frail = slow.copy(fightStats = slow.fightStats.copy(hp = 10L))
      for {
        t <- started(quick, frail)
        (_, battle, dao, fights, _, r) = t
        _      <- foeSide(fights)(_.copy(allies = List(ArenaAlly(1, rat(2)))))
        _      <- battle.action(one, tap("ArenaPoke"), r)
        _      <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        _      <- battle.action(one, aimed("Attack", 1), r)       // соперник падает
        mid     = fight(fights)
        foeHp  <- dao.getHeroByUserId(twoId).map(_.get.fightStats.hp)
        _      <- r.reset
        _      <- battle.action(two, tap("ArenaPoke"), r)        // его ход — за него бьётся крыса
        downLog <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        back    = fight(fights)
        _      <- battle.action(one, tap("ArenaPoke"), r)
        _      <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        _      <- battle.action(one, aimed("Attack", 2), r)       // добита крыса
        end     = fight(fights)
        beaten <- dao.getHeroByUserId(twoId).map(_.get.fightStats.hp)
      } yield assertTrue(mid.status == ArenaStatus.Fighting && foeHp == 0L && mid.isTurn(twoId)) &&
              assertTrue(downLog.contains("за вас бьётся отряд") && back.isTurn(oneId)) &&
              assertTrue(end.status == ArenaStatus.Finished && end.winner.contains(oneId)) &&
              // проигравший уходит с песка на ногах
              assertTrue(beaten == 1L)
    },

    test("раны на сопернике тикают в конце хода того, кто их наложил") {
      for {
        t <- started(quick, slow)
        (_, battle, dao, fights, _, r) = t
        now     = fight(fights)
        _      <- fights.update(now.withSide(now.a.copy(ready = true,
                    effects = BattleEffects.empty.copy(monsterBleed = Some(Bleed(10))))), 0L)
        _      <- battle.action(one, tap("ArenaPoke"), r)
        _      <- battle.action(one, tap("Wait"), r)
        foe    <- dao.getHeroByUserId(twoId).map(_.get)
      } yield assertTrue(foe.fightStats.hp < slow.fightStats.hp)
    },

    test("крыса, вышедшая посреди боя, получает номер и остаётся на песке; павший союзник уходит") {
      val side = ArenaSide(oneId, quick.id, "Я", 10L, allies = List(ArenaAlly(1, rat(2))), nextAlly = 2)
      val b0   = SoloPveBattle.fromGroup(List(pangea.model.monster.Monster(0L, 10L,
                   pangea.model.monster.Race.Orc, pangea.model.monster.Rarity.Common, slow.fightStats)), quick, Nil)
      // в бою: старая крыса пала, новая выскочила из-под ног (дом 0)
      val b    = b0.copy(group = b0.group.copy(allies = List(rat(2, hp = 0L).copy(home = 1), rat(3).copy(home = 0))))
      val got  = ArenaBattle.harvest(side, b)
      assertTrue(got.allies.map(a => (a.id, a.ally.position)) == List((2, 3)) && got.nextAlly == 3)
    },

    test("щит и кинжал держит только сам соперник: его союзник, вставший в поля, ими не прикрыт") {
      val b0  = SoloPveBattle.fromGroup(List.fill(2)(pangea.model.monster.Monster(0L, 10L,
                  pangea.model.monster.Race.Orc, pangea.model.monster.Rarity.Common, slow.fightStats)), quick, Nil)
      val b   = b0.copy(monsterShieldUp = true, monsterCounterPower = 50L,
                  monsterArenaUnit = Some(ArenaBattle.HeroUnit),
                  group = b0.group.copy(others = b0.group.others.map(_.copy(arenaUnit = Some(1)))))
      val ally = b.engage(b.group.places.head)
      assertTrue(b.foeShieldUp && b.foeCounterPower == 50L) &&
      assertTrue(!ally.foeShieldUp && ally.foeCounterPower == 0L)
    }
  )
}
