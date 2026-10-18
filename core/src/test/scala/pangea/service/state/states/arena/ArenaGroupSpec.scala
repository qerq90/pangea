package pangea.service.state.states.arena

import pangea.engine.SceneContent
import pangea.model.arena.{ArenaFight, ArenaMode}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.squad.{AllyKind, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.battle.BattleState
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Режим группового боя: режим выбирается первым, очередь у каждого своя, и в
  * групповом бою каждый выходит со своим отрядом — а отряд в «Отряде» после
  * боя прежний. */
object ArenaGroupSpec extends ZIOSpecDefault {

  private val oneId = UserId(1L)
  private val twoId = UserId(2L)
  private val one   = User(oneId, VkId("vk_one"), TelegramId("tg_one"))
  private val two   = User(twoId, VkId("vk_two"), TelegramId("tg_two"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def inMode(key: String, mode: ArenaMode): UserAction =
    UserAction("", Some(s"""{"action":"$key","mode":"${mode.entryName}"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))
  private def typed(text: String): UserAction = UserAction(text, None)

  private def content = ZIO.attempt(SceneContent.load())

  private def fighter(id: UserId, heroId: Long, agi: Long, int: Long, squad: Squad = Squad.empty): Hero = {
    val h = TestFixtures.hero(id)
    h.copy(
      id         = pangea.model.hero.HeroId(heroId),
      lvl        = 10L,
      baseStats  = h.baseStats.copy(agi = agi, int = int, vit = 500, str = 10),
      fightStats = FightStats(atk = 50L, hp = 100000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 1000),
      equipment  = TestFixtures.emptyEquipment,
      squad      = squad)
  }

  private val withOne = Squad.empty.hire(AllyKind.Human, 10L, 0L)
  private val withTwo = Squad.empty.hire(AllyKind.Human, 10L, 0L).hire(AllyKind.Gnome, 10L, 0L)

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

  private def battleOf(dao: TestHeroDao, id: UserId): Task[SoloPveBattle] =
    dao.readActiveBattle(id).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def fight(fights: TestArenaDao): ArenaFight = fights.snapshot.head

  override def spec = suite("Групповой бой на арене")(

    test("сначала выбирается режим, и дальше он едет в кнопках") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10))
        (state, _, _, _, _, r) = t
        _    <- state.enter(one, r)
        menu <- r.sentScreens.map(_.last)
        _    <- state.action(one, tap("ArenaGroup"), r)
        mode <- r.sentScreens.map(_.last)
      } yield assertTrue(menu.choices.map(_.id).take(2) == List("ArenaSolo", "ArenaGroup")) &&
              assertTrue(mode.text.contains("Групповой бой")) &&
              assertTrue(mode.choices.filter(c => c.id == "ArenaNearest" || c.id == "ArenaByCode")
                           .forall(_.data.get("mode").contains("Group")))
    },

    test("очередь у каждого режима своя; в групповой видно, сколько бойцов выйдет с ждущим") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10), twoId -> fighter(twoId, 2L, 5, 5, withTwo))
        (state, _, _, fights, _, r) = t
        _     <- state.action(two, inMode("ArenaNearest", ArenaMode.Group), r)
        _     <- state.action(one, inMode("ArenaNearest", ArenaMode.Solo), r)
        solo  <- r.sentScreens.map(_.last)
        _     <- state.action(one, inMode("ArenaNearest", ArenaMode.Group), r)
        group <- r.sentScreens.map(_.last)
        mine   = fights.snapshot.find(_.has(oneId))
      } yield assertTrue(!solo.choices.exists(_.id.startsWith(ArenaState.JoinPrefix))) &&
              assertTrue(group.choices.exists(c => c.id.startsWith(ArenaState.JoinPrefix) && c.label.contains("отряд: 2"))) &&
              // ждал один на один — нажал групповой: запись перешла туда же
              assertTrue(mine.exists(_.mode == ArenaMode.Group))
    },

    test("по записи — тот же код в другом режиме: запись переходит, код прежний") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10))
        (state, _, _, fights, _, r) = t
        _     <- state.action(one, inMode("ArenaByCode", ArenaMode.Solo), r)
        first  = fight(fights)
        _     <- state.action(one, inMode("ArenaByCode", ArenaMode.Group), r)
        after  = fight(fights)
        said  <- r.sentScreens.map(_.last.text)
      } yield assertTrue(first.mode == ArenaMode.Solo && after.mode == ArenaMode.Group && after.code == first.code) &&
              assertTrue(said.contains("групповой бой"))
    },

    test("групповой бой: оба выходят со своими отрядами, по их местам; а отряд в «Отряде» после ран прежний") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20, squad = withOne)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5, squad = withTwo)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, dao, fights, _, r) = t
        _       <- state.action(one, inMode("ArenaByCode", ArenaMode.Group), r)
        _       <- state.action(two, typed(fight(fights).code), r)
        started  = fight(fights)
        _       <- state.action(one, tap("ArenaPoke"), r)
        mine    <- battleOf(dao, oneId)
        // первый бьёт союзника второго, стоящего на втором месте
        _       <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        _       <- battle.action(one, aimed("Attack", 2), r)
        hurt     = fight(fights).b.flatMap(_.allies.find(_.ally.position == 2)).map(_.ally.hp)
        squadNow <- dao.getHeroByUserId(twoId).map(_.get.squad)
      } yield assertTrue(started.mode == ArenaMode.Group) &&
              assertTrue(started.a.allies.map(_.ally.kind) == List(AllyKind.Human)) &&
              assertTrue(started.b.exists(_.allies.map(_.ally.kind).toSet == Set[AllyKind](AllyKind.Human, AllyKind.Gnome))) &&
              // у первого в бою: свой Йорген — союзник, отряд второго — «мобы» под номерами
              assertTrue(mine.group.allies.map(_.kind) == List(AllyKind.Human)) &&
              assertTrue(mine.group.entries.map(_._2.arenaUnit).toSet == Set[Option[Int]](Some(1), Some(2))) &&
              // рана легла на сторону второго, а в «Отряде» у него всё как было
              assertTrue(hurt.exists(hp => started.b.flatMap(_.allies.find(_.ally.position == 2)).exists(_.ally.hp > hp))) &&
              assertTrue(squadNow == slow.squad)
    },

    test("один на один отряд остаётся за оградой") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20, squad = withOne)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5, squad = withTwo)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, _, _, fights, _, r) = t
        _ <- state.action(one, inMode("ArenaByCode", ArenaMode.Solo), r)
        _ <- state.action(two, typed(fight(fights).code), r)
      } yield assertTrue(fight(fights).a.allies.isEmpty && fight(fights).b.exists(_.allies.isEmpty))
    }
  )
}
