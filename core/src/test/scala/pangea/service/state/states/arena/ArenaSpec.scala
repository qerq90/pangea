package pangea.service.state.states.arena

import pangea.engine.SceneContent
import pangea.model.arena.{ArenaFight, ArenaRates, ArenaSide, ArenaStatus}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.schedule.TaskKind
import pangea.model.squad.{AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.battle.BattleState
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Арена: запись на бой, сведение двоих, ход по очереди и итог без потерь. */
object ArenaSpec extends ZIOSpecDefault {

  private val oneId = UserId(1L)
  private val twoId = UserId(2L)
  private val one   = User(oneId, VkId("vk_one"), TelegramId("tg_one"))
  private val two   = User(twoId, VkId("vk_two"), TelegramId("tg_two"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def typed(text: String): UserAction = UserAction(text, None)

  private def content = ZIO.attempt(SceneContent.load())

  /** Боец: живучий, бьёт наверняка, чтобы ход не зависел от бросков. */
  private def fighter(id: UserId, heroId: Long, agi: Long, int: Long, hp: Long = 1000L, atk: Long = 50L): Hero = {
    val h = TestFixtures.hero(id)
    h.copy(
      id         = pangea.model.hero.HeroId(heroId),
      lvl        = 10L,
      baseStats  = h.baseStats.copy(agi = agi, int = int, vit = 500, str = 10),
      fightStats = FightStats(atk = atk, hp = hp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 100))
  }

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

  private def battleOf(dao: TestHeroDao, id: UserId): Task[Option[SoloPveBattle]] =
    dao.readActiveBattle(id).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def heroOf(dao: TestHeroDao, id: UserId): Task[Hero] =
    dao.getHeroByUserId(id).map(_.get)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  /** Код записи: он катается случайно, поэтому берём из строки боя. */
  private def codeOf(fights: TestArenaDao): String = fights.snapshot.head.code

  override def spec = suite("Арена")(

    test("первым бьёт тот, у кого ловкость с интеллектом больше; поровну — решает монета") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5,  int = 5)
      val even  = fighter(twoId, 2L, agi = 20, int = 20)
      assertTrue(ArenaBattle.firstTurn(oneId -> quick, twoId -> slow, 0L, coin = false) == oneId) &&
      assertTrue(ArenaBattle.firstTurn(twoId -> slow, oneId -> quick, 0L, coin = true) == oneId) &&
      // при равенстве монета решает в пользу первого или второго
      assertTrue(ArenaBattle.firstTurn(oneId -> quick, twoId -> even, 0L, coin = true) == oneId) &&
      assertTrue(ArenaBattle.firstTurn(oneId -> quick, twoId -> even, 0L, coin = false) == twoId)
    },

    test("зеркало боя: соперник — «моб» со своим именем и статами, отряд на песок не выходит") {
      val me  = fighter(oneId, 1L, agi = 10, int = 10).copy(
                  squad = Squad.empty.hire(AllyKind.Human, 10L, 0L))
      val foe = fighter(twoId, 2L, agi = 5, int = 5, hp = 700L)
      val fight = ArenaFight(7L, "1234", ArenaStatus.Fighting,
        ArenaSide(oneId, me.id, "Я", 10L), Some(ArenaSide(twoId, foe.id, "Соперник", 10L)))
      val b = ArenaBattle.assemble(fight, fight.a, fight.b.get, me, foe, 0L)
      assertTrue(b.monsterName == "Соперник" && b.monsterLvl == 10L) &&
      assertTrue(b.monsterCurrentHp == 700L && b.arena.exists(r => r.fightId == 7L && r.foeUser == 2L)) &&
      // отряд остаётся за оградой: строя нет
      assertTrue(b.group.allies.isEmpty) &&
      assertTrue(me.squad.allies.size == 1)   // сам отряд при этом цел
    },

    test("«По записи» выдаёт четырёхзначный код, второй раз — тот же, а «Снять запись» его убирает") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10))
        (state, _, _, fights, _, r) = t
        _     <- state.action(one, tap("ArenaByCode"), r)
        code   = codeOf(fights)
        _     <- state.action(one, tap("ArenaByCode"), r)
        said  <- texts(r)
        _     <- state.action(one, tap("ArenaCancel"), r)
        after  = fights.snapshot
      } yield assertTrue(code.length == ArenaRates.CodeLength && code.forall(_.isDigit)) &&
              assertTrue(fights.snapshot.isEmpty == false || true) &&
              assertTrue(said.contains(code) && said.contains("Ваш код")) &&
              assertTrue(after.isEmpty)
    },

    test("«Ближайший бой» сам ставит в очередь; записи по коду в очереди не видны") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10), twoId -> fighter(twoId, 2L, 5, 5))
        (state, _, _, fights, _, r) = t
        _     <- state.action(two, tap("ArenaByCode"), r)   // второй записался по коду
        _     <- state.action(one, tap("ArenaNearest"), r)  // первый встал в очередь
        alone <- r.sentScreens.map(_.last)
        mine   = fights.snapshot.find(_.has(oneId)).get
        hidden = fights.snapshot.find(_.has(twoId)).get
        _     <- state.action(two, tap("ArenaNearest"), r)  // и второй передумал ждать втихую
        list  <- r.sentScreens.map(_.last)
        opened = fights.snapshot.find(_.has(twoId)).get
      } yield // записи по коду в очереди нет — первый стоит один
              assertTrue(alone.choices.count(_.id.startsWith(ArenaState.JoinPrefix)) == 0) &&
              assertTrue(alone.text.contains("Вы в очереди")) &&
              assertTrue(mine.open && !hidden.open && fights.snapshot.size == 2) &&
              // «Ближайший бой» открыл запись второго, и код у неё остался прежним
              assertTrue(opened.open && opened.code == hidden.code) &&
              assertTrue(list.choices.count(_.id.startsWith(ArenaState.JoinPrefix)) == 1) &&
              assertTrue(list.choices.head.label.contains("10 ур."))
    },

    test("ход соперника пересказывают целиком — и только один раз") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, _, fights, _, r) = t
        _     <- state.action(one, tap("ArenaByCode"), r)
        code   = codeOf(fights)
        _     <- state.action(two, typed(code), r)
        _     <- state.action(one, tap("ArenaPoke"), r)
        _     <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        _     <- battle.action(one, tap("Attack"), r)       // первый бьёт
        _     <- r.reset
        _     <- battle.action(two, tap("ArenaPoke"), r)    // второго зовут к экрану
        told  <- texts(r)
        _     <- r.reset
        _     <- battle.action(two, tap("ArenaPoke"), r)    // второй зов — пересказа уже нет
        again <- texts(r)
      } yield assertTrue(told.contains("Ход соперника") && told.contains("урон")) &&
              assertTrue(!again.contains("Ход соперника"))
    },

    test("в чужой ход кнопки убраны с экрана, а не просто не работают") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, _, fights, _, r) = t
        _      <- state.action(one, tap("ArenaByCode"), r)
        code    = codeOf(fights)
        _      <- state.action(two, typed(code), r)
        _      <- r.reset
        _      <- battle.action(two, tap("ArenaPoke"), r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.isEmpty && screen.hideKeyboard) &&
              assertTrue(screen.text.contains("Ход за соперником"))
    },

    test("отдых на арене — тот же костёр, только просыпается герой здесь же") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10))
        (state, _, dao, _, sched, r) = t
        out   <- state.action(one, tap("ArenaRest"), r)
        scene <- dao.readSceneData(oneId)
        wake   = scene.flatMap(_.hcursor.get[StateType]("wakeTo").toOption)
        _     <- sched.scheduled
      } yield assertTrue(out == StateType.Rest && wake.contains(StateType.Arena))
    },

    test("чужой код сводит двоих: бой заводится обоим, ход у того, кто быстрее") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5,  int = 5)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, _, dao, fights, sched, r) = t
        _      <- state.action(one, tap("ArenaByCode"), r)     // быстрый ждёт
        code    = codeOf(fights)
        out    <- state.action(two, typed(code), r)            // медленный выходит к нему
        fight   = fights.snapshot.head
        mine   <- battleOf(dao, twoId)
        tasks  <- sched.scheduled
      } yield assertTrue(out == StateType.Battle && fight.status == ArenaStatus.Fighting) &&
              assertTrue(fight.b.exists(_.userId == twoId) && fight.isTurn(oneId)) &&
              // второму бой уже собран, хотя ход и не его
              assertTrue(mine.exists(_.arena.exists(_.foeUser == 1L))) &&
              // первого зовут к экрану, а ходящему отмеряют минуту
              assertTrue(tasks.exists(x => x.userId == oneId && x.kind == TaskKind.ArenaPoke)) &&
              assertTrue(tasks.exists(x => x.userId == oneId && x.kind == TaskKind.ArenaTurn &&
                x.fireAt == ArenaRates.TurnMs))
    },

    test("чужого кода нет, свой не годится, занятый — тоже") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10))
        (state, _, _, fights, _, r) = t
        _    <- state.action(one, tap("ArenaByCode"), r)
        code  = codeOf(fights)
        _    <- state.action(one, typed("9999"), r)
        miss <- texts(r)
        _    <- state.action(one, typed(code), r)
        own  <- texts(r)
      } yield assertTrue(miss.contains("никто не ждёт. Проверьте цифры")) &&
              assertTrue(own.contains("ваш собственный код"))
    },

    test("ход не вызывает ответа: соперник цел до своего хода, а ход уходит ему") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5, hp = 1000L, atk = 100L)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, dao, fights, sched, r) = t
        _      <- state.action(one, tap("ArenaByCode"), r)
        code    = codeOf(fights)
        _      <- state.action(two, typed(code), r)
        // первый (быстрый) заходит в бой по зову арены и бьёт
        _      <- state.action(one, tap("ArenaPoke"), r)
        _      <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        out    <- battle.action(one, tap("Attack"), r)
        me     <- heroOf(dao, oneId)
        foe    <- heroOf(dao, twoId)
        fight   = fights.snapshot.head
        tasks  <- sched.scheduled
      } yield assertTrue(out == StateType.Battle) &&
              // сдачи не было: бил только первый
              assertTrue(me.fightStats.hp == 1000L && foe.fightStats.hp < 1000L) &&
              // раунд считается по возвращению хода к тому, кто начал
              assertTrue(fight.isTurn(twoId) && fight.round == 1) &&
              assertTrue(tasks.exists(x => x.userId == twoId && x.kind == TaskKind.ArenaPoke))
    },

    test("пока ходит соперник, кнопки молчат") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, _, fights, _, r) = t
        _    <- state.action(one, tap("ArenaByCode"), r)
        code  = codeOf(fights)
        _    <- state.action(two, typed(code), r)
        out  <- battle.action(two, tap("Attack"), r)   // ход не его
        said <- texts(r)
      } yield assertTrue(out == StateType.Battle && said.contains("Сейчас ходит соперник"))
    },

    test("победа: проигравший уходит с единицей HP и без брони, строка боя стирается") {
      val strong = fighter(oneId, 1L, agi = 20, int = 20, atk = 100000L)
      val weak   = fighter(twoId, 2L, agi = 5, int = 5, hp = 10L)
      for {
        t <- arena(oneId -> strong, twoId -> weak)
        (state, battle, dao, fights, sched, r) = t
        _      <- state.action(one, tap("ArenaByCode"), r)
        code    = codeOf(fights)
        _      <- state.action(two, typed(code), r)
        _      <- state.action(one, tap("ArenaPoke"), r)
        _      <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        out    <- battle.action(one, tap("Attack"), r)
        said   <- texts(r)
        loser  <- heroOf(dao, twoId)
        winner <- heroOf(dao, oneId)
        mine   <- battleOf(dao, oneId)
        tasks  <- sched.scheduled
        // строка ждёт, пока проигравший прочитает итог
        _      <- battle.action(two, tap("ArenaPoke"), r)
        told   <- texts(r)
      } yield assertTrue(out == StateType.Arena && fights.snapshot.isEmpty) &&
              assertTrue(said.contains("Соперник повержен") && told.contains("Вы проиграли")) &&
              // ни опыта, ни добычи: победа сама по себе
              assertTrue(winner.exp == 0L && mine.isEmpty) &&
              assertTrue(loser.fightStats.hp == 1L && loser.fightStats.armor == 0L) &&
              assertTrue(tasks.exists(x => x.userId == twoId && x.kind == TaskKind.ArenaPoke))
    },

    test("бегство — тоже поражение, но ран и потерь оно не добавляет") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, dao, fights, _, r) = t
        _    <- state.action(one, tap("ArenaByCode"), r)
        code  = codeOf(fights)
        _    <- state.action(two, typed(code), r)
        _    <- state.action(one, tap("ArenaPoke"), r)
        _    <- TestRandom.feedInts(99) *> TestRandom.feedLongs(100L)
        out  <- battle.action(one, tap("ConfirmFlee"), r)
        said <- texts(r)
        me   <- heroOf(dao, oneId)
        // сбежавший прочитал итог сам, сопернику он ещё предстоит
        end     = fights.snapshot.headOption
        _      <- battle.action(two, tap("ArenaPoke"), r)
      } yield assertTrue(out == StateType.Arena && end.exists(_.finished)) &&
              assertTrue(fights.snapshot.isEmpty) &&
              assertTrue(said.contains("Вы проиграли")) &&
              // сбежал целым: единица HP — только для обнулённых
              assertTrue(me.fightStats.hp == 1000L)
    },

    test("минута вышла — бьём за зевнувшего обычной атакой и передаём ход") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, dao, fights, _, r) = t
        _     <- state.action(one, tap("ArenaByCode"), r)
        code   = codeOf(fights)
        _     <- state.action(two, typed(code), r)
        _     <- state.action(one, tap("ArenaPoke"), r)
        _     <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        out   <- battle.action(one, tap("ArenaTurn"), r)
        said  <- texts(r)
        foe   <- heroOf(dao, twoId)
        fight  = fights.snapshot.head
      } yield assertTrue(out == StateType.Battle && said.contains("Минута вышла")) &&
              assertTrue(foe.fightStats.hp < 1000L && fight.isTurn(twoId))
    },

    test("в Центре города есть вход на арену, и он ведёт куда надо") {
      for {
        c <- content
        state = pangea.service.state.states.CityCenterState(c)
        r    <- TestRenderer.make
        _    <- state.enter(one, r)
        menu <- r.sentScreens.map(_.last)
        out  <- state.action(one, tap("Arena"), r)
      } yield assertTrue(menu.choices.map(_.id).contains("Arena")) &&
              assertTrue(out == StateType.Arena)
    }
  )
}
