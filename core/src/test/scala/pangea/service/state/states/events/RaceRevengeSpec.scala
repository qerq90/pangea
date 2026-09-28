package pangea.service.state.states.events

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, KillLog}
import pangea.model.monster.{Race, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.events.RaceRevengeState.RevengeScene
import pangea.service.state.{KillLogData, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Расплата: раса, которой герой проредил ряды, приходит за своим. */
object RaceRevengeSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 20L): Hero = TestFixtures.hero(userId).copy(lvl = lvl)

  private def state(h: Hero = hero(), scene: Option[RevengeScene] = Some(RevengeScene(Race.Orc.entryName, 0))) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- ZIO.foreachDiscard(scene)(s => dao.writeSceneData(userId, s.asJson))
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (RaceRevengeState(dao, c), dao, r)

  private def battleOf(dao: TestHeroDao): Task[Option[SoloPveBattle]] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Расплата расы")(

    // ── Журнал убийств ──────────────────────────────────────────────────────

    test("в журнал идут только основные расы: элементалю и зверю мстить некому") {
      val log = KillLog.empty.add(List(
        Race.Orc.entryName, Race.Orc.entryName, Race.Elf.entryName,
        Race.Elemental.entryName, Race.Animal.entryName, Race.Undead.entryName))
      assertTrue(log.count(Race.Orc) == 2L && log.count(Race.Elf) == 1L) &&
      assertTrue(log.count(Race.Elemental) == 0L && log.count(Race.Animal) == 0L) &&
      assertTrue(log.byRace.keySet == Set(Race.Orc.entryName, Race.Elf.entryName))
    },

    test("долг появляется на 250-м убитом и закрывается приходом расы") {
      val step  = KillLogData.RevengeEvery
      val below = KillLog(byRace = Map(Race.Orc.entryName -> (step - 1)))
      val due   = KillLog(byRace = Map(Race.Orc.entryName -> step))
      val paid  = due.markAvenged(Race.Orc)
      val again = paid.copy(byRace = Map(Race.Orc.entryName -> (step * 2)))
      assertTrue(below.owed(step).isEmpty) &&
      assertTrue(due.owed(step) == List(Race.Orc)) &&
      // пришли — долг закрыт, счёт при этом не обнулился
      assertTrue(paid.owed(step).isEmpty && paid.count(Race.Orc) == step) &&
      // следующая расплата — ещё через столько же
      assertTrue(again.owed(step) == List(Race.Orc)) &&
      assertTrue(again.markAvenged(Race.Orc).owed(step).isEmpty)
    },

    test("две расы разом — обе в очереди, по одной за раз") {
      val step = KillLogData.RevengeEvery
      val log  = KillLog(byRace = Map(Race.Orc.entryName -> step * 2, Race.Elf.entryName -> step))
      val owed = log.owed(step)
      // орки злее: у них два невыплаченных долга против одного
      assertTrue(owed == List(Race.Orc, Race.Elf)) &&
      assertTrue(log.markAvenged(Race.Orc).owed(step) == List(Race.Orc, Race.Elf)) &&
      assertTrue(log.markAvenged(Race.Orc).markAvenged(Race.Orc).owed(step) == List(Race.Elf))
    },

    // ── Сцена ───────────────────────────────────────────────────────────────

    test("сцена окружения: варианты разные, кнопки — драться, отпираться, «Персонаж»") {
      for {
        t <- state()
        (s, _, r) = t
        _      <- s.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        // другой номер сцены — другой текст, и там род назван прямо
        t2 <- state(scene = Some(RevengeScene(Race.Orc.entryName, 1)))
        (s2, _, r2) = t2
        _      <- s2.enter(testUser, r2)
        other  <- r2.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.map(_.id) == List("RevengeFight", "RevengePersuade", "OpenCharacter")) &&
              // в тексте — имя пришедшего, а числа убитых нет ни в одной сцене
              assertTrue(screen.text.contains("Каркан")) &&
              assertTrue(!screen.text.exists(_.isDigit) && !other.text.exists(_.isDigit)) &&
              assertTrue(other.text != screen.text && other.text.contains("Орк"))
    },

    test("принять бой: именной и двое старших по уровню героя") {
      for {
        t <- state(hero(lvl = 30L))
        (s, dao, r) = t
        res    <- s.action(testUser, tap("RevengeFight"), r)
        battle <- battleOf(dao)
        all     = battle.map(b => b.rarity :: b.group.others.map(o => Rarity.withName(o.rarity))).getOrElse(Nil)
        said   <- texts(r)
        scene  <- dao.readSceneData(userId)
      } yield assertTrue(res == StateType.Battle && battle.exists(_.group.others.size == 2)) &&
              assertTrue(all.count(_ == Rarity.Legendary) == 1 && all.count(_ == Rarity.Mythical) == 2) &&
              // пришли за героем — и уровень у них геройский, а не по этажу
              assertTrue(battle.exists(_.monsterLvl == 30L)) &&
              assertTrue(battle.exists(_.monsterRace == Race.Orc.entryName)) &&
              assertTrue(said.contains("идут на вас разом")) &&
              // звать родню им никто не запрещал: в конце первого раунда позовут
              assertTrue(battle.exists(!_.noKin)) &&
              assertTrue(scene.contains(io.circe.Json.Null))
    },

    test("отпираться: поверят редко, не поверят — тот же бой") {
      for {
        lucky <- state()
        (s1, dao1, r1) = lucky
        _      <- TestRandom.feedInts(RaceRevengeState.PersuadePct)   // ровно на грани — верят
        ok     <- s1.action(testUser, tap("RevengePersuade"), r1)
        saidOk <- texts(r1)
        noFight <- battleOf(dao1)
        unlucky <- state()
        (s2, dao2, r2) = unlucky
        _      <- TestRandom.feedInts(RaceRevengeState.PersuadePct + 1)
        bad    <- s2.action(testUser, tap("RevengePersuade"), r2)
        saidNo <- texts(r2)
        battle <- battleOf(dao2)
      } yield assertTrue(ok == StateType.Dungeon && noFight.isEmpty && saidOk.contains("Вас пропускают")) &&
              assertTrue(bad == StateType.Battle && battle.isDefined) &&
              assertTrue(saidNo.contains("в нашей крови"))
    },

    // ── Лабиринт ────────────────────────────────────────────────────────────

    test("в лабиринте расплата выходит вперёд обычных событий и закрывает долг") {
      val debtor = hero().copy()
      for {
        dao       <- TestHeroDao.withHero(userId, debtor)
        _         <- KillLogData.write(dao, userId, KillLog(byRace = Map(
                       Race.Orc.entryName -> KillLogData.RevengeEvery,
                       Race.Elf.entryName -> KillLogData.RevengeEvery)))
        renderer  <- TestRenderer.make
        content   <- ZIO.attempt(SceneContent.load())
        scheduler <- TestScheduler.make
        dungeon    = DungeonState(dao, TestInventoryRepository.accepting, scheduler, content)
        first     <- dungeon.action(testUser, tap("FindEvent"), renderer)
        scene     <- dao.readSceneData(userId).map(_.flatMap(_.as[RevengeScene].toOption))
        log       <- KillLogData.read(dao, userId)
        // вторая раса ждала своей очереди и выходит следующей
        second    <- dungeon.action(testUser, tap("FindEvent"), renderer)
        scene2    <- dao.readSceneData(userId).map(_.flatMap(_.as[RevengeScene].toOption))
        after     <- KillLogData.read(dao, userId)
        // долгов больше нет — дальше обычные события
        third     <- dungeon.action(testUser, tap("FindEvent"), renderer)
      } yield assertTrue(first == StateType.RaceRevenge && second == StateType.RaceRevenge) &&
              assertTrue(scene.exists(_.race == Race.Orc.entryName) || scene.exists(_.race == Race.Elf.entryName)) &&
              assertTrue(scene2.exists(s => scene.exists(_.race != s.race))) &&
              assertTrue(log.avenged.values.sum == 1 && after.avenged.values.sum == 2) &&
              // счёт убитых не обнулился
              assertTrue(after.count(Race.Orc) == KillLogData.RevengeEvery) &&
              assertTrue(third != StateType.RaceRevenge)
    },

    test("в профиле рядом с уровнем видно, сколько всего убито") {
      val killer = TestFixtures.hero(userId).copy(kills = 253L)
      assertTrue(killer.getInfo(0L).contains("☠ Убито: 253"))
    }
  )
}
