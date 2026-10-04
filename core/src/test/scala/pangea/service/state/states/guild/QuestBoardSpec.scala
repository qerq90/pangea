package pangea.service.state.states.guild

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.monster.Race
import pangea.model.quest._
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{BoardProgress, UserAction}
import pangea.test._
import zio.ZIO
import zio.test._

/** Доска заданий гильдии: своя на каждые двадцать пять уровней, переписывается
  * в понедельник, берут с неё хоть всё разом. */
object QuestBoardSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def trophy(id: Long, race: Race, lvl: Long, kind: TrophyKind = TrophyKind.Head): Item =
    Item(id, s"${kind.displayName} ($race)", lvl, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy(race.entryName, kind))

  private def hero(lvl: Long): Hero =
    TestFixtures.hero(userId).copy(lvl = lvl, exp = 0L, doubloons = 0L)

  /** Полдень понедельника 5 октября 2026 по Москве. */
  private val monday = 1791183600000L

  private def board(lvl: Long = 10L, items: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero(lvl))
      inv       = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (QuestBoardState(heroDao, inv, content), heroDao, inv, renderer)

  private def saved(dao: TestHeroDao) =
    dao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).get)

  override def spec = suite("Доска заданий гильдии")(

    test("неделя считается с понедельника по Москве") {
      val mondayMidnight = BoardRates.nextWeekAt(monday)
      assertTrue(BoardRates.weekOf(mondayMidnight) == BoardRates.weekOf(monday) + 1L) &&
      // минута до полуночи — ещё та неделя, минута после — уже новая
      assertTrue(BoardRates.weekOf(mondayMidnight - 60000L) == BoardRates.weekOf(monday)) &&
      assertTrue(BoardRates.weekOf(mondayMidnight + 60000L) == BoardRates.weekOf(monday) + 1L) &&
      // от понедельника до понедельника ровно семь суток
      assertTrue(BoardRates.nextWeekAt(mondayMidnight) - mondayMidnight == 7L * DailyRates.DayMs)
    },

    test("разделы: 1–25, 26–50 и дальше до потолка, свой — по уровню героя") {
      assertTrue(BoardTier.values.size == 6) &&
      assertTrue(BoardTier.of(1L) == BoardTier.Novice && BoardTier.of(25L) == BoardTier.Novice) &&
      assertTrue(BoardTier.of(26L) == BoardTier.Seasoned && BoardTier.of(150L) == BoardTier.Legend) &&
      // разделы идут встык, без дыр и нахлёста
      assertTrue(BoardTier.values.sliding(2).forall { case Seq(a, b) => b.from == a.to + 1L; case _ => true })
    },

    test("свежая доска: восемь объявлений, караван и пещера по одному, расы трофеев не повторяются") {
      val slots  = QuestBoardState.roll(Rng(7L))
      val races  = slots.flatMap(_.race)
      assertTrue(slots.size == BoardRates.Slots) &&
      assertTrue(slots.count(_.kind == BoardKind.CaravanRout) == 1) &&
      assertTrue(slots.count(_.kind == BoardKind.CaveClear) == 1) &&
      assertTrue(slots.count(_.kind == BoardKind.Trophy) == 6) &&
      assertTrue(races.size == 6 && races.distinct.size == 6) &&
      // сложности: трофей — один знак, караван и пещера — пятнадцать
      assertTrue(BoardKind.Trophy.difficulty == 1 && BoardKind.CaravanRout.difficulty == 15) &&
      assertTrue(BoardKind.CaveClear.difficulty == 15)
    },

    test("стена досок: своя открыта, чужая отвечает своим отказом") {
      for {
        t <- board(lvl = 10L)
        (state, _, _, r) = t
        _      <- state.enter(testUser, r)
        wall   <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("BoardTier", "tier" -> BoardTier.Veteran.key), r)
        locked <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        out    <- state.action(testUser, tap("BoardMine"), r)
        mine   <- r.sentScreens.map(_.last)
      } yield assertTrue(wall.choices.count(_.id == "BoardTier") == 5) &&
              assertTrue(wall.choices.count(_.id == "BoardMine") == 1) &&
              assertTrue(locked.contains("Зал ветеранов")) &&
              assertTrue(out == StateType.QuestBoard && mine.text.contains(BoardTier.Novice.title))
    },

    test("брать можно хоть всё разом") {
      for {
        t <- board()
        (state, dao, _, r) = t
        _    <- state.action(testUser, tap("BoardMine"), r)
        _    <- ZIO.foreachDiscard(0 until BoardRates.Slots)(i =>
                  state.action(testUser, tap(s"${QuestBoardState.TakePrefix}$i"), r))
        data <- saved(dao)
      } yield assertTrue(data.slots.forall(_.taken) && data.slots.size == BoardRates.Slots)
    },

    test("трофейное: уходит самый дорогой подходящий, платят опытом и дублоном") {
      // мешок и реликвия одной расы: реликвия дороже по коэффициенту
      val sack   = trophy(1L, Race.Orc, 10L, TrophyKind.Sack)
      val relic  = trophy(2L, Race.Orc, 10L, TrophyKind.Relic)
      val alien  = trophy(3L, Race.Elf, 10L, TrophyKind.Relic)
      for {
        t <- board(items = List(sack, relic, alien))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, tap("BoardMine"), r)
        // кладём на доску заведомо орочье задание
        fresh <- saved(dao)
        orcs   = fresh.copy(slots = List(BoardSlot(BoardKind.Trophy, Some(Race.Orc.entryName), taken = true)))
        _     <- dao.writeQuestData(userId, orcs.asJson)
        _     <- state.action(testUser, tap(s"${QuestBoardState.HandPrefix}0"), r)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        h     <- dao.getHeroByUserId(userId).map(_.get)
        data  <- saved(dao)
      } yield assertTrue(inv.snapshot.map(_.id).toSet == Set(1L, 3L)) &&
              assertTrue(h.doubloons == BoardRates.TrophyDoubloons) &&
              assertTrue(h.exp == BoardRates.trophyExp(10L, TrophyKind.Relic.coef) || h.lvl > 10L) &&
              // объявление снято с доски
              assertTrue(data.slots.isEmpty && said.contains("уходит в гильдию"))
    },

    test("без трофея сдать нельзя, и сумку не трогают") {
      for {
        t <- board(items = List(trophy(1L, Race.Elf, 5L)))
        (state, dao, inv, r) = t
        _    <- state.action(testUser, tap("BoardMine"), r)
        fresh <- saved(dao)
        orcs  = fresh.copy(slots = List(BoardSlot(BoardKind.Trophy, Some(Race.Orc.entryName), taken = true)))
        _    <- dao.writeQuestData(userId, orcs.asJson)
        _    <- state.action(testUser, tap(s"${QuestBoardState.HandPrefix}0"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        data <- saved(dao)
      } yield assertTrue(said.contains("Такого трофея при вас нет")) &&
              assertTrue(inv.snapshot.size == 1 && data.slots.size == 1 && !data.slots.head.done)
    },

    test("караван и пещера отмечаются в пути, а платят на доске") {
      for {
        t <- board()
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("BoardMine"), r)
        fresh  <- saved(dao)
        taken   = fresh.copy(slots = List(
                    BoardSlot(BoardKind.CaravanRout, taken = true),
                    BoardSlot(BoardKind.CaveClear,   taken = true)))
        _      <- dao.writeQuestData(userId, taken.asJson)
        // пока дело не сделано — платить не за что
        _      <- state.action(testUser, tap(s"${QuestBoardState.HandPrefix}0"), r)
        early  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        hunting <- BoardProgress.hunting(dao, userId, BoardKind.CaravanRout)
        // лабиринт отмечает разгром
        marked <- BoardProgress.markDone(dao, userId, BoardKind.CaravanRout)
        _      <- state.action(testUser, tap(s"${QuestBoardState.HandPrefix}0"), r)
        h      <- dao.getHeroByUserId(userId).map(_.get)
        data   <- saved(dao)
      } yield assertTrue(early.contains("Дело ещё не сделано") && hunting && marked) &&
              assertTrue(h.doubloons == BoardRates.Doubloons) &&
              assertTrue(h.exp == BoardRates.exp(10L) || h.lvl > 10L) &&
              // закрыто только объявление каравана, пещера осталась висеть
              assertTrue(data.slots.map(_.kind) == List(BoardKind.CaveClear))
    },

    test("в понедельник доска переписывается вместе со взятым") {
      for {
        t <- board()
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("BoardMine"), r)
        _      <- state.action(testUser, tap(s"${QuestBoardState.TakePrefix}0"), r)
        before <- saved(dao)
        // отматываем доску на прошлую неделю — как будто наступил понедельник
        _      <- dao.writeQuestData(userId, before.copy(week = before.week - 1L).asJson)
        _      <- state.action(testUser, tap("BoardMine"), r)
        after  <- saved(dao)
      } yield assertTrue(before.slots.head.taken) &&
              assertTrue(after.week == before.week && after.slots.size == BoardRates.Slots) &&
              assertTrue(after.slots.forall(!_.taken))
    },

    test("перерос раздел — доска меняется на новую") {
      for {
        t <- board(lvl = 25L)
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("BoardMine"), r)
        novice <- saved(dao)
        grown   = hero(26L)
        _      <- dao.insertHero(grown)
        _      <- state.action(testUser, tap("BoardMine"), r)
        next   <- saved(dao)
      } yield assertTrue(novice.board == BoardTier.Novice && next.board == BoardTier.Seasoned)
    },

    test("у всех разделов и видов заданий есть тексты") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(BoardTier.values.forall(t => c.text(s"questBoard.locked.${t.key}").nonEmpty)) &&
              assertTrue(BoardKind.values.forall(k => c.text(s"questBoard.ask.${k.key}").nonEmpty)) &&
              assertTrue(List("markCaravan", "markCave", "doneCaravan", "doneCave", "taken", "paid",
                "paidTrophy", "noTrophy", "notYet").forall(f => c.text(s"questBoard.$f").nonEmpty))
    }
  )
}
