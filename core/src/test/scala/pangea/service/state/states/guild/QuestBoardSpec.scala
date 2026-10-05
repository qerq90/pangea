package pangea.service.state.states.guild

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.cave.SewerRates
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.monster.Race
import pangea.model.quest._
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.road.RoadProgress
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
      sched    <- TestScheduler.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (QuestBoardState(heroDao, inv, sched, content), heroDao, inv, renderer)

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

    test("свежая доска: восемь объявлений, тяжёлые по одному, расы трофеев не повторяются") {
      val slots  = QuestBoardState.roll(Rng(7L))
      val races  = slots.flatMap(_.race)
      assertTrue(slots.size == BoardRates.Slots) &&
      assertTrue(slots.count(_.kind == BoardKind.CaravanRout) == 1) &&
      assertTrue(slots.count(_.kind == BoardKind.CaveClear) == 1) &&
      assertTrue(slots.count(_.kind == BoardKind.SewerRats) == 1) &&
      assertTrue(slots.count(_.kind == BoardKind.Thieves) == 1) &&
      assertTrue(slots.count(_.kind == BoardKind.DwarfSupply) == 1) &&
      assertTrue(slots.count(_.kind == BoardKind.Trophy) == 3) &&
      assertTrue(races.size == 3 && races.distinct.size == 3) &&
      // сложности: трофей — один знак, караван и пещера — пятнадцать
      assertTrue(BoardKind.Trophy.difficulty == 1 && BoardKind.CaravanRout.difficulty == 15) &&
      assertTrue(BoardKind.CaveClear.difficulty == 15)
    },

    test("канализация: уровень задания 1–25 и он же сложность объявления") {
      val sewers = (1L to 300L).toList.map(s =>
        QuestBoardState.roll(Rng(s)).find(_.kind == BoardKind.SewerRats).get)
      val lvls   = sewers.map(_.lvl)
      assertTrue(lvls.forall(l => l >= SewerRates.MinLvl && l <= SewerRates.MaxLvl)) &&
      // уровень катается, а не стоит на месте, и всю шкалу задевает
      assertTrue(lvls.distinct.size > 15) &&
      // сложность объявления — его собственный уровень, а не ставка вида
      assertTrue(sewers.forall(s => s.difficulty == s.lvl.toInt)) &&
      // у прочих видов сложность по-прежнему общая
      assertTrue(BoardSlot(BoardKind.CaveClear).difficulty == BoardKind.CaveClear.difficulty)
    },

    test("выездное уводит от доски в дорогу, а поллеру оставляет задачу") {
      for {
        t <- board()
        (state, dao, _, r) = t
        _     <- state.action(testUser, tap("BoardMine"), r)
        fresh <- saved(dao)
        idx    = fresh.slots.indexWhere(_.kind == BoardKind.SewerRats)
        out   <- state.action(testUser, tap(s"${QuestBoardState.TakePrefix}$idx"), r)
        data  <- saved(dao)
        road  <- dao.readSceneData(userId).map(_.flatMap(_.as[RoadProgress].toOption).get)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        last  <- r.sentScreens.map(_.last)
      } yield assertTrue(out == StateType.QuestRoad) &&
              assertTrue(data.slots(idx).taken && road.kind == BoardKind.SewerRats) &&
              assertTrue(road.lvl == data.slots(idx).lvl && road.lvl > 0L) &&
              assertTrue(said.contains("выходите из гильдии")) &&
              // на дороге кнопок нет: клавиатуру убираем
              assertTrue(last.choices.isEmpty && last.hideKeyboard)
    },

    test("с выездного можно уйти и выдвинуться к нему снова") {
      for {
        t <- board()
        (state, dao, _, r) = t
        _     <- state.action(testUser, tap("BoardMine"), r)
        fresh <- saved(dao)
        idx    = fresh.slots.indexWhere(_.kind == BoardKind.SewerRats)
        _     <- state.action(testUser, tap(s"${QuestBoardState.TakePrefix}$idx"), r)
        // герой ушёл с задания, сцена пуста — и он снова у доски
        _     <- dao.writeSceneData(userId, io.circe.Json.Null)
        again <- state.action(testUser, tap("BoardMine"), r)
        board <- r.sentScreens.map(_.last)
        out   <- state.action(testUser, tap(s"${QuestBoardState.GoPrefix}$idx"), r)
        road  <- dao.readSceneData(userId).map(_.flatMap(_.as[RoadProgress].toOption))
      } yield assertTrue(again == StateType.QuestBoard) &&
              assertTrue(board.choices.exists(_.id == s"${QuestBoardState.GoPrefix}$idx")) &&
              assertTrue(out == StateType.QuestRoad && road.exists(_.kind == BoardKind.SewerRats))
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

    test("заказ гномов: десять склянок одного толка, и всё разом") {
      val orders = (1L to 300L).toList.map(s =>
        QuestBoardState.roll(Rng(s)).find(_.kind == BoardKind.DwarfSupply).get)
      val brews  = orders.flatMap(_.brew)
      assertTrue(BoardKind.DwarfSupply.difficulty == 5 && BoardRates.BrewsWanted == 10) &&
      // отвар всегда первого ранга, и он катается
      assertTrue(brews.forall(b => BrewKind.rank1.exists(_.entryName == b))) &&
      assertTrue(brews.distinct.size >= 5) &&
      // дружина тоже катается, и ходить за этим никуда не надо
      assertTrue(orders.map(_.band).distinct.size > 50) &&
      assertTrue(!BoardKind.DwarfSupply.away && !BoardKind.DwarfSupply.rolledLvl)
    },

    test("ящик берут полным: девяти склянок мало, десять уходят разом") {
      val kind  = BrewKind.rank1.head
      def flask(id: Long) = BrewKind.item(kind).copy(id = id)
      val nine  = (1L to 9L).toList.map(flask)
      val twelve = (1L to 12L).toList.map(flask)
      def order(items: List[Item]) =
        for {
          t <- board(items = items)
          (state, dao, inv, r) = t
          _     <- state.action(testUser, tap("BoardMine"), r)
          fresh <- saved(dao)
          one    = fresh.copy(slots = List(BoardSlot(BoardKind.DwarfSupply,
                     taken = true, brew = Some(kind.entryName), band = 3)))
          _     <- dao.writeQuestData(userId, one.asJson)
          _     <- state.action(testUser, tap(s"${QuestBoardState.HandPrefix}0"), r)
          said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
          data  <- saved(dao)
          h     <- dao.getHeroByUserId(userId).map(_.get)
        } yield (said, data, inv.snapshot.size, h)
      for {
        few  <- order(nine)
        many <- order(twelve)
      } yield assertTrue(few._1.contains("Ящик берут только полным")) &&
              // девять остались при герое, объявление висит
              assertTrue(few._3 == 9 && few._2.slots.size == 1 && few._4.doubloons == 0L) &&
              // из двенадцати уходит ровно десять
              assertTrue(many._3 == 2 && many._2.slots.isEmpty) &&
              assertTrue(many._4.doubloons == BoardRates.Doubloons) &&
              assertTrue(many._1.contains("пересчитывают его дважды"))
    },

    test("у всех разделов и видов заданий есть тексты") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(BoardTier.values.forall(t => c.text(s"questBoard.locked.${t.key}").nonEmpty)) &&
              assertTrue(BoardKind.values.forall(k => c.text(s"questBoard.ask.${k.key}").nonEmpty)) &&
              assertTrue(List("markCaravan", "markCave", "doneCaravan", "doneCave", "doneSewer", "taken",
                "paid", "paidTrophy", "noTrophy", "notYet", "stateAway", "goLabel", "depart")
                .forall(f => c.text(s"questBoard.$f").nonEmpty)) &&
              assertTrue(List("enter", "wait", "arrived", "lost").forall(f => c.text(s"questRoad.$f").nonEmpty)) &&
              assertTrue(List("stateBrews", "noBrews", "paidBrews").forall(f => c.text(s"questBoard.$f").nonEmpty)) &&
              assertTrue(c.list("questBoard.bands").size >= 10) &&
              assertTrue(c.list("questBoard.bands").distinct.size == c.list("questBoard.bands").size)
    }
  )
}
