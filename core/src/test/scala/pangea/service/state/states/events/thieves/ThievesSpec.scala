package pangea.service.state.states.events.thieves

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, KillLog}
import pangea.model.item.{Item, MaterialKind, TrophyKind}
import pangea.model.monster.Race
import pangea.model.quest.{BoardData, BoardKind, BoardSlot}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.InventoryState
import pangea.service.state.{GangGrudge, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Разбойники в городе»: засада в подворотне, кошельки с воров и банда,
  * которая этого не забыла. */
object ThievesSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def content = ZIO.attempt(SceneContent.load())

  private def hero(lvl: Long = 20L): Hero = TestFixtures.hero(userId).copy(lvl = lvl)

  private def alley(items: List[Item] = Nil, h: Hero = hero()) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- content
    } yield (ThievesState(dao, c), dao, inv, r, c)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def scene(count: Int, race: Race, lvl: Long) =
    ThievesScene(race.entryName, lvl, count)

  private def spoils(h: Hero = hero()) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      r   <- TestRenderer.make
      c   <- content
    } yield (ThievesSpoilsState(dao, c), dao, r)

  private val taken: BoardData =
    BoardData(0L, "novice", List(BoardSlot(BoardKind.Thieves, taken = true, lvl = 12L)))

  override def spec = suite("Разбойники в городе")(

    test("объявление: выездное, со своим уровнем и своей сложностью") {
      assertTrue(BoardKind.Thieves.away && BoardKind.Thieves.rolledLvl) &&
      assertTrue(BoardSlot(BoardKind.Thieves, lvl = 17L).difficulty == 17) &&
      assertTrue(pangea.model.quest.BoardRates.Layout.toMap.get(BoardKind.Thieves).contains(1))
    },

    test("в подворотне выходят трое-шестеро одной расы уровнем в задание") {
      for {
        t <- alley()
        (state, dao, _, r, _) = t
        _      <- dao.writeSceneData(userId, scene(count = 5, race = Race.Orc, lvl = 9L).asJson)
        // Выбирать герою не из чего: нода сама ставит бой и уводит в него.
        _      <- state.enter(testUser, r)
        out    <- state.action(testUser, tap("ThievesFight"), r)
        battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        said   <- texts(r)
        loot   <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        back    = loot.flatMap(_.eventData).flatMap(_.as[ThievesScene].toOption)
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(battle.exists(b => b.group.others.size == 4 && b.monsterRace == Race.Orc.entryName)) &&
              assertTrue(battle.exists(_.monsterLvl == 9L)) &&
              assertTrue(battle.exists(b => b.group.others.forall(_.lvl == 9L))) &&
              // вся шайка уже здесь: звать со стороны им некого
              assertTrue(battle.exists(_.noKin)) &&
              assertTrue(said.contains("это они")) &&
              // из боя вернёмся сюда же, и уже с отметкой «дрались»
              assertTrue(loot.exists(_.returnState.contains(StateType.ThievesSpoils))) &&
              assertTrue(back.exists(_.count == 5)) &&
              // кнопок в подворотне нет — ход идёт сам, без игрока
              assertTrue(state.autoAdvance.contains(StateType.Battle)) &&
              assertTrue(ThievesState.MinThieves == 3 && ThievesState.MaxThieves == 6)
    },

    test("сбежал или пал в подворотне — задание провалено и пропало с доски") {
      import pangea.model.battle.SoloPveBattle
      import pangea.model.monster.Rarity
      import pangea.model.stats.FightStats
      val thief = ThievesScene(Race.Orc.entryName, 12L, 1)
      /** Бой с одним вором; роутинг добычи тот же, что ставит подворотня. */
      def fight(h: Hero, thiefAtk: Long) =
        for {
          dao <- TestHeroDao.withHero(userId, h)
          r   <- TestRenderer.make
          c   <- content
          battle = SoloPveBattle(
                     monsterLvl = 12L, monsterRace = Race.Orc.entryName,
                     monsterRarity = Rarity.Common.entryName,
                     monsterStats = FightStats(atk = thiefAtk, hp = 100000, armor = 0, defence = 0,
                                               evasion = 0, accuracy = 9999, energy = 0),
                     monsterCurrentHp = 100000L, monsterCurrentArmor = 0L)
          _   <- dao.writeActiveBattle(userId, battle.asJson)
          _   <- dao.writeQuestData(userId, taken.asJson)
          _   <- dao.writeSceneData(userId,
                   LootData(Nil, Nil, returnState = Some(StateType.ThievesSpoils),
                            eventData = Some(thief.asJson)).asJson)
          state = pangea.service.state.states.battle.BattleState(
                    dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
        } yield (state, dao, r)
      val tough = hero().copy(fightStats = FightStats(atk = 10, hp = 100000, armor = 0, defence = 0,
                                                      evasion = 9999, accuracy = 9999, energy = 0))
      val dying = hero().copy(
        baseStats  = hero().baseStats.copy(agi = 0),
        fightStats = FightStats(atk = 10, hp = 1, armor = 0, defence = 0,
                                evasion = 0, accuracy = 9999, energy = 0))
      def boardOf(dao: TestHeroDao) =
        dao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).get)
      for {
        fled <- fight(tough, thiefAtk = 1).flatMap { case (state, dao, r) =>
                  for {
                    out   <- state.action(testUser, tap("ConfirmFlee"), r)
                    said  <- texts(r)
                    board <- boardOf(dao)
                    loot  <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
                  } yield (out, said, board, loot)
                }
        died <- fight(dying, thiefAtk = 9999).flatMap { case (state, dao, r) =>
                  for {
                    out   <- state.action(testUser, tap("Attack"), r)
                    said  <- texts(r)
                    board <- boardOf(dao)
                  } yield (out, said, board)
                }
      } yield assertTrue(fled._1 == StateType.Dungeon && fled._3.slots.isEmpty) &&
              assertTrue(fled._2.contains("не уверен, что смогу найти их во второй раз")) &&
              assertTrue(died._1 == StateType.Death && died._3.slots.isEmpty) &&
              assertTrue(died._2.contains("задание провалено")) &&
              // роутинг к экрану поживы стёрт: кошельков не будет, идти туда незачем
              assertTrue(fled._4.isEmpty)
    },

    test("после драки: кошелёк с каждого, отметка на доске и обида банды") {
      for {
        t <- spoils()
        (state, dao, r) = t
        _     <- dao.writeQuestData(userId, taken.asJson)
        _     <- dao.writeSceneData(userId, scene(count = 4, race = Race.Goblin, lvl = 12L).asJson)
        _     <- state.enter(testUser, r)
        out    = state.autoAdvance.getOrElse(StateType.GlobalMap)
        loot  <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        board <- dao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).get)
        log   <- dao.readKillLog(userId).map(_.flatMap(_.as[KillLog].toOption).get)
        said  <- texts(r)
      } yield assertTrue(out == StateType.Loot) &&
              // четыре кошелька, каждый уровнем в задание
              assertTrue(loot.exists(_.items.size == 4)) &&
              assertTrue(loot.exists(_.items.forall(i =>
                i.material.contains(MaterialKind.StolenPurse) && i.lvl == 12L))) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.GlobalMap))) &&
              assertTrue(board.slots.head.done) &&
              assertTrue(said.contains("Пустые карманы") && said.contains("Объявление закрыто")) &&
              // банда запомнила: придёт через 5–10 осмотров
              assertTrue(log.gang.contains(Race.Goblin.entryName)) &&
              assertTrue(log.gangIn >= GangGrudge.MinExplores && log.gangIn <= GangGrudge.MaxExplores) &&
              assertTrue(log.gangDue.isEmpty)
    },

    test("ход идёт сам: ни одна из нод не ждёт кнопки и не бросает героя") {
      for {
        t <- spoils()
        (state, dao, r) = t
        // сцену потеряли — добычи нет, но и в пустом экране герой не застрянет
        _    <- state.enter(testUser, r)
        loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        a1   <- alley()
      } yield assertTrue(state.autoAdvance.contains(StateType.Loot)) &&
              assertTrue(loot.exists(l => l.items.isEmpty && l.returnState.contains(StateType.GlobalMap))) &&
              // засада уводит в бой сама, без нажатия
              assertTrue(a1._1.autoAdvance.contains(StateType.Battle))
    },

    // ── Кошелёк ──────────────────────────────────────────────────────────────

    test("кошелёк: вернуть — репутация как за реликвию того же уровня") {
      val purse = ThievesState.purse(12L).copy(id = 7L)
      for {
        dao <- TestHeroDao.withHero(userId, hero())
        inv  = TestInventoryRepository.withItems(List(purse))
        r   <- TestRenderer.make
        c   <- content
        state = InventoryState(dao, inv, TestItemRepository.make, c, None)
        _     <- state.action(testUser, UserAction("", Some("""{"action":"InventoryItem_7"}""")), r)
        _     <- state.action(testUser, tap("PurseReturn"), r)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        said  <- texts(r)
      } yield assertTrue(h.guildReputation == TrophyKind.reputationFor(TrophyKind.Relic.coef, 12L)) &&
              assertTrue(h.guildReputation == 53L) &&   // ceil(5 + 12 × 4)
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(said.contains("репутации"))
    },

    test("кошелёк: развязать — от десяти до восьмисот серебра") {
      val purse = ThievesState.purse(12L).copy(id = 7L)
      for {
        dao <- TestHeroDao.withHero(userId, hero())
        inv  = TestInventoryRepository.withItems(List(purse))
        r   <- TestRenderer.make
        c   <- content
        state = InventoryState(dao, inv, TestItemRepository.make, c, None)
        _     <- state.action(testUser, UserAction("", Some("""{"action":"InventoryItem_7"}""")), r)
        _     <- state.action(testUser, tap("PurseOpen"), r)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        said  <- texts(r)
      } yield assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(h.silver >= InventoryState.PurseMinSilver) &&
              assertTrue(h.silver <= InventoryState.PurseMaxSilver || h.silver == InventoryState.PurseJackpot) &&
              assertTrue(said.contains("серебра")) &&
              // джекпот редок: один кошелёк из двухсот
              assertTrue(InventoryState.PurseJackpotIn == 200 && InventoryState.PurseJackpot == 100000L)
    },

    // ── Расплата банды ───────────────────────────────────────────────────────

    test("банда дожидается своего часа и бьёт первой, из засады") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, dungeonLevel = 10))
        r    <- TestRenderer.make
        sch  <- TestScheduler.make
        c    <- content
        state = DungeonState(dao, TestInventoryRepository.accepting, sch, c)
        h0   <- dao.getHeroByUserId(userId).map(_.get)
        // Банда уже на пороге: ещё один осмотр — и она выходит.
        _    <- dao.writeKillLog(userId, KillLog().grudge(Race.Khajiit, 1).asJson)
        out  <- state.action(testUser, tap("FindEvent"), r)
        h    <- dao.getHeroByUserId(userId).map(_.get)
        battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        log  <- dao.readKillLog(userId).map(_.flatMap(_.as[KillLog].toOption).get)
        said <- texts(r)
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(battle.exists(b => b.monsterRace == Race.Khajiit.entryName &&
                b.monsterRarity == pangea.model.monster.Rarity.Legendary.entryName)) &&
              // именной бьёт первым: в бой герой входит уже раненым, но живым
              assertTrue(h.fightStats.hp + h.fightStats.armor < h0.fightStats.hp + h0.fightStats.armor) &&
              assertTrue(h.fightStats.hp > 0L) &&
              assertTrue(said.contains("Девять теней")) &&
              // счёты сведены: второй раз эта шайка не придёт
              assertTrue(log.gang.isEmpty && log.gangDue.isEmpty)
    },

    test("из западни не сбежать") {
      for {
        dao <- TestHeroDao.withHero(userId, hero())
        r   <- TestRenderer.make
        c   <- content
        h   <- dao.getHeroByUserId(userId).map(_.get)
        m    = pangea.generator.monster.MonsterGenerator
                 .generateOfRaceAndRarity(10, Race.Khajiit, pangea.model.monster.Rarity.Legendary)
        _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(m, h).copy(noFlee = true).asJson)
        state = pangea.service.state.states.battle.BattleState(
                  dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
        out  <- state.action(testUser, tap("Flee"), r)
        said <- texts(r)
        live <- dao.readActiveBattle(userId)
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(said.contains("подстроил западню") && said.contains(m.name)) &&
              // бой никуда не делся, подтверждения бегства не предложили
              assertTrue(live.isDefined && !said.contains("Подтвердить"))
    },

    test("каждой расе — своя банда и свой удар из темноты") {
      for {
        c <- content
      } yield assertTrue(Race.mortals.forall(race => c.text(s"thieves.gang.${race.entryName}").nonEmpty)) &&
              assertTrue(Race.mortals.forall(race => c.text(s"thieves.ambushScene.${race.entryName}").nonEmpty)) &&
              // названия банд не повторяются
              assertTrue(Race.mortals.map(r => c.text(s"thieves.gang.${r.entryName}")).distinct.size ==
                Race.mortals.size) &&
              assertTrue(List("alley", "ambush", "won", "lost").forall(k => c.text(s"thieves.$k").nonEmpty)) &&
              assertTrue(List("return", "open", "returned", "opened", "jackpot")
                .forall(k => c.text(s"purse.$k").nonEmpty)) &&
              assertTrue(c.text("questBoard.ask.thieves").nonEmpty) &&
              assertTrue(c.text("questBoard.doneThieves").nonEmpty) &&
              assertTrue(c.text("battle.noFlee").nonEmpty)
    },

    test("подворотня — отдельное состояние игры") {
      assertTrue(StateType.values.contains(StateType.Thieves)) &&
      assertTrue(StateType.withName("Thieves") == StateType.Thieves)
    }
  )
}
