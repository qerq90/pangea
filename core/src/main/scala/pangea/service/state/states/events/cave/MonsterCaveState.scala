package pangea.service.state.states.events.cave

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.MaterialGenerator
import pangea.generator.loot.{LootGenerator, SchronGenerator}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{Poison, SoloPveBattle}
import pangea.model.cave.{CaveDir, CaveGenerator, CaveRates, CaveScene, RoomKind}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, MaterialKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.schedule.TaskKind
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.artifact.ArtifactIntake
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.events.cave.MonsterCaveState._
import pangea.service.state.{CharacterMenu, HerbLore, InventoryFeedback, ItemMenu, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Пещера с монстрами» (1% в лабиринте) — событие на целый поход. У входа видно
  * только чья это пещера; сколько внутри мобов (15–30), герою не говорят. На
  * пороге можно осмотреться, сходить в «Персонаж» и пустить в дело вещь из
  * сумки: сонный дурман и дымная фляга одурманивают всех обитателей сразу,
  * глефовый гриб травит их ещё до первой встречи.
  *
  * Внутри — клубок из 10–20 комнат на сетке: четыре направления, зелёные туда,
  * где есть ход, красные в стену. В комнате сперва дерутся, а находка (трава,
  * сундук, чей-то схрон, сухой угол для привала) ждёт до конца боя. Мобы стоят
  * кучками по 3–5, легендарных среди них не бывает, минибоссы сюда не заходят.
  * За последнего убитого пещера отдаёт вдвое больше опыта, чем герой взял со
  * всех её обитателей.
  *
  * Уход и смерть пещеру закрывают: недобитое и необысканное остаётся в ней. */
case class MonsterCaveState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  scheduler:     Scheduler,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "CaveEnter"     -> Target.Run { (u, _, r) => enterCave(u, r) },
      "CaveSupply"    -> Target.Run { (u, _, r) => withScene(u)(s => showSupplies(u, s, r).as(StateType.MonsterCave)) },
      "CaveSupplyOut" -> Target.Run { (u, _, r) => withScene(u)(s => showGate(u, s, r)) },
      "CavePrev"      -> Target.Run { (u, _, r) => turnPage(u, r, -1) },
      "CaveNext"      -> Target.Run { (u, _, r) => turnPage(u, r, +1) },
      "CaveForward"   -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Forward, r)) },
      "CaveBack"      -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Back, r)) },
      "CaveLeft"      -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Left, r)) },
      "CaveRight"     -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Right, r)) },
      "CaveSearch"    -> Target.Run { (u, _, r) => awake(u, r)(search(u, r)) },
      "CaveRest"      -> Target.Run { (u, _, r) => startRest(u, r) },
      "CaveRested"    -> Target.Run { (u, _, r) => wake(u, r) },
      "CaveOut"       -> Target.Run { (u, _, r) => askLeave(u, r) },
      "CaveOutYes"    -> Target.Run { (u, _, r) => leave(u, r) },
      "CaveOutNo"     -> Target.Run { (u, _, r) => withScene(u)(s => showRoom(u, s, r)) },
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.MonsterCave) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.Loot, StateType.HeroStats, StateType.MonsterCave)

  // ── Вход в событие и возвращение в него ────────────────────────────────────

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    readScene(user).flatMap {
      case None        => discover(user, renderer)
      case Some(scene) => resume(user, scene, renderer).unit
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Событие выпало впервые: катаем пещеру и ставим героя на порог. */
  private def discover(user: User, renderer: Renderer): Task[Unit] =
    for {
      raceIdx <- Random.nextIntBounded(Race.mortals.size)
      race     = Race.mortals(raceIdx)
      seed    <- Random.nextLong
      (scene, _) = CaveGenerator.generate(race.entryName, Rng(seed))
      _ <- writeScene(user, scene)
      _ <- showGate(user, scene, renderer)
    } yield ()

  /** Вернулись в пещеру — из боя, с добычи или из меню персонажа. Привал,
    * начатый до ухода, досыпается; зачищенная пещера отдаёт свою награду. */
  private def resume(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    for {
      now <- nowMs
      res <- if (!scene.inside) showGate(user, scene, renderer)
             else if (scene.restUntil > 0L && now >= scene.restUntil) wake(user, renderer)
             else if (scene.restUntil > 0L) sleeping(user, scene, now, renderer)
             else reward(user, scene, renderer).flatMap(s => showRoom(user, s, renderer))
    } yield res

  /** Порог пещеры: чья она — видно по следам, сколько их — нет. */
  private def showGate(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(
      content.format("cave.gate.text", "race" -> Race.withName(scene.race).toString),
      List(
        content.choice("CaveEnter", "cave.gate.enter").copy(color = ChoiceColor.Positive, row = Some(0)),
        content.choice("CaveSupply", "cave.gate.supply").copy(row = Some(1)),
        content.choice("OpenCharacter", "common.character").copy(row = Some(2)),
        content.choice("CaveOut", "cave.gate.leave").copy(color = ChoiceColor.Negative, row = Some(3))
      ))).as(StateType.MonsterCave)

  private def enterCave(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      val inside = scene.copy(inside = true, at = 0)
      writeScene(user, inside) *>
        renderer.show(user, Screen(content.text("cave.entered"), Nil)) *>
        showRoom(user, inside, renderer)
    }

  // ── Вещь, пущенная в дело на пороге ────────────────────────────────────────

  /** Весь скарб героя кнопками: и то, что в сумке, и надетая фляга — дымную
    * редко носят в мешке, а именно она здесь и нужна. */
  private def supplies(hero: Hero, items: List[Item]): List[Item] = {
    val flask = hero.equipment.flask
    val worn  = Option.when(flask.id > 0L && CaveSupply.charged(flask).isDefined)(flask)
    items ++ worn.toList
  }

  private def showSupplies(user: User, scene: CaveScene, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      all    = supplies(hero, inv.items.data)
      _ <- if (all.isEmpty)
             renderer.show(user, Screen(content.text("cave.supply.empty"), List(backToGate)))
           else {
             val (pageItems, pages, p) = ItemMenu.page(all, scene.page)
             val text = content.format("cave.supply.pick",
               "page" -> (p + 1).toString, "total" -> pages.toString)
             val buttons = pageItems.zipWithIndex.map { case (item, i) =>
               Choice(s"$UsePrefix${item.id}", ItemMenu.itemButtonLabel(item), row = Some(i))
             }
             val nav = List(
               Some(backToGate),
               Option.when(p > 0)(Choice("CavePrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
               Option.when(p < pages - 1)(Choice("CaveNext", content.text("common.next"), row = Some(ItemMenu.NavRow)))
             ).flatten
             renderer.show(user, Screen(text, buttons ++ nav))
           }
    } yield ()

  private def backToGate: Choice =
    content.choice("CaveSupplyOut", "cave.supply.back").copy(color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))

  private def turnPage(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    withScene(user) { scene =>
      val turned = scene.copy(page = (scene.page + delta).max(0))
      writeScene(user, turned) *> showSupplies(user, turned, renderer).as(StateType.MonsterCave)
    }

  /** Пустить вещь в дело: заряженной тратим один заряд, остальное забираем
    * целиком. Помогают только три вещи — прочее уходит впустую, о чём герою
    * честно сообщаем. Сюжетные предметы не трогаем: их не теряют нигде. */
  private def useItem(user: User, scene: CaveScene, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      chosen = supplies(hero, inv.items.data).find(_.id == itemId)
      res <- chosen match {
        case None                       => showSupplies(user, scene, renderer).as(StateType.MonsterCave)
        case Some(item) if item.isQuestItem =>
          renderer.show(user, Screen(content.format("cave.supply.quest", "name" -> item.displayTitle), Nil)) *>
            showSupplies(user, scene, renderer).as(StateType.MonsterCave)
        case Some(item) => spend(user, hero, scene, item, renderer)
      }
    } yield res

  private def spend(user: User, hero: Hero, scene: CaveScene, item: Item, renderer: Renderer): Task[StateType] = {
    val boon    = CaveSupply.boonOf(item)
    val charged = CaveSupply.charged(item)
    // Пустая фляга остаётся при герое: тратить нечего, и забирать её не за что.
    if (charged.exists(_.charges <= 0))
      renderer.show(user, Screen(content.format("cave.supply.emptyFlask", "name" -> item.displayTitle), Nil)) *>
        showSupplies(user, scene, renderer).as(StateType.MonsterCave)
    else {
      val next = boon match {
        case CaveBoon.Dope   => scene.copy(weakened = true)
        case CaveBoon.Poison => scene.copy(poisoned = true)
        case CaveBoon.None   => scene
      }
      // Второй дурман поверх первого ничего не добавляет — честно предупреждаем.
      val already = (boon == CaveBoon.Dope && scene.weakened) || (boon == CaveBoon.Poison && scene.poisoned)
      val line = boon match {
        case _ if already    => content.format("cave.supply.already", "name" -> item.displayTitle)
        case CaveBoon.Dope   => content.format("cave.supply.dope", "name" -> item.displayTitle,
                                  "pct" -> CaveRates.DopeCutPct.toString)
        case CaveBoon.Poison => content.format("cave.supply.poison", "name" -> item.displayTitle)
        case CaveBoon.None   => content.format("cave.supply.wasted", "name" -> item.displayTitle)
      }
      for {
        _ <- takeFrom(user, hero, item, charged)
        _ <- writeScene(user, next)
        _ <- renderer.show(user, Screen(line, Nil))
        _ <- showSupplies(user, next, renderer)
      } yield StateType.MonsterCave
    }
  }

  /** Заряд — из фляги (хоть надетой, хоть из сумки), всё прочее — из сумки целиком. */
  private def takeFrom(user: User, hero: Hero, item: Item, charged: Option[ItemDetails.Charged]): Task[Unit] =
    charged match {
      case Some(c) if hero.equipment.flask.id == item.id =>
        heroDao.updateEquipment(user.userId, hero.equipment.copy(flask = item.copy(details = c.spent)))
      case Some(c) =>
        inventoryRepo.updateItem(hero.id, item.copy(details = c.spent)).mapError(asThrowable).unit
      case None =>
        inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable).unit
    }

  // ── Ходьба по комнатам ─────────────────────────────────────────────────────

  private def go(user: User, dir: CaveDir, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      if (!scene.inside) showGate(user, scene, renderer)
      else scene.neighbour(dir) match {
        case None =>
          renderer.show(user, Screen(content.text("cave.wall"), Nil)) *> showRoom(user, scene, renderer)
        case Some(idx) =>
          val moved = scene.copy(at = idx)
          if (moved.room.monsters > 0) getHero(user).flatMap(fight(user, _, moved, idx, renderer))
          else writeScene(user, moved) *>
            renderer.show(user, Screen(content.text("cave.moved"), Nil)) *>
            showRoom(user, moved, renderer)
      }
    }

  /** Экран комнаты: что здесь есть и куда отсюда ведут ходы. Направления видны
    * все четыре — зелёные там, где проход, красные там, где камень. */
  private def showRoom(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] = {
    val room = scene.room
    val text = content.text(roomKey(scene))
    def dirChoice(id: String, key: String, dir: CaveDir, row: Int): Choice =
      content.choice(id, key).copy(
        color = if (scene.neighbour(dir).isDefined) ChoiceColor.Positive else ChoiceColor.Negative,
        row   = Some(row))
    // Пока в комнате есть кому драться, до находки дело не доходит.
    val action = Option.when(room.monsters <= 0 && !room.done && actionKey(room.kind).isDefined)(
      content.choice(if (room.kind == RoomKind.Rest) "CaveRest" else "CaveSearch",
        actionKey(room.kind).get).copy(row = Some(3)))
    val choices = List(
      dirChoice("CaveForward", "cave.dir.forward", CaveDir.Forward, 0),
      dirChoice("CaveLeft",    "cave.dir.left",    CaveDir.Left,    1),
      dirChoice("CaveRight",   "cave.dir.right",   CaveDir.Right,   1),
      dirChoice("CaveBack",    "cave.dir.back",    CaveDir.Back,    2)
    ) ++ action.toList ++ List(
      content.choice("OpenCharacter", "common.character").copy(row = Some(4)),
      content.choice("CaveOut", "cave.gate.leave").copy(color = ChoiceColor.Negative, row = Some(4))
    )
    writeScene(user, scene) *> renderer.show(user, Screen(text, choices)).as(StateType.MonsterCave)
  }

  /** Описание комнаты: обысканная выглядит голой, как и пустая с самого начала. */
  private def roomKey(scene: CaveScene): String = {
    val room = scene.room
    if (room.done) "cave.room.empty"
    else room.kind match {
      case RoomKind.Empty => "cave.room.empty"
      case RoomKind.Herb  => "cave.room.herb"
      case RoomKind.Chest => "cave.room.chest"
      case RoomKind.Stash => "cave.room.stash"
      case RoomKind.Rest  => if (scene.restUsed) "cave.room.restUsed" else "cave.room.rest"
    }
  }

  private def actionKey(kind: RoomKind): Option[String] = kind match {
    case RoomKind.Herb  => Some("cave.act.herb")
    case RoomKind.Chest => Some("cave.act.chest")
    case RoomKind.Stash => Some("cave.act.stash")
    case RoomKind.Rest  => Some("cave.act.rest")
    case RoomKind.Empty => None
  }

  // ── Бой в комнате ──────────────────────────────────────────────────────────

  /** Кучка мобов бросается на героя всей комнатой. Комнату помечаем зачищенной
    * заранее: вернуться из боя можно только победив, а павший герой теряет
    * пещеру целиком. */
  private def fight(user: User, hero: Hero, scene: CaveScene, idx: Int, renderer: Renderer): Task[StateType] = {
    val race  = Race.withName(scene.race)
    val count = scene.rooms(idx).monsters
    for {
      seeds   <- ZIO.foreach(List.fill(count)(()))(_ => Random.nextLong)
      monsters = seeds.map { seed =>
                   val (rarity, _) = CaveGenerator.rollRarity(Rng(seed))
                   weaken(MonsterGenerator.generateOfRaceAndRarity(hero.dungeonLevel, race, rarity), scene.weakened)
                 }
      energies <- ZIO.foreach(monsters)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => cut(MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), scene.weakened)))
      battle    = poisonAll(SoloPveBattle.fromGroup(monsters, hero, energies), scene.poisoned)
      gained    = monsters.map(expFor).sum
      next      = scene.withRoom(idx, _.copy(monsters = 0)).copy(expEarned = scene.expEarned + gained)
      routing   = LootData(Nil, Nil, returnState = Some(StateType.MonsterCave), eventData = Some(next.asJson))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, routing.asJson)
      _ <- renderer.show(user, Screen(content.format("cave.ambush", "count" -> count.toString), Nil))
    } yield StateType.Battle
  }

  /** Одурманенный моб бьёт слабее и копит энергию медленнее. */
  private def weaken(m: Monster, on: Boolean): Monster =
    if (!on) m
    else m.copy(fightStats = m.fightStats.copy(
      atk    = cut(m.fightStats.atk, on).max(1L),
      energy = cut(m.fightStats.energy, on)))

  private def cut(value: Long, on: Boolean): Long =
    if (on) value * (100L - CaveRates.DopeCutPct) / 100L else value

  /** Глефовый гриб: отравлены все, и тот, кто в паре, и те, кто в строю. */
  private def poisonAll(battle: SoloPveBattle, on: Boolean): SoloPveBattle =
    if (!on) battle
    else {
      val poison  = Some(Poison.onHit)
      val paired  = battle.withEffects(battle.effects.copy(monsterPoison = poison))
      val inLine  = paired.group.others.map(s => s.copy(effects = s.effects.copy(monsterPoison = poison)))
      paired.copy(group = paired.group.copy(others = inLine))
    }

  // ── Находки комнат ─────────────────────────────────────────────────────────

  private def search(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      val room = scene.room
      if (room.done || room.monsters > 0) showRoom(user, scene, renderer)
      else room.kind match {
        case RoomKind.Herb  => pickHerb(user, scene, renderer)
        case RoomKind.Chest => openChest(user, scene, renderer)
        case RoomKind.Stash => openStash(user, scene, renderer)
        case _              => showRoom(user, scene, renderer)
      }
    }

  /** Трава в трещине: как сорванный в поле цветок — что именно сорвано, зависит
    * от знаний героя. Волков в пещере нет, за спину тут никто не заходит. */
  private def pickHerb(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      rankRoll <- Random.nextIntBetween(1, 101)
      rank      = if (rankRoll <= HerbLore.RareHerbPct) 2 else 1
      pool      = MaterialKind.herbsOfRank(rank)
      idx      <- Random.nextIntBounded(pool.size)
      lore     <- HerbLore.readLore(heroDao, user.userId)
      item      = MaterialGenerator.item(HerbLore.recognised(lore, pool(idx)))
      persisted <- itemRepo.persist(hero.id, item)
      intake    <- ArtifactIntake.accept(artifacts, inventoryRepo, hero.id, persisted)
      where     <- InventoryFeedback.intakeLine(inventoryRepo, content, hero.id, persisted, intake)
      _ <- renderer.show(user, Screen(
             content.format("cave.herb", "flower" -> persisted.name) + "\n" + where, Nil))
      done = scene.withRoom(scene.at, _.copy(done = true))
      res <- showRoom(user, done, renderer)
    } yield res

  /** Сундук: чаще всего в нём то же, что берут с мобов четвёртого тира, но
    * иногда — большая руна, ради которой такие сундуки и вскрывают. */
  private def openChest(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      roll <- Random.nextIntBetween(1, 101)
      seed <- Random.nextLong
      loot  = if (roll <= CaveRates.ChestRunePct) {
                val (rune, _) = Rng(seed).pick(RuneStone.all)
                LootData(items = List(RuneStone.item(rune, RuneStoneSize.Big)), silvers = Nil)
              } else {
                val (drops, _) = LootGenerator.roll(Rarity.Mythical, Race.withName(scene.race),
                                   hero.dungeonLevel.toLong, Rng(seed),
                                   gearChanceBonusPct = hero.gems.gearDropBonusPct)
                LootData(
                  items     = drops.flatMap(_.itemOpt),
                  silvers   = drops.collect { case LootGenerator.LootDrop.Silver(a, _) => a },
                  doubloons = drops.collect { case LootGenerator.LootDrop.Doubloons(a) => a }.sum)
              }
      _ <- renderer.show(user, Screen(content.text("cave.chest"), Nil))
      _ <- handOver(user, scene, loot)
    } yield StateType.Loot

  /** Схрон: тот же, что прикопан в лабиринте, только копать не надо. */
  private def openStash(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      seed <- Random.nextLong
      (reward, _) = SchronGenerator.roll(Race.withName(scene.race), hero.dungeonLevel.toLong,
                      CaveRates.StashDoubloonMin, CaveRates.StashDoubloonMax, Rng(seed))
      loot = LootData(
               items     = reward.items,
               silvers   = if (reward.silver > 0L) List(reward.silver) else Nil,
               doubloons = reward.doubloons)
      _ <- renderer.show(user, Screen(content.text("cave.stash"), Nil))
      _ <- handOver(user, scene, loot)
    } yield StateType.Loot

  /** Отдать находку экрану добычи и условиться, что он вернёт героя в пещеру —
    * в ту же комнату, уже обысканную. */
  private def handOver(user: User, scene: CaveScene, loot: LootData): Task[Unit] =
    writeSceneData(user, loot.copy(
      returnState = Some(StateType.MonsterCave),
      eventData   = Some(scene.withRoom(scene.at, _.copy(done = true)).asJson)))

  // ── Привал ─────────────────────────────────────────────────────────────────

  /** Сухой угол: перевести дух в пещере можно один раз — второго такого места
    * герой уже не найдёт. */
  private def startRest(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      if (scene.restUsed || scene.room.kind != RoomKind.Rest) showRoom(user, scene, renderer)
      else
        for {
          now  <- nowMs
          till  = now + CaveRates.RestMs
          _    <- writeScene(user, scene.copy(restUntil = till, restUsed = true))
          _    <- scheduler.schedule(user.userId, till, TaskKind.CaveRest, StateType.MonsterCave, RestAction)
          _    <- renderer.show(user, Screen(content.text("cave.rest.start"), Nil, hideKeyboard = true))
        } yield StateType.MonsterCave
    }

  private def sleeping(user: User, scene: CaveScene, now: Long, renderer: Renderer): Task[StateType] = {
    val left = ((scene.restUntil - now) / 1000L).max(1L)
    renderer.show(user, Screen(content.format("cave.rest.waiting", "remaining" -> s"${left}с"), Nil, hideKeyboard = true))
      .as(StateType.MonsterCave)
  }

  /** Привал окончен: герой и отряд как новенькие. */
  private def wake(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      for {
        now  <- nowMs
        hero <- getHero(user)
        _    <- scheduler.cancel(user.userId, TaskKind.CaveRest)
        _    <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(
                  hp = hero.effectiveMaxHp(now), armor = hero.effectiveMaxArmor(now), energy = hero.maxEnergy(now)))
        _    <- heroDao.updateSquad(user.userId, hero.squad.restored(hero.lvl))
        rested = scene.copy(restUntil = 0L).withRoom(scene.at, _.copy(done = true))
        _    <- renderer.show(user, Screen(content.text("cave.rest.done"), Nil))
        res  <- showRoom(user, rested, renderer)
      } yield res
    }

  // ── Зачистка и уход ────────────────────────────────────────────────────────

  /** Последний обитатель пал — пещера отдаёт вдвое больше опыта, чем герой взял
    * со всех её мобов. Награда одна на пещеру. Из пещеры это героя не выводит:
    * необысканные углы остаются на месте, и уходит он сам, когда захочет. */
  private def reward(user: User, scene: CaveScene, renderer: Renderer): Task[CaveScene] =
    if (!scene.cleared || scene.rewarded || scene.expEarned <= 0L) ZIO.succeed(scene)
    else {
      val bonus = scene.expEarned * CaveRates.ClearExpFactor
      for {
        hero   <- getHero(user)
        leveled = hero.gainExp(bonus)
        _      <- heroDao.updateExpAndLevel(user.userId, leveled.exp, leveled.lvl, leveled.upgradePoints)
        _      <- renderer.show(user, Screen(content.format("cave.cleared", "exp" -> bonus.toString), Nil))
        _      <- ZIO.when(leveled.lvl > hero.lvl)(
                    renderer.show(user, Screen(s"Вы получили новый уровень ${leveled.lvl}!", Nil)))
        // Отдельной строкой, чтобы это не потерялось среди опыта и уровня.
        _      <- renderer.show(user, Screen(content.text("cave.allClear"), Nil))
        done    = scene.copy(rewarded = true)
        _      <- writeScene(user, done)
      } yield done
    }

  private def askLeave(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      if (!scene.inside) leave(user, renderer)
      else renderer.show(user, Screen(content.text("cave.confirmLeave.text"),
        content.screen("cave.confirmLeave").choices)).as(StateType.MonsterCave)
    }

  private def leave(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.CaveRest) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("cave.left"), Nil)).as(StateType.Dungeon)

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(UsePrefix) =>
        withScene(user) { scene =>
          a.drop(UsePrefix.length).toLongOption
            .fold(showSupplies(user, scene, renderer).as(StateType.MonsterCave: StateType))(
              useItem(user, scene, _, renderer))
        }
      case _ => awake(user, renderer)(
        withScene(user)(s => if (s.inside) showRoom(user, s, renderer) else showGate(user, s, renderer)))
    }

  /** Спящий герой по пещере не ходит: пока тянется привал, любое действие,
    * кроме ухода, упирается в дрёму. Проспал дольше таймера (поллер не достал
    * его в меню персонажа) — просыпаемся прямо здесь. */
  private def awake(user: User, renderer: Renderer)(f: => Task[StateType]): Task[StateType] =
    readScene(user).flatMap {
      case Some(scene) if scene.restUntil > 0L =>
        nowMs.flatMap(now =>
          if (now >= scene.restUntil) wake(user, renderer) else sleeping(user, scene, now, renderer))
      case _ => f
    }

  /** Действие над начатой пещерой. Сцены нет (пришли не оттуда или её стёрли) —
    * возвращаем героя в лабиринт, а не роняем экран. */
  private def withScene(user: User)(f: CaveScene => Task[StateType]): Task[StateType] =
    readScene(user).flatMap {
      case Some(scene) => f(scene)
      case None        => ZIO.succeed(StateType.Dungeon)
    }

  private def readScene(user: User): Task[Option[CaveScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[CaveScene].toOption))

  private def writeScene(user: User, scene: CaveScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def writeSceneData(user: User, loot: LootData): Task[Unit] =
    heroDao.writeSceneData(user.userId, loot.asJson)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object MonsterCaveState {
  /** Префикс кнопки «пустить эту вещь в дело» перед входом. */
  val UsePrefix: String = "CaveUse_"

  /** payload синтетического действия, которым поллер будит героя после привала. */
  val RestAction: String = """{"action":"CaveRested"}"""

  /** Опыт за одного моба пещеры — та же формула, что и в бою: этаж на редкость. */
  def expFor(m: Monster): Long = (m.lvl.toDouble * m.rarity.factor).toLong.max(1L)
}
