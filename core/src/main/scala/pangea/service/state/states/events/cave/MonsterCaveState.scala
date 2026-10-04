package pangea.service.state.states.events.cave

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.MaterialGenerator
import pangea.generator.loot.{LootGenerator, SchronGenerator, TreasureHuntGenerator}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{Poison, SoloPveBattle}
import pangea.model.cave.{CaveDir, CaveGenerator, CaveRates, CaveScene, RoomKind}
import pangea.model.hero.{Hero, Knowledge}
import pangea.generator.item.GemGenerator
import pangea.model.item.{GemKind, Item, ItemDetails, ItemType, MapZone, MaterialKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.squad.UndeadForm
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
import pangea.model.quest.{BoardKind, Difficulty}
import pangea.service.state.{BoardProgress, CharacterMenu, HerbLore, InventoryFeedback, ItemMenu, State, UserAction}
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
  * За последнего убитого пещера докладывает большую долю опыта, взятого со
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
      "CaveSupplyOut" -> Target.Run { (u, _, r) =>
        withScene(u)(s => if (s.inside) showRoom(u, s, r) else showGate(u, s, r)) },
      "CavePrev"      -> Target.Run { (u, _, r) => turnPage(u, r, -1) },
      "CaveNext"      -> Target.Run { (u, _, r) => turnPage(u, r, +1) },
      "CaveForward"   -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Forward, r)) },
      "CaveBack"      -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Back, r)) },
      "CaveLeft"      -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Left, r)) },
      "CaveRight"     -> Target.Run { (u, _, r) => awake(u, r)(go(u, CaveDir.Right, r)) },
      "CaveSearch"    -> Target.Run { (u, _, r) => awake(u, r)(search(u, r)) },
      "CaveDown"      -> Target.Run { (u, _, r) => awake(u, r)(descend(u, r)) },
      "CaveAltar"     -> Target.Run { (u, _, r) => awake(u, r)(withScene(u)(s => showSupplies(u, s, r).as(StateType.MonsterCave))) },
      "CaveSwap"      -> Target.Run { (u, ua, r) => swap(u, ua, r) },
      "CaveSwapNo"    -> Target.Run { (u, _, r) => withScene(u)(s => cancelSwap(u, s, r)) },
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
    Set(StateType.Dungeon, StateType.Battle, StateType.Loot, StateType.HeroStats,
        StateType.MonsterCave, StateType.GlobalMap)

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
      // Взято задание с доски — говорим сразу, что эта пещера годится.
      hunt <- BoardProgress.hunting(heroDao, user.userId, BoardKind.CaveClear)
      _ <- ZIO.when(hunt)(renderer.show(user, Screen(content.text("questBoard.markCave"), Nil)))
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

  /** Порог пещеры: чья она — видно по следам, сколько их — нет. У канализации
    * гадать не о чем: герой пришёл сюда по объявлению и знает, за кем. */
  private def showGate(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(
      if (scene.sewer) content.format("sewer.gate.text", "lvl" -> Difficulty.render(scene.questLvl.toInt))
      else content.format("cave.gate.text", "race" -> Race.withName(scene.race).toString),
      List(
        content.choice("CaveEnter", key(scene, "gate.enter")).copy(color = ChoiceColor.Positive, row = Some(0)),
        content.choice("CaveSupply", "cave.gate.supply").copy(row = Some(1)),
        content.choice("OpenCharacter", "common.character").copy(row = Some(2)),
        content.choice("CaveOut", "cave.gate.leave").copy(color = ChoiceColor.Negative, row = Some(3))
      ))).as(StateType.MonsterCave)

  private def enterCave(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      val inside = scene.copy(inside = true, at = 0)
      writeScene(user, inside) *>
        renderer.show(user, Screen(txt(scene, "entered"), Nil)) *>
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
             val text = content.format(if (atAltar(scene)) "cave.altar.pick" else "cave.supply.pick",
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
        case Some(item) if atAltar(scene) => offer(user, hero, scene, item, renderer)
        case Some(item)                   => spend(user, hero, scene, item, renderer)
      }
    } yield res

  private def spend(user: User, hero: Hero, scene: CaveScene, item: Item, renderer: Renderer): Task[StateType] = {
    val boon    = CaveSupply.boonOf(item)
    val charged = CaveSupply.charged(item)
    // Карта клада читается отдельно от прочего скарба: она не дурманит пещеру,
    // а показывает, что клад спрятан именно в ней.
    if (item.isTreasureMap) readMap(user, hero, scene, item, renderer)
    // Пустая фляга остаётся при герое: тратить нечего, и забирать её не за что.
    else if (charged.exists(_.charges <= 0))
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
        case _ if already    => content.format(key(scene, "supply.already"), "name" -> item.displayTitle)
        case CaveBoon.Dope   => content.format(key(scene, "supply.dope"), "name" -> item.displayTitle,
                                  "pct" -> CaveRates.DopeCutPct.toString)
        case CaveBoon.Poison => content.format("cave.supply.poison", "name" -> item.displayTitle)
        case CaveBoon.None   => content.format(key(scene, "supply.wasted"), "name" -> item.displayTitle)
      }
      for {
        _ <- takeFrom(user, hero, item, charged)
        _ <- writeScene(user, next)
        _ <- renderer.show(user, Screen(line, Nil))
        _ <- showSupplies(user, next, renderer)
      } yield StateType.MonsterCave
    }
  }

  /** Карта клада, приложенная к пещере. Целая карта сходится с этой пещерой,
    * тратится и открывает в ней комнату с кладом; вторая карта класть уже
    * некуда — её герой оставляет себе. Половинку читать бессмысленно: на ней
    * не видно, где копать, и портить её незачем. */
  private def readMap(user: User, hero: Hero, scene: CaveScene, map: Item, renderer: Renderer): Task[StateType] =
    (map.itemType, map.details) match {
      case (ItemType.TreasureMap, ItemDetails.TreasureMap(zone)) if scene.treasure.isEmpty =>
        for {
          seed <- Random.nextLong
          (dug, _) = CaveGenerator.addTreasureRoom(scene.copy(treasure = Some(zone)), Rng(seed))
          _ <- inventoryRepo.removeItem(map.id, hero.id).mapError(asThrowable)
          _ <- writeScene(user, dug)
          _ <- renderer.show(user, Screen(txt(scene, "supply.map"), Nil))
          _ <- showSupplies(user, dug, renderer)
        } yield StateType.MonsterCave
      case (ItemType.TreasureMap, _) => say(user, scene, "supply.mapAlready", renderer)
      case _                         => say(user, scene, "supply.mapHalf", renderer)
    }

  // ── Алтарь тёмных сил ──────────────────────────────────────────────────────

  /** Что камень делает с положенной на него вещью. Камни-усилители он
    * переплавляет в черепа, трофеи поднимает обратно; всё прочее ему
    * безразлично — такую вещь герой уносит с собой. */
  private def offer(user: User, hero: Hero, scene: CaveScene, item: Item, renderer: Renderer): Task[StateType] =
    if (scene.altarSpent) say(user, scene, "altar.spent", renderer)
    else item.gem match {
      case Some(gem) if gem.kind == GemKind.Skull => say(user, scene, "altar.skullAlready", renderer)
      case Some(gem)                              => forgeSkull(user, hero, scene, item, gem.grade, renderer)
      case None => DarkAltar.formOf(item) match {
        case Some(form) => raise(user, hero, scene, item, form, renderer)
        case None       => say(user, scene, "altar.indifferent", renderer)
      }
    }

  /** Камень-усилитель уходит в камень алтаря и возвращается черепом того же
    * достоинства — тем самым, что выкапывают из свежих могил. */
  private def forgeSkull(user: User, hero: Hero, scene: CaveScene, item: Item, grade: Int, renderer: Renderer): Task[StateType] = {
    val skull = GemGenerator.item(GemKind.Skull, grade).copy(id = item.id)
    for {
      _   <- inventoryRepo.updateItem(hero.id, skull).mapError(asThrowable)
      _   <- renderer.show(user, Screen(content.format("cave.altar.skull",
               "gem" -> item.displayTitle, "skull" -> skull.displayTitle), Nil))
      // Череп стоит камню тех же сил, что и поднятый с камня, — алтарь гаснет.
      res <- burnOut(user, scene, renderer)
    } yield res
  }

  /** Трофей встаёт с камня тем, кем был при жизни. Мест в отряде нет — сперва
    * спросим, кем герой готов пожертвовать; трофей до ответа цел. */
  private def raise(user: User, hero: Hero, scene: CaveScene, trophy: Item, form: UndeadForm, renderer: Renderer): Task[StateType] =
    if (hero.squad.full) {
      val waiting = scene.copy(pending = Some(form), pendingTrophy = trophy.id)
      writeScene(user, waiting) *> askSwap(user, hero, form, renderer)
    } else
      for {
        now <- nowMs
        _ <- inventoryRepo.removeItem(trophy.id, hero.id).mapError(asThrowable)
        _ <- heroDao.updateSquad(user.userId, hero.squad.raise(form, hero.lvl, now))
        _ <- renderer.show(user, Screen(content.format("cave.altar.risen", "name" -> form.name), Nil))
        res <- burnOut(user, scene, renderer)
      } yield res

  /** Экран «кем жертвуем»: весь отряд кнопками и отказ. */
  private def askSwap(user: User, hero: Hero, form: UndeadForm, renderer: Renderer): Task[StateType] = {
    val buttons = hero.squad.inOrder.zipWithIndex.map { case (a, i) =>
      Choice("CaveSwap", Choice.fit(a.name), data = Map("pos" -> a.position.toString), row = Some(i / SwapPerRow))
    }
    val rows = (hero.squad.allies.size + SwapPerRow - 1) / SwapPerRow
    renderer.show(user, Screen(content.format("cave.altar.full", "name" -> form.name),
      buttons :+ content.choice("CaveSwapNo", "cave.altar.keepSquad")
        .copy(color = ChoiceColor.Negative, row = Some(rows)))).as(StateType.MonsterCave)
  }

  /** Выбран тот, кто уступит место поднятому. */
  private def swap(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      val pos = payloadField(ua, "pos").flatMap(_.toIntOption)
      (scene.pending, pos) match {
        case (Some(form), Some(p)) =>
          for {
            hero <- getHero(user)
            res <- hero.squad.allyAt(p) match {
              case None => showRoom(user, scene, renderer)
              case Some(old) =>
                for {
                  now <- nowMs
                  _ <- inventoryRepo.removeItem(scene.pendingTrophy, hero.id).mapError(asThrowable).ignore
                  _ <- heroDao.updateSquad(user.userId, hero.squad.replaceAt(p, form, hero.lvl, now))
                  _ <- renderer.show(user, Screen(content.format(key(scene, "altar.swapped"),
                         "old" -> old.name, "name" -> form.name), Nil))
                  out <- burnOut(user, scene.copy(pending = None, pendingTrophy = 0L), renderer)
                } yield out
            }
          } yield res
        case _ => showRoom(user, scene, renderer)
      }
    }

  /** Герой передумал жертвовать своими: трофей цел, алтарь ещё ждёт. */
  private def cancelSwap(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("cave.altar.kept"), Nil)) *>
      showRoom(user, scene.copy(pending = None, pendingTrophy = 0L), renderer)

  /** Алтарь отдал свою силу — больше он не отзовётся. */
  private def burnOut(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("cave.altar.spent"), Nil)) *>
      showRoom(user, scene.copy(altarSpent = true), renderer)

  /** Короткая реплика камня и обратно к сумке. */
  private def say(user: User, scene: CaveScene, name: String, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(txt(scene, name), Nil)) *>
      showSupplies(user, scene, renderer).as(StateType.MonsterCave)

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
          wall(scene).flatMap(line => renderer.show(user, Screen(line, Nil))) *> showRoom(user, scene, renderer)
        case Some(idx) =>
          val moved = scene.copy(at = idx)
          if (moved.room.monsters > 0) getHero(user).flatMap(fight(user, _, moved, idx, renderer))
          else writeScene(user, moved) *>
            renderer.show(user, Screen(content.text("cave.moved"), Nil)) *>
            showRoom(user, moved, renderer)
      }
    }

  /** Стена, в которую упёрся герой: пещера каждый раз показывает её по-своему. */
  private def wall(scene: CaveScene): Task[String] = {
    val walls = content.list(key(scene, "walls"))
    Random.nextIntBounded(walls.size).map(walls(_))
  }

  /** Экран комнаты: что здесь есть и куда отсюда ведут ходы. Направления видны
    * все четыре — зелёные там, где проход, красные там, где камень. */
  private def showRoom(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] = {
    val room = scene.room
    val text = roomText(scene)
    def dirChoice(id: String, key: String, dir: CaveDir, row: Int): Choice =
      content.choice(id, key).copy(
        color = if (scene.neighbour(dir).isDefined) ChoiceColor.Positive else ChoiceColor.Negative,
        row   = Some(row))
    // Пока в комнате есть кому драться, до находки дело не доходит.
    // Алтарь «обысканным» не становится: он гаснет, отдав силу (`altarSpent`).
    val spent  = room.kind == RoomKind.Altar && scene.altarSpent
    val action = Option.when(room.monsters <= 0 && !room.done && !spent && actionKey(room.kind).isDefined)(
      content.choice(actionId(room.kind), actionKey(room.kind).get).copy(row = Some(3)))
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

  /** Описание комнаты. Обысканная показывает, что от находки осталось, — сорванную
    * трещину, откинутую крышку, разрытые камни; пустая с самого начала берёт одно
    * из описаний [[emptyRoom]]. */
  private def roomText(scene: CaveScene): String = {
    val room = scene.room
    room.kind match {
      case RoomKind.Rest  => content.text(if (scene.restUsed) "cave.room.restUsed" else "cave.room.rest")
      case RoomKind.Altar => content.text(if (scene.altarSpent) "cave.room.altarSpent" else "cave.room.altar")
      case RoomKind.Herb  => content.text(if (room.done) "cave.room.herbTaken" else "cave.room.herb")
      case RoomKind.Chest => content.text(if (room.done) "cave.room.chestOpen" else "cave.room.chest")
      case RoomKind.Stash => content.text(if (room.done) "cave.room.stashDug" else "cave.room.stash")
      case RoomKind.Treasure => content.text(if (room.done) "cave.room.treasureDug" else "cave.room.treasure")
      case RoomKind.Stairs   => content.text("sewer.room.stairs")
      case RoomKind.Empty => emptyRoom(scene)
    }
  }

  /** Описание пустой комнаты. Вариант закреплён за местом, а не тянется наугад:
    * вернувшись, герой должен узнать комнату, в которой уже был. */
  private def emptyRoom(scene: CaveScene): String = {
    val room  = scene.room
    val lines = content.list(key(scene, "rooms.empty"))
    lines(math.floorMod(room.x * 31 + room.y * 17, lines.size))
  }

  private def actionKey(kind: RoomKind): Option[String] = kind match {
    case RoomKind.Herb  => Some("cave.act.herb")
    case RoomKind.Chest => Some("cave.act.chest")
    case RoomKind.Stash => Some("cave.act.stash")
    case RoomKind.Treasure => Some("cave.act.treasure")
    case RoomKind.Rest  => Some("cave.act.rest")
    case RoomKind.Altar => Some("cave.act.altar")
    case RoomKind.Stairs => Some("sewer.act.down")
    case RoomKind.Empty => None
  }

  /** Кнопка находки: у привала, алтаря и хода вниз свои маршруты, прочее обыскивают. */
  private def actionId(kind: RoomKind): String = kind match {
    case RoomKind.Rest   => "CaveRest"
    case RoomKind.Altar  => "CaveAltar"
    case RoomKind.Stairs => "CaveDown"
    case _               => "CaveSearch"
  }

  /** Герой стоит у алтаря — значит вещи из сумки идут не в пещеру, а на камень. */
  private def atAltar(scene: CaveScene): Boolean =
    scene.inside && scene.room.kind == RoomKind.Altar

  /** Полный ключ текста: у канализации свои слова там, где они у неё есть
    * (см. [[MonsterCaveState.SewerSays]]), в остальном она говорит языком
    * пещеры — механика у них одна, и дублировать её тексты незачем. */
  private def key(scene: CaveScene, name: String): String =
    if (scene.sewer && SewerSays.contains(name)) s"sewer.$name" else s"cave.$name"

  private def txt(scene: CaveScene, name: String): String = content.text(key(scene, name))

  /** Чья добыча в сундуке и схроне: в пещере — хозяев пещеры, в канализации —
    * людей, что прятали там своё. Крысы сундуков не набивают. */
  private def lootRace(scene: CaveScene): Race =
    if (scene.sewer) Race.Human else Race.withName(scene.race)

  // ── Бой в комнате ──────────────────────────────────────────────────────────

  /** Кучка мобов бросается на героя всей комнатой. Комнату помечаем зачищенной
    * заранее: вернуться из боя можно только победив, а павший герой теряет
    * пещеру целиком. */
  private def fight(user: User, hero: Hero, scene: CaveScene, idx: Int, renderer: Renderer): Task[StateType] = {
    val race  = Race.withName(scene.race)
    val count = scene.rooms(idx).monsters
    // В канализации уровень берётся от задания, а не от этажа лабиринта:
    // объявление обещало крыс именно такой силы (см. SewerRates.MinLvl..MaxLvl).
    val lvl   = if (scene.sewer) scene.questLvl.toInt.max(1) else hero.dungeonLevel
    for {
      seeds   <- ZIO.foreach(List.fill(count)(()))(_ => Random.nextLong)
      monsters = seeds.map { seed =>
                   val (rarity, _) =
                     if (scene.sewer) CaveGenerator.rollRatRarity(Rng(seed))
                     else CaveGenerator.rollRarity(Rng(seed))
                   weaken(MonsterGenerator.generateOfRaceAndRarity(lvl, race, rarity), scene.weakened)
                 }
      energies <- ZIO.foreach(monsters)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => cut(MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), scene.weakened)))
      battle    = poisonAll(SoloPveBattle.fromGroup(monsters, hero, energies), scene.poisoned)
      gained    = monsters.map(expFor).sum
      next      = scene.withRoom(idx, _.copy(monsters = 0)).copy(expEarned = scene.expEarned + gained)
      routing   = LootData(Nil, Nil, returnState = Some(StateType.MonsterCave), eventData = Some(next.asJson))
      _ <- heroDao.writeActiveBattle(user.userId, battle.copy(noKin = true).asJson)
      _ <- heroDao.writeSceneData(user.userId, routing.asJson)
      _ <- renderer.show(user, Screen(content.format(key(scene, "ambush"), "count" -> count.toString), Nil))
    } yield StateType.Battle
  }

  /** Ход вниз: нижний ярус катается в этот самый момент — до спуска его ещё
    * нет. Дурман и отрава идут вниз вместе с героем: сквозняк в трубах общий, а
    * взятый наверху опыт копится дальше — платят за канализацию один раз. */
  private def descend(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      if (!scene.sewer || scene.room.kind != RoomKind.Stairs || scene.room.monsters > 0)
        showRoom(user, scene, renderer)
      else
        for {
          seed      <- Random.nextLong
          (deep, _)  = CaveGenerator.sewerDeep(scene.questLvl, Rng(seed))
          below      = deep.copy(weakened = scene.weakened, poisoned = scene.poisoned,
                         expEarned = scene.expEarned, restUsed = true)
          _         <- renderer.show(user, Screen(content.text("sewer.descended"), Nil))
          res       <- showRoom(user, below, renderer)
        } yield res
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
        case RoomKind.Treasure => digTreasure(user, scene, renderer)
        case RoomKind.Stairs => descend(user, renderer)
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
                val (drops, _) = LootGenerator.roll(Rarity.Mythical, lootRace(scene),
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
      (reward, _) = SchronGenerator.roll(lootRace(scene), hero.dungeonLevel.toLong,
                      CaveRates.StashDoubloonMin, CaveRates.StashDoubloonMax, Rng(seed),
                      CaveRates.StashDoubloonChancePct)
      loot = LootData(
               items     = reward.items,
               silvers   = if (reward.silver > 0L) List(reward.silver) else Nil,
               doubloons = reward.doubloons)
      _ <- renderer.show(user, Screen(content.text("cave.stash"), Nil))
      _ <- handOver(user, scene, loot)
    } yield StateType.Loot

  /** Клад по карте: в пещере он тот же, что и в походе за город, — карта-то одна
    * и та же. Редкие травы в нём находит только знающий цветы 2 ранга. */
  private def digTreasure(user: User, scene: CaveScene, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      seed <- Random.nextLong
      lore <- HerbLore.readLore(heroDao, user.userId)
      zone  = scene.treasure.getOrElse(MapZone.forLevel(hero.lvl))
      (reward, _) = TreasureHuntGenerator.roll(zone, Rng(seed),
                      knowsRareHerbs = lore.knows(Knowledge.FlowersRank2))
      loot  = LootData(
                items     = reward.items ++ reward.gems ++ reward.materials,
                silvers   = if (reward.silver > 0L) List(reward.silver) else Nil,
                doubloons = reward.doubloons)
      _ <- renderer.show(user, Screen(content.format("cave.treasure", "name" -> zone.treasureName), Nil))
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

  /** Последний обитатель пал — пещера докладывает долю от опыта, что герой взял
    * со всех её мобов. Награда одна на пещеру. Из пещеры это героя не выводит:
    * необысканные углы остаются на месте, и уходит он сам, когда захочет. */
  private def reward(user: User, scene: CaveScene, renderer: Renderer): Task[CaveScene] =
    if (!scene.cleared || scene.rewarded || scene.expEarned <= 0L) ZIO.succeed(scene)
    // Верхний ярус канализации выбит, а большой крысы так и не видели: за
    // половину дела не платят — надо искать ход вниз.
    else if (!scene.lastFloor)
      renderer.show(user, Screen(content.text("sewer.upperClear"), Nil)).as(scene)
    else {
      val bonus = (scene.expEarned * CaveRates.ClearExpPct / 100L).max(1L)
      // Канализация в счёт пещер не идёт: за неё своё объявление.
      val quest = if (scene.sewer) BoardKind.SewerRats else BoardKind.CaveClear
      for {
        hero   <- getHero(user)
        leveled = hero.gainExp(bonus)
        _      <- heroDao.updateExpAndLevel(user.userId, leveled.exp, leveled.lvl, leveled.upgradePoints)
        _      <- renderer.show(user, Screen(content.format(key(scene, "cleared"), "exp" -> bonus.toString), Nil))
        _      <- ZIO.when(leveled.lvl > hero.lvl)(
                    renderer.show(user, Screen(s"Вы получили новый уровень ${leveled.lvl}!", Nil)))
        // Отдельной строкой, чтобы это не потерялось среди опыта и уровня.
        _      <- renderer.show(user, Screen(content.text(key(scene, "allClear")), Nil))
        closed <- BoardProgress.markDone(heroDao, user.userId, quest)
        _      <- ZIO.when(closed)(renderer.show(user, Screen(
                    content.text(if (scene.sewer) "questBoard.doneSewer" else "questBoard.doneCave"), Nil)))
        done    = scene.copy(rewarded = true)
        _      <- writeScene(user, done)
      } yield done
    }

  private def askLeave(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      if (!scene.inside) leave(user, renderer)
      else renderer.show(user, Screen(txt(scene, "confirmLeave.text"),
        content.screen("cave.confirmLeave").choices)).as(StateType.MonsterCave)
    }

  /** Уход. Из пещеры герой возвращается в лабиринт, из канализации — в город:
    * он и пришёл-то сюда из гильдии, по объявлению. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap { scene =>
      val sewer = scene.exists(_.sewer)
      scheduler.cancel(user.userId, TaskKind.CaveRest) *>
        heroDao.writeSceneData(user.userId, Json.Null) *>
        renderer.show(user, Screen(content.text(if (sewer) "sewer.left" else "cave.left"), Nil))
          .as(if (sewer) StateType.GlobalMap else StateType.Dungeon)
    }

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

  private def payloadField(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object MonsterCaveState {
  /** Префикс кнопки «пустить эту вещь в дело» перед входом. */
  val UsePrefix: String = "CaveUse_"

  /** Ключи, на которые у канализации свои слова (`sewer.<имя>`); на всё
    * остальное она отвечает словами пещеры (`cave.<имя>`). Список держим
    * здесь, одним местом: по нему же тесты проверяют, что тексты на месте. */
  val SewerSays: Set[String] = Set(
    "gate.text", "gate.enter", "entered", "left", "confirmLeave.text",
    "walls", "rooms.empty", "ambush", "cleared", "allClear",
    "supply.dope", "supply.already", "supply.wasted",
    "supply.map", "supply.mapAlready", "supply.mapHalf",
    "altar.swapped"
  )

  /** Сколько своих помещается в ряд на экране «кем жертвуем». */
  val SwapPerRow: Int = 2

  /** payload синтетического действия, которым поллер будит героя после привала. */
  val RestAction: String = """{"action":"CaveRested"}"""

  /** Опыт за одного моба пещеры — та же формула, что и в бою: этаж на редкость. */
  def expFor(m: Monster): Long = (m.lvl.toDouble * m.rarity.factor).toLong.max(1L)
}
