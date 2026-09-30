package pangea.service.state.states.events.caravan

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{MonsterSlot, SoloPveBattle}
import pangea.model.caravan.{CaravanGenerator, CaravanRates, CaravanScene}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails}
import pangea.model.monster.{Monster, Race}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.events.cave.{CaveBoon, CaveSupply}
import pangea.service.state.states.events.caravan.CaravanState._
import pangea.service.state.{CharacterMenu, ItemMenu, State, UserAction}
import zio.{Random, Task, ZIO}

/** Караван (1% в лабиринте): десять-двадцать охранников, пара башен со
  * стрелками и три хороших вещи в поклаже.
  *
  * Подходить к нему можно тремя шагами — заметил издали, подобрался вплотную,
  * выждал удобный момент, — и на каждом решать заново: напасть, пустить в дело
  * вещь из сумки или сделать ещё шаг. Чем дольше герой выжидает, тем больше у
  * него выборов: на последнем шаге открываются «Напугать» (тем, кто ужасает) и
  * «Скрытно» (тем, кто умеет красться).
  *
  * Вещи работают так же, как у входа в пещеру: сонный дурман и дымная фляга
  * одурманивают охрану, а божественное оружие пугает её — часть каравана
  * разбегается ещё до боя. Пугает и пассивка «Ужасающий»; вместе они уводят
  * половину. Откуда пассивка взялась — с вещи или выжжена руной — неважно. */
case class CaravanState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "CaravanSupply" -> Target.Run { (u, _, r) => withScene(u)(s => showSupplies(u, s, r).as(StateType.Caravan)) },
      "CaravanBack"   -> Target.Run { (u, _, r) => withScene(u)(s => show(u, s, r)) },
      "CaravanPrev"   -> Target.Run { (u, _, r) => turnPage(u, r, -1) },
      "CaravanNext"   -> Target.Run { (u, _, r) => turnPage(u, r, +1) },
      "CaravanAttack" -> Target.Run { (u, _, r) => withScene(u)(s => attack(u, s, r)) },
      "CaravanSneakUp" -> Target.Run { (u, _, r) => withScene(u)(s => step(u, s, CaravanRates.StageClose, r)) },
      "CaravanWait"   -> Target.Run { (u, _, r) => withScene(u)(s => step(u, s, CaravanRates.StageMoment, r)) },
      "CaravanScare"  -> Target.Run { (u, _, r) => withScene(u)(s => scare(u, s, r)) },
      "CaravanSneak"  -> Target.Run { (u, _, r) => withScene(u)(s => sneak(u, s, r)) },
      "CaravanTrade"  -> Target.Run { (u, _, r) => withScene(u)(s => trade(u, s, r)) },
      "CaravanLeave"  -> Target.Run { (u, _, r) => leave(u, r) },
      "CaravanSpoils" -> Target.Run { (u, _, r) => withScene(u)(s => takeSpoils(u, s, r)) },
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.Caravan) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.Loot, StateType.HeroStats, StateType.Caravan)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    readScene(user).flatMap {
      // Охрана перебита — остаётся разобрать повозки.
      case Some(scene) if scene.spoils => showSpoils(user, renderer)
      case Some(scene)                 => show(user, scene, renderer).unit
      case None                        => discover(user, renderer)
    }

  /** Бой кончился, обоз стоит без охраны: поклажу забирают одной кнопкой. */
  private def showSpoils(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("caravan.spoils.text"), List(
      content.choice("CaravanSpoils", "caravan.spoils.take")
        .copy(color = ChoiceColor.Positive, row = Some(0)))))

  /** Поклажа уходит на экран добычи — тем же путём, каким её отдавали бы за
    * тихую кражу. Сцена на этом кончается. */
  private def takeSpoils(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("caravan.spoils.taken"), Nil)) *>
      heroDao.writeSceneData(user.userId, LootData(items = scene.goods, silvers = Nil).asJson)
        .as(StateType.Loot)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Встреча ───────────────────────────────────────────────────────────────

  /** Караван попался на глаза: катаем охрану, башни и поклажу. Товар у него по
    * герою, а охрана — по этажу: люди в лабиринте местные. */
  private def discover(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      seed  <- Random.nextLong
      (base, r1) = CaravanGenerator.generate(Rng(seed))
      (goods, prices) = rollGoods(hero.lvl, r1)
      scene  = base.copy(goods = goods, prices = prices)
      _     <- writeScene(user, scene)
      _     <- show(user, scene, renderer)
    } yield ()

  private def rollGoods(heroLvl: Long, rng: Rng): (List[Item], List[Long]) =
    (0 until CaravanRates.Goods).foldLeft((List.empty[Item], List.empty[Long], rng)) {
      case ((items, prices, r), _) =>
        val (rarity, r1) = CaravanGenerator.goodsRarity(r)
        val (item, r2)   = ItemGenerator.createItemAtLevel(heroLvl, rarity, r1)
        val (price, r3)  = CaravanGenerator.price(heroLvl, rarity, r2)
        (items :+ item, prices :+ price, r3)
    } match { case (items, prices, _) => (items, prices) }

  // ── Экраны подхода ────────────────────────────────────────────────────────

  private def show(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      race  = Race.withName(scene.race)
      text  = content.format(s"caravan.stage${scene.stage}", "race" -> race.genitivePlural) + effectsLine(scene)
      keys  = List(
        Some(content.choice("CaravanSupply", "caravan.useItem").copy(row = Some(0))),
        Some(content.choice("CaravanAttack", "caravan.attack").copy(color = ChoiceColor.Negative, row = Some(1))),
        Option.when(scene.stage == CaravanRates.StageSpotted)(
          content.choice("CaravanSneakUp", "caravan.sneakUp").copy(row = Some(2))),
        Option.when(scene.stage == CaravanRates.StageSpotted)(
          content.choice("CaravanTrade", "caravan.trade").copy(color = ChoiceColor.Positive, row = Some(2))),
        Option.when(scene.stage == CaravanRates.StageClose)(
          content.choice("CaravanWait", "caravan.wait").copy(row = Some(2))),
        Option.when(scene.stage == CaravanRates.StageMoment && hero.passives.hasTerrifying)(
          content.choice("CaravanScare", "caravan.scare").copy(row = Some(2))),
        Option.when(scene.stage == CaravanRates.StageMoment && hero.passives.hasStealthy)(
          content.choice("CaravanSneak", "caravan.sneak").copy(row = Some(2))),
        Some(content.choice("OpenCharacter", "common.character").copy(row = Some(3))),
        Some(content.choice("CaravanLeave", "caravan.leave").copy(color = ChoiceColor.Negative, row = Some(3)))
      ).flatten
      _ <- renderer.show(user, Screen(text, keys))
    } yield StateType.Caravan

  /** Что герой уже успел сделать с караваном — строкой под описанием. */
  private def effectsLine(scene: CaravanScene): String = {
    val parts = List(
      Option.when(scene.weakened)(content.text("caravan.markDoped")),
      Option.when(scene.scared > 0)(content.format("caravan.markScared", "n" -> scene.scared.toString))
    ).flatten
    if (parts.isEmpty) "" else "\n\n" + parts.mkString("\n")
  }

  private def step(user: User, scene: CaravanScene, stage: Int, renderer: Renderer): Task[StateType] = {
    val next = scene.copy(stage = stage)
    writeScene(user, next) *>
      renderer.show(user, Screen(content.text(s"caravan.moveTo$stage"), Nil)) *>
      show(user, next, renderer)
  }

  // ── Вещи и страх ──────────────────────────────────────────────────────────

  /** Список вещей — тот же, что у входа в пещеру: сумка и надетая фляга. */
  private def supplies(hero: Hero, items: List[Item]): List[Item] = {
    val flask = hero.equipment.flask
    val worn  = Option.when(flask.id > 0L && CaveSupply.charged(flask).isDefined)(flask)
    val blade = hero.equipment.additionalWeapon
    val divine = Option.when(blade.id > 0L && blade.divine.isDefined)(blade)
    items ++ worn.toList ++ divine.toList
  }

  private def showSupplies(user: User, scene: CaravanScene, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      all   = supplies(hero, inv.items.data)
      _ <- if (all.isEmpty) renderer.show(user, Screen(content.text("caravan.supply.empty"), List(backButton)))
           else {
             val (pageItems, pages, p) = ItemMenu.page(all, scene.page)
             val buttons = pageItems.zipWithIndex.map { case (item, i) =>
               Choice(s"$UsePrefix${item.id}", ItemMenu.itemButtonLabel(item), row = Some(i))
             }
             val nav = List(
               Some(backButton),
               Option.when(p > 0)(Choice("CaravanPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
               Option.when(p < pages - 1)(Choice("CaravanNext", content.text("common.next"), row = Some(ItemMenu.NavRow)))
             ).flatten
             renderer.show(user, Screen(content.format("caravan.supply.pick",
               "page" -> (p + 1).toString, "total" -> pages.toString), buttons ++ nav))
           }
    } yield ()

  private def backButton: Choice =
    content.choice("CaravanBack", "caravan.supply.back").copy(color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))

  private def turnPage(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    withScene(user) { scene =>
      val turned = scene.copy(page = (scene.page + delta).max(0))
      writeScene(user, turned) *> showSupplies(user, turned, renderer).as(StateType.Caravan)
    }

  /** Пустить вещь в дело. Дурман и дым усыпляют охрану, божественное оружие её
    * пугает — как и «Ужасающий», только клинком. Прочее каравану безразлично. */
  private def useItem(user: User, scene: CaravanScene, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      item  = supplies(hero, inv.items.data).find(_.id == itemId)
      res <- item match {
        case None => showSupplies(user, scene, renderer).as(StateType.Caravan)
        case Some(it) if it.isQuestItem =>
          say(user, scene, content.format("caravan.supply.quest", "name" -> it.displayTitle), renderer)
        case Some(it) if it.divine.isDefined => frighten(user, hero, scene, it, renderer)
        case Some(it) => dope(user, hero, scene, it, renderer)
      }
    } yield res

  /** Божественное оружие: одного его вида хватает, чтобы часть охраны
    * растворилась в темноте. Стоит это одного удара из тех, что в нём остались. */
  private def frighten(user: User, hero: Hero, scene: CaravanScene, blade: Item, renderer: Renderer): Task[StateType] =
    blade.divine match {
      case Some(d) if d.charges <= 0 =>
        say(user, scene, content.format("caravan.supply.spent", "name" -> blade.displayTitle), renderer)
      case Some(_) if scene.scared >= MaxScares =>
        say(user, scene, content.text("caravan.supply.alreadyScared"), renderer)
      case Some(d) =>
        val next = scene.copy(scared = scene.scared + 1)
        heroDao.updateEquipment(user.userId,
          hero.equipment.copy(additionalWeapon = blade.copy(details = d.spent))) *>
          writeScene(user, next) *>
          renderer.show(user, Screen(content.format("caravan.supply.divine", "name" -> blade.displayTitle), Nil)) *>
          show(user, next, renderer)
      case None => showSupplies(user, scene, renderer).as(StateType.Caravan)
    }

  /** Дурман и дым: охрана вялая, а караван приходится брать двумя заходами —
    * половина успевает опомниться, пока герой возится с первой. */
  private def dope(user: User, hero: Hero, scene: CaravanScene, item: Item, renderer: Renderer): Task[StateType] = {
    val boon    = CaveSupply.boonOf(item)
    val charged = CaveSupply.charged(item)
    val smoke   = item.details match {
      case ItemDetails.Flask(pangea.model.item.FlaskEffect.Smoke(_), _, _) => true
      case _                                                              => false
    }
    if (boon != CaveBoon.Dope)
      say(user, scene, content.format("caravan.supply.wasted", "name" -> item.displayTitle), renderer)
    else if (charged.exists(_.charges <= 0))
      say(user, scene, content.format("caravan.supply.spent", "name" -> item.displayTitle), renderer)
    else {
      val next = scene.copy(weakened = true, smoke = scene.smoke || smoke)
      for {
        _ <- takeFrom(user, hero, item, charged)
        _ <- writeScene(user, next)
        _ <- renderer.show(user, Screen(content.format(
               if (smoke) "caravan.supply.smoke" else "caravan.supply.dope",
               "name" -> item.displayTitle, "pct" -> CaravanRates.DopeCutPct.toString), Nil))
        res <- show(user, next, renderer)
      } yield res
    }
  }

  private def takeFrom(user: User, hero: Hero, item: Item, charged: Option[ItemDetails.Charged]): Task[Unit] =
    charged match {
      case Some(c) if hero.equipment.flask.id == item.id =>
        heroDao.updateEquipment(user.userId, hero.equipment.copy(flask = item.copy(details = c.spent)))
      case Some(c) => inventoryRepo.updateItem(hero.id, item.copy(details = c.spent)).mapError(asThrowable).unit
      case None    => inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable).unit
    }

  /** «Ужасающий»: герой выпрямляется во весь рост, и часть охраны решает, что
    * плата за этот караван слишком высока. */
  private def scare(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    if (scene.scared >= MaxScares) say(user, scene, content.text("caravan.supply.alreadyScared"), renderer)
    else {
      val next = scene.copy(scared = scene.scared + 1)
      writeScene(user, next) *>
        renderer.show(user, Screen(content.text("caravan.scared"), Nil)) *>
        show(user, next, renderer)
    }

  private def say(user: User, scene: CaravanScene, text: String, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(text, Nil)) *> showSupplies(user, scene, renderer).as(StateType.Caravan)

  // ── Торговля ──────────────────────────────────────────────────────────────

  private def trade(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      lines   = scene.goods.zip(scene.prices).zipWithIndex.map { case ((item, price), i) =>
                  s"${i + 1}) ${item.displayTitle}\n${item.statsLines.mkString("\n")}\n🪙 Цена: $price"
                }
      buttons = scene.goods.zipWithIndex.map { case (_, i) =>
                  Choice(s"$BuyPrefix$i", content.format("caravan.buyLabel", "n" -> (i + 1).toString), row = Some(i))
                }
      _ <- renderer.show(user, Screen(
             content.format("caravan.trade.text", "silver" -> hero.silver.toString) + "\n\n" + lines.mkString("\n\n"),
             buttons :+ content.choice("CaravanBack", "caravan.trade.back")
               .copy(color = ChoiceColor.Negative, row = Some(scene.goods.size))))
    } yield StateType.Caravan

  private def buy(user: User, scene: CaravanScene, idx: Int, renderer: Renderer): Task[StateType] =
    (scene.goods.lift(idx), scene.prices.lift(idx)) match {
      case (Some(item), Some(price)) =>
        for {
          hero   <- getHero(user)
          // Караван берёт только то, что при герое: до банковской ячейки из
          // лабиринта не дойти, и торговцу до неё дела нет.
          res <- if (hero.silver < price)
                   renderer.show(user, Screen(content.text("caravan.trade.poor"), Nil)) *> trade(user, scene, renderer)
                 else
                   for {
                     _         <- heroDao.updateSilver(user.userId, hero.silver - price)
                     persisted <- itemRepo.persist(hero.id, item)
                     added     <- inventoryRepo.addItem(hero.id, persisted).either
                     _         <- renderer.show(user, Screen(
                                    if (added.isRight) content.format("caravan.trade.bought", "name" -> persisted.displayTitle)
                                    else content.text("common.inventoryFull"), Nil))
                     left       = scene.copy(
                                    goods  = scene.goods.patch(idx, Nil, 1),
                                    prices = scene.prices.patch(idx, Nil, 1))
                     _         <- writeScene(user, left)
                     out       <- if (left.goods.isEmpty) show(user, left, renderer) else trade(user, left, renderer)
                   } yield out
        } yield res
      case _ => trade(user, scene, renderer)
    }

  // ── Кража ─────────────────────────────────────────────────────────────────

  /** Скрытность: без дыма подобраться к поклаже не выйдет — караван видит
    * слишком хорошо. С дымом — половина на половину, и столько же на то, чтобы
    * уйти с добычей незамеченным. */
  private def sneak(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    if (!scene.smoke)
      renderer.show(user, Screen(content.text("caravan.sneak.noSmoke"), Nil)) *> attack(user, scene, renderer)
    else
      Random.nextIntBetween(1, 101).flatMap { roll =>
        if (roll > CaravanRates.SneakPct)
          renderer.show(user, Screen(content.text("caravan.sneak.caught"), Nil)) *> attack(user, scene, renderer)
        else
          for {
            _   <- renderer.show(user, Screen(content.text("caravan.sneak.took"), Nil))
            out <- Random.nextIntBetween(1, 101).flatMap { away =>
                     if (away <= CaravanRates.SlipPct) slipAway(user, scene, renderer)
                     else renderer.show(user, Screen(content.text("caravan.sneak.spotted"), Nil)) *>
                            attack(user, scene, renderer)
                   }
          } yield out
      }

  /** Ушёл с поклажей и без боя: вещи забирает экран добычи, караван на этом
    * кончается. */
  private def slipAway(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("caravan.sneak.away"), Nil)) *>
      heroDao.writeSceneData(user.userId, LootData(items = scene.goods, silvers = Nil).asJson)
        .as(StateType.Loot)

  // ── Бой ───────────────────────────────────────────────────────────────────

  /** Напасть. Одурманенный караван приходится брать двумя волнами; в остальном
    * это обычный групповой бой, только с башнями в хвосте строя. Поклажу
    * отдают после последней волны. */
  private def attack(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    startWave(user, scene.copy(wave = scene.wave + 1), renderer)

  private def startWave(user: User, scene: CaravanScene, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      race   = Race.withName(scene.race)
      count  = scene.waveSize(scene.wave)
      lvl    = hero.dungeonLevel
      seeds <- ZIO.foreach(List.fill(count)(()))(_ => Random.nextLong)
      guards = seeds.map { seed =>
                 val (rarity, _) = CaravanGenerator.guardRarity(Rng(seed))
                 weaken(MonsterGenerator.generateOfRaceAndRarity(lvl, race, rarity), scene.weakened)
               }
      towers = List.fill(scene.towersInFight)(MonsterGenerator.tower(lvl))
      energies <- ZIO.foreach(guards ++ towers)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      battle  = withTowers(SoloPveBattle.fromGroup(guards ++ towers, hero, energies), guards.size, towers.size)
      last    = scene.wave >= scene.waves
      // Добычу с мобов экран добычи соберёт сам, а поклажу каравана он не
      // знает — за ней герой возвращается сюда же, к разбитому обозу.
      routing = LootData(Nil, Nil, returnState = Some(StateType.Caravan),
                  eventData = Some(scene.copy(stage = CaravanRates.StageMoment, spoils = last).asJson))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, routing.asJson)
      _ <- renderer.show(user, Screen(content.format(
             if (scene.waves > 1) "caravan.battleWave" else "caravan.battle",
             "count" -> count.toString, "wave" -> scene.wave.toString, "of" -> scene.waves.toString), Nil))
    } yield StateType.Battle

  /** Башни встают в хвост строя — с десятого места и ниже — и с него не сходят. */
  private def withTowers(battle: SoloPveBattle, guards: Int, towers: Int): SoloPveBattle =
    if (towers <= 0) battle
    else {
      val places = battle.group.places.zipWithIndex.map { case (p, i) =>
        // Башни идут в списке последними: их места переносим в хвост строя.
        val towerIdx = i - (guards - 1)
        if (towerIdx >= 0) CaravanRates.TowerPlace + towerIdx else p
      }
      battle.copy(group = battle.group.copy(places = places))
    }

  private def weaken(m: Monster, on: Boolean): Monster =
    if (!on) m
    else m.copy(fightStats = m.fightStats.copy(
      atk    = (m.fightStats.atk * (100L - CaravanRates.DopeCutPct) / 100L).max(1L),
      energy = m.fightStats.energy * (100L - CaravanRates.DopeCutPct) / 100L))

  // ── Уход и вспомогательное ────────────────────────────────────────────────

  private def leave(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("caravan.left"), Nil)).as(StateType.Dungeon)

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(UsePrefix) =>
        withScene(user) { scene =>
          a.drop(UsePrefix.length).toLongOption
            .fold(showSupplies(user, scene, renderer).as(StateType.Caravan: StateType))(
              useItem(user, scene, _, renderer))
        }
      case Some(a) if a.startsWith(BuyPrefix) =>
        withScene(user) { scene =>
          a.drop(BuyPrefix.length).toIntOption
            .fold(trade(user, scene, renderer))(buy(user, scene, _, renderer))
        }
      case _ => withScene(user)(s => show(user, s, renderer))
    }

  private def withScene(user: User)(f: CaravanScene => Task[StateType]): Task[StateType] =
    readScene(user).flatMap {
      case Some(scene) => f(scene)
      case None        => ZIO.succeed(StateType.Dungeon)
    }

  private def readScene(user: User): Task[Option[CaravanScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[CaravanScene].toOption))

  private def writeScene(user: User, scene: CaravanScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object CaravanState {
  val UsePrefix: String = "CaravanUse_"
  val BuyPrefix: String = "CaravanBuy_"

  /** Дважды напугать один караван нельзя: клинок и рык уводят по четверти, и
    * вместе это уже половина. */
  val MaxScares: Int = 2

  /** Слоты мобов волны — для тестов и сборки боя. */
  def slotOf(m: Monster, energy: Long): MonsterSlot =
    MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
      m.fightStats.armor, m.marked, energy, pangea.model.battle.BattleEffects.empty)

  /** Есть ли у героя нужная пассивка — неважно, с вещи она или выжжена руной. */
  def terrifies(hero: Hero): Boolean = hero.passives.hasTerrifying
  def sneaks(hero: Hero): Boolean    = hero.passives.hasStealthy
}
