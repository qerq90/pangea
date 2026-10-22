package pangea.service.state.states.home

import io.circe.jawn
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.CubeCraft
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.{Achievement, Hero}
import pangea.model.home.{HomeData, HomeRates, House}
import pangea.model.item.{BrewKind, Item, ItemStack}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{HerbLore, ItemMenu, MarisaQuest, State, UserAction}
import zio.{Random, Task, ZIO}

/** Верстак травника — со второго дома. Полки под травы (всего
  * [[HomeRates.HerbShelves]], не по видам), котелок на [[HomeRates.CauldronSlots]]
  * мест и полка готовых отваров на [[HomeRates.BrewShelves]].
  *
  * Травы ложатся на полки одной кнопкой — из сумки и из Живой сумки, пока есть
  * место. Траву с полки можно бросить в котелок или забрать в сумку. Котелок
  * варит, как магия Живой сумки (`CubeCraft.brewHerbs`: отвары из трав и
  * перегонка склянки с травами), и тратит энергию дома — по одной на каждую
  * варку. Сваренное встаёт на полку отваров, что не поместилось — ждёт в
  * котелке. На полке отваров — то же: сложить все из сумки и Живой сумки,
  * забрать в сумку, бросить в котелок основой для перегонки. Всё живёт в
  * `home_data`. */
case class HerbWorkbenchState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "WbMenu"      -> Target.Run { (u, ua, r) => menu(u, page(ua), r) },
      "WbHerb"      -> Target.Run { (u, ua, r) => herb(u, ua, r) },
      "WbHerbPot"   -> Target.Run { (u, ua, r) => herbToPot(u, ua, r) },
      "WbHerbBag"   -> Target.Run { (u, ua, r) => herbToBag(u, ua, r) },
      "WbPutHerbs"  -> Target.Run { (u, _, r) => putHerbs(u, r) },
      "WbPot"       -> Target.Run { (u, _, r) => pot(u, r) },
      "WbCook"      -> Target.Run { (u, _, r) => cook(u, r) },
      "WbEmptyPot"  -> Target.Run { (u, _, r) => emptyPot(u, r) },
      "WbBrews"     -> Target.Run { (u, ua, r) => brews(u, page(ua), r) },
      "WbBrew"      -> Target.Run { (u, ua, r) => brew(u, ua, r) },
      "WbBrewBag"   -> Target.Run { (u, ua, r) => brewToBag(u, ua, r) },
      "WbBrewPot"   -> Target.Run { (u, ua, r) => brewToPot(u, ua, r) },
      "WbPutBrews"  -> Target.Run { (u, _, r) => putBrews(u, r) },
      "LeaveWb"     -> Target.Goto(StateType.Home)
    ),
    fallback = Target.Run { (u, _, r) => menu(u, 0, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = menu(user, 0, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Полки трав ──────────────────────────────────────────────────────────────

  /** Верстак: что на полках, сколько в котелке, энергия дома; травы — стопками
    * по страницам. */
  private def menu(user: User, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val text = content.format("home.workbench.text",
        "herbs" -> home.herbs.size.toString, "herbsMax" -> HomeRates.HerbShelves.toString,
        "pot" -> home.cauldron.size.toString, "potMax" -> HomeRates.CauldronSlots.toString,
        "brewsMax" -> HomeRates.BrewShelves.toString,
        "charges" -> home.charges.toString, "chargesMax" -> HomeRates.AltarMaxCharges.toString)
      val tail = List(
        Choice("WbPutHerbs", content.text("home.workbench.putHerbs"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
        Choice("WbPot", content.text("home.workbench.pot"), row = Some(ItemMenu.NavRow + 1)),
        Choice("WbBrews", content.text("home.workbench.brews"), row = Some(ItemMenu.NavRow + 1)),
        Choice("LeaveWb", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))
      )
      renderer.show(user, Screen(text, stacks(home.herbs, p, "WbHerb", "WbMenu") ++ tail)).as(StateType.HerbWorkbench)
    }

  private def herb(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.herbs, ua) match {
        case None => menu(user, page(ua), renderer)
        case Some(it) =>
          val data = Map("id" -> it.id.toString, "p" -> page(ua).toString)
          renderer.show(user, Screen(content.format("home.workbench.herb", "name" -> it.name, "n" -> same(home.herbs, it).toString), List(
            Choice("WbHerbPot", content.text("home.workbench.toPot"), color = ChoiceColor.Positive, data = data, row = Some(0)),
            Choice("WbHerbBag", content.text("home.workbench.toBag"), data = data, row = Some(0)),
            Choice("WbMenu", content.text("home.house.back"), color = ChoiceColor.Negative, data = Map("p" -> page(ua).toString), row = Some(1))
          ))).as(StateType.HerbWorkbench)
      }
    }

  private def herbToPot(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.herbs, ua) match {
        case None => menu(user, page(ua), renderer)
        case Some(_) if home.cauldron.size >= HomeRates.CauldronSlots =>
          say(user, content.format("home.workbench.potFull", "max" -> HomeRates.CauldronSlots.toString), renderer) *> herb(user, ua, renderer)
        case Some(it) =>
          saveHome(user, home.copy(herbs = without(home.herbs, it), cauldron = home.cauldron :+ it)) *>
            say(user, content.format("home.workbench.potIn", "name" -> it.name), renderer) *>
            afterTake(user, ua, home.herbs, it, renderer, herb, menu)
      }
    }

  private def herbToBag(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.herbs, ua) match {
        case None => menu(user, page(ua), renderer)
        case Some(it) =>
          toBag(user, it).flatMap {
            case false => say(user, content.text("home.workbench.bagFull"), renderer) *> herb(user, ua, renderer)
            case true =>
              saveHome(user, home.copy(herbs = without(home.herbs, it))) *>
                say(user, content.format("home.workbench.toBagDone", "name" -> it.name), renderer) *>
                afterTake(user, ua, home.herbs, it, renderer, herb, menu)
          }
      }
    }

  /** Все травы — из сумки и из Живой сумки — на полки, пока есть место. */
  private def putHerbs(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      gather(user, isHerb, HomeRates.HerbShelves - home.herbs.size).flatMap {
        case (Nil, 0) => say(user, content.text("home.workbench.noHerbs"), renderer) *> menu(user, 0, renderer)
        case (taken, left) =>
          saveHome(user, home.copy(herbs = home.herbs ++ taken)) *>
            say(user, content.format("home.workbench.herbsPut", "n" -> taken.size.toString) +
              (if (left > 0) "\n" + content.format("home.workbench.herbsLeft", "n" -> left.toString) else ""), renderer) *>
            menu(user, 0, renderer)
      }
    }

  // ── Котелок ─────────────────────────────────────────────────────────────────

  private def pot(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val lines = ItemStack.grouped(home.cauldron).map { case (it, n) => s"• ${it.name}${ItemStack.countSuffix(n)}" }
      val text  =
        if (home.cauldron.isEmpty) content.text("home.workbench.potEmpty")
        else content.format("home.workbench.potList", "n" -> home.cauldron.size.toString,
          "max" -> HomeRates.CauldronSlots.toString, "lines" -> lines.mkString("\n"))
      renderer.show(user, Screen(text + "\n" + content.format("home.workbench.energy",
        "n" -> home.charges.toString, "max" -> HomeRates.AltarMaxCharges.toString), List(
        Choice("WbCook", content.text("home.workbench.cook"), color = ChoiceColor.Positive, row = Some(0)),
        Choice("WbEmptyPot", content.text("home.workbench.emptyPot"), row = Some(0)),
        Choice("WbMenu", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(1))
      ))).as(StateType.HerbWorkbench)
    }

  /** Сварить: каждая варка — одна энергия дома. Сваренное — на полку отваров,
    * что не поместилось — остаётся в котелке. */
  private def cook(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      if (home.charges <= 0) say(user, content.text("home.workbench.noCharges"), renderer) *> pot(user, renderer)
      else for {
        hero   <- getHero(user)
        seed   <- Random.nextLong
        result  = CubeCraft.brewHerbs(home.cauldron, home.charges, Rng(seed))
        res    <- if (!result.anyApplied) say(user, content.text("home.workbench.nothing"), renderer) *> pot(user, renderer)
                  else {
                    val (fresh, kept) = result.items.partition(_.id <= 0L)
                    for {
                      made  <- ZIO.foreach(fresh)(itemRepo.persist(hero.id, _))
                      room   = (HomeRates.BrewShelves - home.brews.size).max(0)
                      (shelf, queued) = made.splitAt(room)
                      _     <- saveHome(user, home.copy(brews = home.brews ++ shelf, cauldron = kept ++ queued,
                                 charges = home.charges - result.chargesUsed))
                      _     <- say(user, content.format("home.workbench.cooked", "n" -> result.chargesUsed.toString,
                                 "bottles" -> made.size.toString) +
                                 (if (queued.nonEmpty) "\n" + content.format("home.workbench.cookedWait", "n" -> queued.size.toString) else ""),
                                 renderer)
                      _     <- brewedAchievement(user, hero, made, renderer)
                      s     <- pot(user, renderer)
                    } yield s
                  }
      } yield res
    }

  /** Вынуть всё из котелка: травы — на полки, отвары — на полку отваров, а
    * где места нет — в сумку; что и туда не влезло, остаётся в котелке. */
  private def emptyPot(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      if (home.cauldron.isEmpty) pot(user, renderer)
      else for {
        hero <- getHero(user)
        inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
        (hShelf, bShelf, bag, stay) = {
          val hRoom = HomeRates.HerbShelves - home.herbs.size
          val bRoom = HomeRates.BrewShelves - home.brews.size
          val (hs, rest1) = home.cauldron.partition(isHerb)
          val (bs, other) = rest1.partition(_.brew.isDefined)
          val (hIn, hOver) = hs.splitAt(hRoom.max(0))
          val (bIn, bOver) = bs.splitAt(bRoom.max(0))
          val over = hOver ++ bOver ++ other
          val (toBag, stayPot) = over.splitAt(inv.freeSlots.toInt.max(0))
          (hIn, bIn, toBag, stayPot)
        }
        _ <- ZIO.foreachDiscard(bag)(it => inventoryRepo.addItem(hero.id, it).mapError(asThrowable))
        _ <- saveHome(user, home.copy(herbs = home.herbs ++ hShelf, brews = home.brews ++ bShelf, cauldron = stay))
        _ <- say(user, content.text("home.workbench.emptied") +
               (if (stay.nonEmpty) "\n" + content.format("home.workbench.emptiedStay", "n" -> stay.size.toString) else ""), renderer)
        s <- pot(user, renderer)
      } yield s
    }

  // ── Полка отваров ───────────────────────────────────────────────────────────

  private def brews(user: User, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val text =
        if (home.brews.isEmpty) content.text("home.workbench.brewsEmpty")
        else content.format("home.workbench.brewsText", "n" -> home.brews.size.toString, "max" -> HomeRates.BrewShelves.toString)
      val tail = List(
        Choice("WbPutBrews", content.text("home.workbench.putBrews"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
        Choice("WbMenu", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))
      )
      renderer.show(user, Screen(text, stacks(home.brews, p, "WbBrew", "WbBrews") ++ tail)).as(StateType.HerbWorkbench)
    }

  private def brew(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.brews, ua) match {
        case None => brews(user, page(ua), renderer)
        case Some(it) =>
          val data = Map("id" -> it.id.toString, "p" -> page(ua).toString)
          renderer.show(user, Screen(content.format("home.workbench.brewItem", "name" -> it.name, "n" -> same(home.brews, it).toString), List(
            Choice("WbBrewBag", content.text("home.workbench.toBag"), color = ChoiceColor.Positive, data = data, row = Some(0)),
            Choice("WbBrewPot", content.text("home.workbench.toPot"), data = data, row = Some(0)),
            Choice("WbBrews", content.text("home.house.back"), color = ChoiceColor.Negative, data = Map("p" -> page(ua).toString), row = Some(1))
          ))).as(StateType.HerbWorkbench)
      }
    }

  private def brewToBag(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.brews, ua) match {
        case None => brews(user, page(ua), renderer)
        case Some(it) =>
          toBag(user, it).flatMap {
            case false => say(user, content.text("home.workbench.bagFull"), renderer) *> brew(user, ua, renderer)
            case true =>
              saveHome(user, home.copy(brews = without(home.brews, it))) *>
                say(user, content.format("home.workbench.toBagDone", "name" -> it.name), renderer) *>
                afterTake(user, ua, home.brews, it, renderer, brew, brews)
          }
      }
    }

  /** Склянку — основой в котелок: из неё и трав выходит перегонка. */
  private def brewToPot(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.brews, ua) match {
        case None => brews(user, page(ua), renderer)
        case Some(_) if home.cauldron.size >= HomeRates.CauldronSlots =>
          say(user, content.format("home.workbench.potFull", "max" -> HomeRates.CauldronSlots.toString), renderer) *> brew(user, ua, renderer)
        case Some(it) =>
          saveHome(user, home.copy(brews = without(home.brews, it), cauldron = home.cauldron :+ it)) *>
            say(user, content.format("home.workbench.potIn", "name" -> it.name), renderer) *>
            afterTake(user, ua, home.brews, it, renderer, brew, brews)
      }
    }

  /** Все отвары — из сумки и из Живой сумки — на полку, пока есть место. */
  private def putBrews(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      gather(user, _.brew.isDefined, HomeRates.BrewShelves - home.brews.size).flatMap {
        case (Nil, 0) => say(user, content.text("home.workbench.noBrews"), renderer) *> brews(user, 0, renderer)
        case (taken, left) =>
          saveHome(user, home.copy(brews = home.brews ++ taken)) *>
            say(user, content.format("home.workbench.brewsPut", "n" -> taken.size.toString) +
              (if (left > 0) "\n" + content.format("home.workbench.brewsLeft", "n" -> left.toString) else ""), renderer) *>
            brews(user, 0, renderer)
      }
    }

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  /** Верстак есть со второго дома; нет — обратно в дом. */
  private def withWorkbench(user: User)(f: HomeData => Task[StateType]): Task[StateType] =
    loadHome(user).flatMap(home =>
      if (home.house.exists(_.tier >= House.Townhouse.tier)) f(home) else ZIO.succeed(StateType.Home))

  /** Стопки одинакового по страницам, листание и номер страницы в кнопках. */
  private def stacks(items: List[Item], p: Int, pick: String, self: String): List[Choice] = {
    val (pageItems, totalPages, cur) = ItemMenu.page(ItemStack.grouped(items), p)
    val buttons = pageItems.zipWithIndex.map { case ((it, n), i) =>
      Choice(pick, ItemMenu.truncate(it.name + ItemStack.countSuffix(n)),
        data = Map("id" -> it.id.toString, "p" -> cur.toString), row = Some(i))
    }
    val nav = List(
      Option.when(cur > 0)(Choice(self, content.text("common.prev"), data = Map("p" -> (cur - 1).toString), row = Some(ItemMenu.NavRow))),
      Option.when(cur < totalPages - 1)(Choice(self, content.text("common.next"), data = Map("p" -> (cur + 1).toString), row = Some(ItemMenu.NavRow)))
    ).flatten
    buttons ++ nav
  }

  /** Сколько таких же, как `it`, лежит в `items` (вместе с ним). */
  private def same(items: List[Item], it: Item): Int =
    ItemStack.key(it).fold(1)(k => items.count(i => ItemStack.key(i).contains(k)))

  /** Вещь по id из кнопки — если она ещё здесь. */
  private def itemOf(items: List[Item], ua: UserAction): Option[Item] =
    payload(ua, "id").flatMap(_.toLongOption).flatMap(id => items.find(_.id == id))

  /** Убрать ровно эту вещь. */
  private def without(items: List[Item], it: Item): List[Item] =
    items.indexWhere(_.id == it.id) match {
      case -1 => items
      case i  => items.patch(i, Nil, 1)
    }

  /** После того как одну взяли: остались ещё такие — экран той же стопки (по
    * следующей её вещи), нет — список. */
  private def afterTake(
      user: User, ua: UserAction, before: List[Item], it: Item, renderer: Renderer,
      stack: (User, UserAction, Renderer) => Task[StateType], list: (User, Int, Renderer) => Task[StateType]
  ): Task[StateType] =
    ItemStack.key(it).flatMap(k => without(before, it).findLast(i => ItemStack.key(i).contains(k))) match {
      case Some(next) =>
        stack(user, UserAction(ua.text, Some(Map("action" -> "", "id" -> next.id.toString, "p" -> page(ua).toString).asJson.noSpaces)), renderer)
      case None => list(user, page(ua), renderer)
    }

  /** Положить в сумку, если есть место. */
  private def toBag(user: User, it: Item): Task[Boolean] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      ok   <- if (inv.freeSlots <= 0L) ZIO.succeed(false)
              else inventoryRepo.addItem(hero.id, it).fold(_ => false, _ => true)
    } yield ok

  /** Собрать вещи из сумки, а следом из Живой сумки — не больше `room`.
    * Возвращает взятое и сколько подходящего осталось за бортом. */
  private def gather(user: User, accepts: Item => Boolean, room: Int): Task[(List[Item], Int)] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      fromBag = inv.items.data.filter(i => accepts(i) && !i.isQuestItem)
      living <- artifacts.fold(ZIO.succeed(List.empty[Item]))(a =>
                  a.get(hero.id).fold(_ => Nil, _.of(ArtifactKind.LivingBag).items.data))
      fromLiving = living.filter(accepts)
      all        = fromBag ++ fromLiving
      taken      = all.take(room.max(0))
      takenIds   = taken.map(_.id).toSet
      _ <- ZIO.when(fromBag.exists(i => takenIds.contains(i.id)))(
             inventoryRepo.removeItems(fromBag.map(_.id).filter(takenIds.contains).toSet, hero.id).mapError(asThrowable))
      // Живая сумка отдаёт своё одной записью: остаток её содержимого как есть,
      // заряды не тратятся.
      _ <- ZIO.foreachDiscard(artifacts.filter(_ => fromLiving.exists(i => takenIds.contains(i.id))))(a =>
             a.applyMagic(hero.id, ArtifactKind.LivingBag, living.filterNot(i => takenIds.contains(i.id)), 0).mapError(asThrowable))
    } yield (taken, all.size - taken.size)

  private def isHerb(it: Item): Boolean = it.material.exists(_.isHerb)

  /** Сваренное в котелке идёт в тот же счёт «Зельевара I», что и в кубе. */
  private def brewedAchievement(user: User, hero: Hero, made: List[Item], renderer: Renderer): Task[Unit] = {
    val brewed = made.flatMap(_.brew).map(_.entryName)
    ZIO.when(brewed.nonEmpty) {
      for {
        lore   <- HerbLore.readLore(heroDao, user.userId)
        updated = lore.brewedAlso(brewed)
        _      <- HerbLore.writeLore(heroDao, user.userId, updated)
        _      <- ZIO.when(BrewKind.rank1.forall(k => updated.brewed.contains(k.entryName)))(
                    MarisaQuest.grant(heroDao, content, user, hero, Achievement.Brewer1, renderer))
      } yield ()
    }.unit
  }

  private def say(user: User, text: String, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(text, Nil))

  private def page(ua: UserAction): Int = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def loadHome(user: User): Task[HomeData] =
    heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  private def saveHome(user: User, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(user.userId, home.asJson)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
