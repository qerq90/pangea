package pangea.service.state.states.home

import io.circe.jawn
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.CubeCraft
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.Hero
import pangea.model.home.{HomeData, HomeRates, House}
import pangea.model.item.{Gem, GemKind, Item}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Random, Task, ZIO}

/** Хранилище камней — на острове, на [[HomeRates.GemVault]] камней. Камни
  * ложатся одной кнопкой — из сумки и из Ларца Азата, пока есть место. На
  * экране — виды камней; нажал на вид — его качества, нажал на качество — один
  * такой камень уходит в сумку.
  *
  * «Улучшить все» — магия алтаря: все камни, которых хватает по трое (вид и
  * качество), сплавляются в камень качеством выше (`CubeCraft.upgradeGems`), по
  * одной энергии дома за каждое сплавление. Живёт в `home_data` (`HomeData.gems`). */
case class GemVaultState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "GvMenu"    -> Target.Run { (u, ua, r) => menu(u, page(ua), r) },
      "GvKind"    -> Target.Run { (u, ua, r) => kindScreen(u, ua, r) },
      "GvTake"    -> Target.Run { (u, ua, r) => take(u, ua, r) },
      "GvPutAll"  -> Target.Run { (u, _, r) => putAll(u, r) },
      "GvUpgrade" -> Target.Run { (u, _, r) => upgrade(u, r) },
      "LeaveGv"   -> Target.Goto(StateType.Home)
    ),
    fallback = Target.Run { (u, _, r) => menu(u, 0, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = menu(user, 0, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Сколько камней и энергии; виды камней кнопками по страницам. */
  private def menu(user: User, p: Int, renderer: Renderer): Task[StateType] =
    withVault(user) { home =>
      val kinds = byKind(home.gems)
      val (pageKinds, totalPages, cur) = ItemMenu.page(kinds, p)
      val buttons = pageKinds.zipWithIndex.map { case ((kind, n), i) =>
        Choice("GvKind", content.format("home.gems.kindLine", "name" -> kind.baseName, "n" -> n.toString),
          data = Map("k" -> kind.entryName, "p" -> cur.toString), row = Some(i))
      }
      val nav = List(
        Option.when(cur > 0)(Choice("GvMenu", content.text("common.prev"), data = Map("p" -> (cur - 1).toString), row = Some(ItemMenu.NavRow))),
        Option.when(cur < totalPages - 1)(Choice("GvMenu", content.text("common.next"), data = Map("p" -> (cur + 1).toString), row = Some(ItemMenu.NavRow)))
      ).flatten
      val tail = List(
        Choice("GvPutAll", content.text("home.gems.putAll"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
        Choice("GvUpgrade", content.text("home.gems.upgrade"), row = Some(ItemMenu.NavRow + 1)),
        Choice("LeaveGv", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))
      )
      val text = content.format("home.gems.text", "n" -> home.gems.size.toString, "max" -> HomeRates.GemVault.toString,
        "charges" -> home.charges.toString, "chargesMax" -> HomeRates.AltarMaxCharges.toString)
      renderer.show(user, Screen(text, buttons ++ nav ++ tail)).as(StateType.GemVault)
    }

  /** Вид камня: какие качества есть и сколько; нажал — один в сумку. */
  private def kindScreen(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withVault(user) { home =>
      kindOf(ua).filter(k => home.gems.exists(_.gem.exists(_.kind == k))) match {
        case None => menu(user, page(ua), renderer)
        case Some(kind) =>
          val grades = home.gems.flatMap(_.gem).filter(_.kind == kind).groupBy(_.grade).toList.sortBy(_._1)
          val buttons = grades.zipWithIndex.map { case ((grade, gs), i) =>
            Choice("GvTake", ItemMenu.truncate(content.format("home.gems.gradeLine",
                "name" -> Gem.gradeName(kind, grade), "n" -> gs.size.toString)),
              data = Map("k" -> kind.entryName, "g" -> grade.toString, "p" -> page(ua).toString), row = Some(i))
          }
          val back = Choice("GvMenu", content.text("home.house.back"), color = ChoiceColor.Negative,
            data = Map("p" -> page(ua).toString), row = Some(grades.size))
          renderer.show(user, Screen(content.format("home.gems.kind", "name" -> kind.baseName,
            "n" -> grades.map(_._2.size).sum.toString), buttons :+ back)).as(StateType.GemVault)
      }
    }

  /** Один камень этого вида и качества — в сумку, если есть место. */
  private def take(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withVault(user) { home =>
      val wanted = for {
        kind  <- kindOf(ua)
        grade <- payload(ua, "g").flatMap(_.toIntOption)
        gem   <- home.gems.findLast(_.gem.contains(Gem(kind, grade)))
      } yield gem
      wanted match {
        case None => kindScreen(user, ua, renderer)
        case Some(gem) =>
          for {
            hero <- getHero(user)
            inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
            ok   <- if (inv.freeSlots <= 0L) ZIO.succeed(false)
                    else inventoryRepo.addItem(hero.id, gem).fold(_ => false, _ => true)
            _    <- if (!ok) say(user, content.text("home.gems.bagFull"), renderer)
                    else saveHome(user, home.copy(gems = without(home.gems, gem))) *>
                           say(user, content.format("home.gems.taken", "name" -> gem.name), renderer)
            s    <- kindScreen(user, ua, renderer)
          } yield s
      }
    }

  /** Все камни — из сумки, а следом из Ларца — в хранилище, пока есть место. */
  private def putAll(user: User, renderer: Renderer): Task[StateType] =
    withVault(user) { home =>
      for {
        hero      <- getHero(user)
        inv       <- inventoryRepo.get(hero.id).mapError(asThrowable)
        fromBag    = inv.items.data.filter(i => i.gem.isDefined && !i.isQuestItem)
        casket    <- artifacts.fold(ZIO.succeed(List.empty[Item]))(a =>
                       a.get(hero.id).fold(_ => Nil, _.of(ArtifactKind.Casket).items.data))
        fromCasket = casket.filter(_.gem.isDefined)
        all        = fromBag ++ fromCasket
        taken      = all.take((HomeRates.GemVault - home.gems.size).max(0))
        ids        = taken.map(_.id).toSet
        res       <- if (all.isEmpty) say(user, content.text("home.gems.nothingToPut"), renderer) *> menu(user, 0, renderer)
                     else for {
                       _ <- ZIO.when(fromBag.exists(i => ids.contains(i.id)))(
                              inventoryRepo.removeItems(fromBag.map(_.id).filter(ids.contains).toSet, hero.id).mapError(asThrowable))
                       // Ларец отдаёт своё одной записью: остаток как есть, заряды не тратятся.
                       _ <- ZIO.foreachDiscard(artifacts.filter(_ => fromCasket.exists(i => ids.contains(i.id))))(a =>
                              a.applyMagic(hero.id, ArtifactKind.Casket, casket.filterNot(i => ids.contains(i.id)), 0).mapError(asThrowable))
                       _ <- saveHome(user, home.copy(gems = home.gems ++ taken))
                       left = all.size - taken.size
                       _ <- say(user, content.format("home.gems.put", "n" -> taken.size.toString) +
                              (if (left > 0) "\n" + content.format("home.gems.putLeft", "n" -> left.toString) else ""), renderer)
                       s <- menu(user, 0, renderer)
                     } yield s
      } yield res
    }

  /** Магия алтаря: все тройки одного вида и качества — в камень качеством выше,
    * по энергии дома за каждое сплавление. */
  private def upgrade(user: User, renderer: Renderer): Task[StateType] =
    withVault(user) { home =>
      if (home.charges <= 0) say(user, content.text("home.gems.noCharges"), renderer) *> menu(user, 0, renderer)
      else for {
        hero   <- getHero(user)
        seed   <- Random.nextLong
        result  = CubeCraft.upgradeGems(home.gems, home.charges, Rng(seed))
        res    <- if (!result.anyApplied) say(user, content.text("home.gems.nothingToUpgrade"), renderer) *> menu(user, 0, renderer)
                  else for {
                    items <- ZIO.foreach(result.items)(i => if (i.id > 0L) ZIO.succeed(i) else itemRepo.persist(hero.id, i))
                    _     <- saveHome(user, home.copy(gems = items, charges = home.charges - result.chargesUsed))
                    _     <- say(user, content.format("home.gems.upgraded", "n" -> result.chargesUsed.toString), renderer)
                    s     <- menu(user, 0, renderer)
                  } yield s
      } yield res
    }

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  /** Хранилище камней — только на острове; нет — обратно в дом. */
  private def withVault(user: User)(f: HomeData => Task[StateType]): Task[StateType] =
    loadHome(user).flatMap(home => if (home.house.contains(House.Island)) f(home) else ZIO.succeed(StateType.Home))

  /** Виды камней с их числом — по порядку видов. */
  private def byKind(gems: List[Item]): List[(GemKind, Int)] = {
    val counts = gems.flatMap(_.gem).groupBy(_.kind).view.mapValues(_.size).toMap
    GemKind.values.toList.flatMap(k => counts.get(k).map(k -> _))
  }

  private def without(items: List[Item], it: Item): List[Item] =
    items.indexWhere(_.id == it.id) match {
      case -1 => items
      case i  => items.patch(i, Nil, 1)
    }

  private def kindOf(ua: UserAction): Option[GemKind] = payload(ua, "k").flatMap(GemKind.withNameOption)

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
