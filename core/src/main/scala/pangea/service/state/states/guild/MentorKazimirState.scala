package pangea.service.state.states.guild

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.rune.{Rune, RuneData}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.InventoryState
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

/** Наставник Казимир — руны на теле. Снимает узор руны с вещи из сумки и
  * выжигает на теле героя (клеймо: до двух боевых и четырёх пассивных, за
  * репутацию искателей, вещь сгорает); углубляет понимание руны за
  * сожжённые вещи с той же руной (очки по редкости, потолок по уровню);
  * переклеймовывает (снимает узор, понимание остаётся). Понимание — общее для
  * клейма и надетой вещи (см. [[RuneData]]).
  *
  * Все экраны без состояния: страница и выбранная руна едут в данных кнопки
  * (`p`, `k`, `id`, `g`), поэтому `scene_data` не трогается. */
case class MentorKazimirState(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent) extends State {
  import MentorKazimirState._

  private val branch = new Branch(
    routes = Map(
      "KazimirMenu"        -> Target.Run { (u, _, r) => enter(u, r).as(StateType.MentorKazimir) },
      "Brand"              -> Target.Run { (u, _, r) => brandMenu(u, r) },
      "BrandList"          -> Target.Run { (u, ua, r) => brandList(u, ua, r) },
      "BrandRune"          -> Target.Run { (u, ua, r) => brandRune(u, ua, r) },
      "BrandItem"          -> Target.Run { (u, ua, r) => brandItem(u, ua, r) },
      "Rebrand"            -> Target.Run { (u, _, r) => rebrandList(u, r) },
      "RebrandRune"        -> Target.Run { (u, ua, r) => rebrandConfirm(u, ua, r) },
      "RebrandYes"         -> Target.Run { (u, ua, r) => rebrand(u, ua, r) },
      "Deepen"             -> Target.Run { (u, _, r) => deepenMenu(u, r) },
      "DeepenList"         -> Target.Run { (u, ua, r) => deepenList(u, ua, r) },
      "DeepenRune"         -> Target.Run { (u, ua, r) => deepenRune(u, ua, r) },
      "DeepenItem"         -> Target.Run { (u, ua, r) => deepenItem(u, ua, r) },
      "BurnAll"            -> Target.Run { (u, _, r) => burnAllConfirm(u, r) },
      "BurnAllYes"         -> Target.Run { (u, _, r) => burnAll(u, r) },
      "BurnSettings"       -> Target.Run { (u, _, r) => burnSettings(u, r) },
      "BurnRarity"         -> Target.Run { (u, ua, r) => toggleBurnRarity(u, ua, r) },
      "Runes"              -> Target.Run { (u, ua, r) => runesView(u, ua, r) },
      "LeaveMentorKazimir" -> Target.Goto(StateType.TrainingHall)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.MentorKazimir) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.MentorKazimir

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("kazimir.intro"), List(
      content.choice("Brand", "kazimir.brandLabel").copy(row = Some(0)),
      content.choice("Deepen", "kazimir.deepenLabel").copy(row = Some(1)),
      Choice("Runes", content.text("kazimir.runesLabel"), row = Some(2)),
      content.choice("LeaveMentorKazimir", "kazimir.back").copy(row = Some(3)))))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Нанести руну ────────────────────────────────────────────────────────────

  private def brandMenu(user: User, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap { hero =>
      val r = hero.runes
      renderer.show(user, Screen(
        content.format("kazimir.brand.menu",
          "a" -> r.active.size.toString, "aMax" -> Rune.ActiveSlots.toString,
          "p" -> r.passive.size.toString, "pMax" -> Rune.PassiveSlots.toString,
          "cost" -> r.nextCost.toString),
        List(
          Choice("BrandList", content.text("kazimir.brand.activeLabel"), data = Map("kind" -> "a", "p" -> "0"), row = Some(0)),
          Choice("BrandList", content.text("kazimir.brand.passiveLabel"), data = Map("kind" -> "p", "p" -> "0"), row = Some(0)),
          content.choice("Rebrand", "kazimir.brand.rebrandLabel").copy(row = Some(1)),
          content.choice("KazimirMenu", "kazimir.back").copy(row = Some(2)))))
    }.as(StateType.MentorKazimir)

  /** Руны выбранного вида на вещах в сумке, кроме уже выжженных. */
  private def brandList(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      active = payload(ua, "kind").contains("a")
      runes  = runesIn(items).filter(r => r.isActive == active && !hero.runes.isBranded(r))
      page   = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)
      text   = content.text(if (active) "kazimir.brand.activeText" else "kazimir.brand.passiveText")
      _     <- renderer.show(user, runeList(text, runes, page, "BrandRune", "BrandList", Map("kind" -> (if (active) "a" else "p")), "Brand"))
    } yield StateType.MentorKazimir

  /** Вещи в сумке с этой руной — какую сжечь. */
  private def brandRune(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      res   <- runeOf(ua) match {
        case None       => brandMenu(user, renderer)
        case Some(rune) =>
          val with_ = items.filter(i => Rune.of(i).contains(rune))
          val page  = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)
          val text  = s"${rune.label}\n\n" +
            content.text(if (rune.isActive) "kazimir.brand.pickActive" else "kazimir.brand.pickPassive") + "\n\n" +
            content.format("kazimir.brand.cost", "cost" -> hero.runes.nextCost.toString)
          renderer.show(user, itemList(text, with_, page, rune, "BrandItem", "BrandRune", "Brand")).as(StateType.MentorKazimir)
      }
    } yield res

  /** Клеймо: вещь сгорает, репутация уходит, руна на теле. Проверки — по порядку. */
  private def brandItem(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      res   <- (runeOf(ua), payload(ua, "id").flatMap(_.toLongOption).flatMap(id => items.find(_.id == id))) match {
        case (Some(rune), Some(item)) if Rune.of(item).contains(rune) =>
          val r    = hero.runes
          val cost = r.nextCost
          if (r.isBranded(rune)) say(user, renderer, content.text("kazimir.brand.already"))
          else if (!r.hasRoomFor(rune)) say(user, renderer, content.text("kazimir.brand.noRoom"))
          else if (hero.guildReputation < cost)
            say(user, renderer, content.format("kazimir.brand.noReputation", "cost" -> cost.toString, "have" -> hero.guildReputation.toString))
          else for {
            _ <- inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString))
            _ <- heroDao.updateGuildReputation(user.userId, hero.guildReputation - cost)
            _ <- heroDao.updateRunes(user.userId, r.brand(rune))
            // Тайник на теле держит слоты сумки, как надетый Тайник (см. InventoryState.equipmentStashDelta).
            _ <- ZIO.when(rune == Rune.Passive(pangea.model.item.PassiveKind.Stash) && !hero.passives.kinds.contains(pangea.model.item.PassiveKind.Stash))(
                   inventoryRepo.increaseCapacity(hero.id, pangea.model.item.PassiveKind.Stash.ExtraSlots).mapError(e => new Throwable(e.toString)))
            _ <- renderer.show(user, Screen(content.text(if (rune.isActive) "kazimir.brand.doneActive" else "kazimir.brand.donePassive"), Nil))
            s <- brandMenu(user, renderer)
          } yield s
        case _ => say(user, renderer, content.text("kazimir.itemGone"))
      }
    } yield res

  // ── Переклеймовать ──────────────────────────────────────────────────────────

  private def rebrandList(user: User, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap { hero =>
      val branded = hero.runes.branded
      val buttons = branded.zipWithIndex.map { case (rune, i) =>
        Choice("RebrandRune", ItemMenu.truncate(rune.label), data = Map("k" -> rune.key),
          color = if (rune.isActive) ChoiceColor.Negative else ChoiceColor.Primary, row = Some(i))
      }
      val text = if (branded.isEmpty) content.text("kazimir.rebrand.empty") else content.text("kazimir.rebrand.list")
      renderer.show(user, Screen(text, buttons :+ content.choice("Brand", "kazimir.back").copy(row = Some(buttons.size))))
    }.as(StateType.MentorKazimir)

  private def rebrandConfirm(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    runeOf(ua) match {
      case None       => rebrandList(user, renderer)
      case Some(rune) =>
        renderer.show(user, Screen(content.format("kazimir.rebrand.confirm", "name" -> rune.label), List(
          Choice("RebrandYes", content.text("kazimir.rebrand.yes"), data = Map("k" -> rune.key), color = ChoiceColor.Negative, row = Some(0)),
          content.choice("Rebrand", "kazimir.rebrand.no").copy(row = Some(0))))).as(StateType.MentorKazimir)
    }

  /** Снять клеймо: место свободно, понимание остаётся. Тайник с тела уносит
    * слоты сумки — если она от этого переполнится, сводить нельзя. */
  private def rebrand(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      res  <- runeOf(ua).filter(hero.runes.isBranded) match {
        case None       => rebrandList(user, renderer)
        case Some(rune) =>
          val stash    = pangea.model.item.PassiveKind.Stash
          val losesStash = rune == Rune.Passive(stash) && !hero.equipment.passiveKinds.contains(stash)
          for {
            inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
            fits  = !losesStash || InventoryState.fitsAfterCapacityChange(inv, -stash.ExtraSlots, 0)
            s    <- if (!fits) say(user, renderer, content.text("kazimir.rebrand.bagFull"))
                    else for {
                      _ <- heroDao.updateRunes(user.userId, hero.runes.unbrand(rune))
                      _ <- ZIO.when(losesStash)(inventoryRepo.increaseCapacity(hero.id, -stash.ExtraSlots).mapError(e => new Throwable(e.toString)))
                      _ <- renderer.show(user, Screen(content.text("kazimir.rebrand.done"), Nil))
                      s <- brandMenu(user, renderer)
                    } yield s
          } yield s
      }
    } yield res

  // ── Углубить понимание ──────────────────────────────────────────────────────

  private def deepenMenu(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("kazimir.deepen.menu"), List(
      Choice("DeepenList", content.text("kazimir.deepen.pickLabel"), data = Map("p" -> "0"), row = Some(0)),
      content.choice("BurnAll", "kazimir.deepen.burnAllLabel").copy(row = Some(1)),
      content.choice("BurnSettings", "kazimir.deepen.settingsLabel").copy(row = Some(2)),
      content.choice("KazimirMenu", "kazimir.back").copy(row = Some(3))))).as(StateType.MentorKazimir)

  /** Любая руна, у которой есть вещи в сумке, — понимание не требует клейма. */
  private def deepenList(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      page   = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)
      _     <- renderer.show(user, runeList(content.text("kazimir.deepen.pickText"), runesIn(items), page, "DeepenRune", "DeepenList", Map.empty, "Deepen"))
    } yield StateType.MentorKazimir

  private def deepenRune(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      res   <- runeOf(ua) match {
        case None       => deepenMenu(user, renderer)
        case Some(rune) =>
          val with_ = items.filter(i => Rune.of(i).contains(rune))
          val page  = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)
          val text  = content.format("kazimir.deepen.rune", "name" -> rune.label,
            "n" -> hero.runes.understandingOf(rune).toString, "max" -> Rune.cap(hero.lvl).toString) + "\n\n" +
            content.text("kazimir.deepen.pickItem")
          renderer.show(user, itemList(text, with_, page, rune, "DeepenItem", "DeepenRune", "Deepen")).as(StateType.MentorKazimir)
      }
    } yield res

  private def deepenItem(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      res   <- (runeOf(ua), payload(ua, "id").flatMap(_.toLongOption).flatMap(id => items.find(_.id == id))) match {
        case (Some(rune), Some(item)) if Rune.of(item).contains(rune) =>
          val cap = Rune.cap(hero.lvl)
          val (updated, gained) = hero.runes.deepen(rune, Rune.points(item.rarity), cap)
          if (gained == 0L) say(user, renderer, content.format("kazimir.deepen.atCap", "name" -> rune.label, "max" -> cap.toString))
          else for {
            _ <- inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString))
            _ <- heroDao.updateRunes(user.userId, updated)
            _ <- renderer.show(user, Screen(content.format("kazimir.deepen.done", "name" -> rune.label,
                   "n" -> updated.understandingOf(rune).toString, "gain" -> gained.toString), Nil))
            s <- deepenRune(user, ua, renderer)
          } yield s
        case _ => say(user, renderer, content.text("kazimir.itemGone"))
      }
    } yield res

  /** Что сгорит по «Сдать всё»: вещи с рунами разрешённых редкостей, по каждой
    * руне — не больше, чем влезает в потолок понимания (сначала те, что дают
    * меньше очков: лишние остаются в сумке). Возвращает данные рун после, что
    * сгорело и прибавку по рунам. */
  private def burnPlan(hero: Hero, items: List[Item]): (RuneData, List[Item], List[(Rune, Long)]) = {
    val cap = Rune.cap(hero.lvl)
    val candidates = items.filter(i => Rune.of(i).isDefined && hero.runes.burns(i.rarity))
    candidates.groupBy(i => Rune.of(i).get).toList.sortBy(_._1.label).foldLeft((hero.runes, List.empty[Item], List.empty[(Rune, Long)])) {
      case ((data, burnt, gains), (rune, its)) =>
        val (data2, burnt2, gain) = its.sortBy(i => Rune.points(i.rarity)).foldLeft((data, burnt, 0L)) { case ((d, b, g), item) =>
          val (d2, got) = d.deepen(rune, Rune.points(item.rarity), cap)
          if (got == 0L) (d, b, g) else (d2, b :+ item, g + got)
        }
        (data2, burnt2, if (gain > 0L) gains :+ (rune -> gain) else gains)
    }
  }

  private def burnAllConfirm(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      (_, burnt, _) = burnPlan(hero, items)
      _     <- if (burnt.isEmpty) renderer.show(user, Screen(content.text("kazimir.deepen.nothingToBurn"),
                 List(content.choice("Deepen", "kazimir.back"))))
               else renderer.show(user, Screen(content.format("kazimir.deepen.burnAllConfirm", "n" -> burnt.size.toString), List(
                 content.choice("BurnAllYes", "kazimir.deepen.burnAllYes").copy(row = Some(0)),
                 content.choice("Deepen", "kazimir.back").copy(row = Some(0)))))
    } yield StateType.MentorKazimir

  private def burnAll(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- bag(hero)
      (data, burnt, gains) = burnPlan(hero, items)
      res   <- if (burnt.isEmpty) say(user, renderer, content.text("kazimir.deepen.nothingToBurn"))
               else for {
                 _ <- inventoryRepo.removeItems(burnt.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString))
                 _ <- heroDao.updateRunes(user.userId, data)
                 lines = gains.map { case (rune, gain) =>
                   content.format("kazimir.deepen.burnAllLine", "name" -> rune.label, "gain" -> gain.toString,
                     "n" -> data.understandingOf(rune).toString) }
                 left  = items.count(i => Rune.of(i).isDefined && hero.runes.burns(i.rarity)) - burnt.size
                 tail  = if (left > 0) "\n\n" + content.text("kazimir.deepen.burnAllLeft") else ""
                 _ <- renderer.show(user, Screen((content.text("kazimir.deepen.burnAllDone") +: lines).mkString("\n") + tail, Nil))
                 s <- deepenMenu(user, renderer)
               } yield s
    } yield res

  // ── Настройка «Сдать всё» — как у продажи хлама ─────────────────────────────

  private def burnSettings(user: User, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap(hero => renderer.show(user, burnSettingsScreen(hero.runes))).as(StateType.MentorKazimir)

  private def burnSettingsScreen(r: RuneData): Screen = {
    def state(on: Boolean) = content.text(if (on) "merchant.junk.on" else "merchant.junk.off")
    val buttons = MerchantState.JunkRarityGroups.zipWithIndex.map { case (g, i) =>
      val on = g.rarities.forall(r.burns)
      Choice("BurnRarity", content.format("merchant.junk.rarity", "emoji" -> g.emoji, "state" -> state(on)),
        color = if (on) ChoiceColor.Positive else ChoiceColor.Negative, data = Map("g" -> g.id), row = Some(i))
    }
    Screen(content.text("kazimir.deepen.settings"), buttons :+ content.choice("Deepen", "kazimir.back").copy(row = Some(buttons.size)))
  }

  private def toggleBurnRarity(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      res  <- payload(ua, "g").flatMap(id => MerchantState.JunkRarityGroups.find(_.id == id)) match {
        case None    => burnSettings(user, renderer)
        case Some(g) =>
          val updated = hero.runes.toggleBurn(g.rarities)
          heroDao.updateRunes(user.userId, updated) *> renderer.show(user, burnSettingsScreen(updated)).as(StateType.MentorKazimir)
      }
    } yield res

  // ── Руны: что на теле и на вещах ────────────────────────────────────────────

  private def runesView(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap { hero =>
      val entries = heroRunes(hero)
      val page    = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)
      val (pageEntries, totalPages, p) = ItemMenu.page(entries, page, RunesPerPage)
      val cap  = Rune.cap(hero.lvl)
      val body = pageEntries.map { case (rune, where) =>
        val n = hero.runes.understandingOf(rune)
        s"${if (rune.isActive) "⚔" else "🔹"} ${rune.label}$where\n${rune.description}\n" +
          content.format("kazimir.runes.line", "n" -> n.toString, "max" -> cap.toString, "strength" -> Rune.strengthText(n))
      }
      val header = content.text("kazimir.runes.header") + (if (totalPages > 1) s" (${p + 1}/$totalPages)" else "")
      val text   = if (entries.isEmpty) content.text("kazimir.runes.empty") else (header +: body).mkString("\n\n")
      val nav    = List(
        Some(content.choice("KazimirMenu", "kazimir.exit").copy(row = Some(0))),
        Option.when(p > 0)(Choice("Runes", content.text("common.prev"), data = Map("p" -> (p - 1).toString), row = Some(0))),
        Option.when(p < totalPages - 1)(Choice("Runes", content.text("common.next"), data = Map("p" -> (p + 1).toString), row = Some(0)))
      ).flatten
      renderer.show(user, Screen(text, nav))
    }.as(StateType.MentorKazimir)

  /** Руны героя с пометкой, где они: на теле, на вещи или там и там. Боевые
    * первыми, в каждом виде — сперва клейма. */
  private def heroRunes(hero: Hero): List[(Rune, String)] = {
    val worn    = hero.equipment.allItems.flatMap(Rune.of).distinct
    val branded = hero.runes.branded
    def mark(rune: Rune): String = {
      val onBody = branded.contains(rune)
      val onItem = worn.contains(rune)
      if (onBody && onItem) content.text("kazimir.runes.both")
      else if (onBody) content.text("kazimir.runes.onBody")
      else content.text("kazimir.runes.onItem")
    }
    val all = (branded ++ worn).distinct
    (all.filter(_.isActive) ++ all.filterNot(_.isActive)).map(r => r -> mark(r))
  }

  // ── Общее ───────────────────────────────────────────────────────────────────

  /** Список рун кнопками по страницам: кнопка руны несёт её ключ, навигация — вид и страницу. */
  private def runeList(text: String, runes: List[Rune], page: Int, pick: String, self: String,
                       selfData: Map[String, String], back: String): Screen = {
    val (pageRunes, totalPages, p) = ItemMenu.page(runes, page)
    val buttons = pageRunes.zipWithIndex.map { case (rune, i) =>
      Choice(pick, ItemMenu.truncate(rune.label), data = Map("k" -> rune.key, "p" -> "0"), row = Some(i))
    }
    val body = if (runes.isEmpty) text + "\n\n" + content.text("kazimir.noRunes") else text
    Screen(body, buttons ++ nav(self, selfData, p, totalPages, back))
  }

  /** Вещи с руной кнопками по страницам. */
  private def itemList(text: String, items: List[Item], page: Int, rune: Rune, pick: String, self: String, back: String): Screen = {
    val (pageItems, totalPages, p) = ItemMenu.page(items, page)
    val buttons = pageItems.zipWithIndex.map { case (item, i) =>
      Choice(pick, ItemMenu.itemButtonLabel(item), data = Map("k" -> rune.key, "id" -> item.id.toString), row = Some(i))
    }
    val body = if (items.isEmpty) text + "\n\n" + content.text("kazimir.noItems") else text
    Screen(body, buttons ++ nav(self, Map("k" -> rune.key), p, totalPages, back))
  }

  private def nav(self: String, data: Map[String, String], page: Int, totalPages: Int, back: String): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice(back, "kazimir.back").copy(row = Some(row))),
      Option.when(page > 0)(Choice(self, content.text("common.prev"), data = data + ("p" -> (page - 1).toString), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice(self, content.text("common.next"), data = data + ("p" -> (page + 1).toString), row = Some(row)))
    ).flatten
  }

  private def say(user: User, renderer: Renderer, text: String): Task[StateType] =
    renderer.show(user, Screen(text, List(content.choice("KazimirMenu", "kazimir.back")))).as(StateType.MentorKazimir)

  /** Руны, встречающиеся на вещах, — по одному разу, по алфавиту. */
  private def runesIn(items: List[Item]): List[Rune] = items.flatMap(Rune.of).distinct.sortBy(_.label)

  private def runeOf(ua: UserAction): Option[Rune] = payload(ua, "k").flatMap(Rune.byKey)

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def bag(hero: Hero): Task[List[Item]] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).map(_.items.data)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MentorKazimirState {
  /** Рун на странице «Рун»: у каждой описание, поэтому меньше, чем кнопок в списках. */
  val RunesPerPage: Int = 5
}
