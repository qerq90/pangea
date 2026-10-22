package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.home.{HomeData, HomeRates, House}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.{ParamsBuff, StatBoost}
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.schedule.Scheduler
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «🏠 Дом» из города. Пока дома нет — канцелярия совета: Юнона Кинтслев
  * продаёт дома. В первый раз — сцена: листок на доске у ратуши и знакомство.
  * Когда дом есть — сам дом: кровать, библиотека (со второго), дверь к Юноне,
  * если захочется дом побольше.
  *
  * Покупка — в два шага: рассказ о доме с ценой и «Подписать». Дом выше
  * покупается с зачётом того, что есть (см. [[House.doubloonPrice]]); за серебро
  * — только домик, и только без дома.
  *
  * Кровать — как комната в таверне, только своя и бесплатно: сон по таймеру
  * (2 / 1,5 / 1 ч), травмы проходят, HP, броня, энергия и отряд — как новые; встал
  * раньше — ничего этого нет. Выспался — ещё и «выспался»: больше опыта на
  * время ([[House.restedMs]]). Пока герой спит, отметка о начале сна лежит в
  * `scene_data`, а будит его таймер ([[TaskKind.HomeSleep]]). */
case class HomeState(
  heroDao:   HeroDao,
  scheduler: Scheduler,
  content:   SceneContent,
  bank:      Option[BankRepository] = None
) extends State {
  import HomeState._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "HomeMain"    -> Target.Run { (u, _, r) => showHouse(u, r) },
      "HomeOffice"  -> Target.Run { (u, _, r) => office(u, r) },
      "HomeOffer"   -> Target.Run { (u, ua, r) => offer(u, ua, r) },
      "HomeBuy"     -> Target.Run { (u, ua, r) => buy(u, ua, r) },
      "HomeBed"     -> Target.Run { (u, _, r) => bed(u, r) },
      "HomeSleep"   -> Target.Run { (u, _, r) => sleep(u, r) },
      WakeKey       -> Target.Run { (u, _, r) => wake(u, r) },
      "HomeWakeYes" -> Target.Run { (u, _, r) => wakeEarly(u, r) },
      "HomeWakeNo"  -> Target.Run { (u, _, r) => showSleeping(u, r) },
      "HomeLibrary" -> Target.Run { (u, _, r) => library(u, r) },
      "LeaveHome"   -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.Home) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  /** Спит — экран сна; дом есть — дом; нет — канцелярия Юноны. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    sleepStartedAt(user).flatMap {
      case Some(_) => showSleeping(user, renderer).unit
      case None    => loadHome(user).flatMap(home => (if (home.owns) showHouse(user, renderer) else office(user, renderer)).unit)
    }

  /** Пока герой спит, работают только кнопки пробуждения: старая кнопка из
    * прежнего экрана не уведёт его из постели. */
  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    sleepStartedAt(user).flatMap {
      case Some(_) if !actionOf(ua).exists(WakeActions.contains) => showSleeping(user, renderer)
      case _                                                     => branch.act(user, ua, renderer)
    }

  // ── Канцелярия совета ───────────────────────────────────────────────────────

  /** Юнона и что у совета есть на продажу. В первый раз — листок на доске и
    * знакомство; дальше — короткое приветствие, своё для тех, у кого дом есть. */
  private def office(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      home   <- loadHome(user)
      first   = !home.metJunona
      _      <- ZIO.when(first)(saveHome(user, home.copy(metJunona = true)))
      current = home.house
      text    =
        if (first) content.text("home.office.intro")
        else if (current.contains(House.Island)) content.format("home.office.top", "race" -> hero.race.genitivePlural)
        else if (current.isDefined) content.text("home.office.owner")
        else content.text("home.office.greeting")
      offers  = House.forSale(current).flatMap(offerButtons(_, current)).zipWithIndex.map { case (c, i) => c.copy(row = Some(i)) }
      back    =
        if (current.isDefined) Choice("HomeMain", content.text("home.office.home"), color = ChoiceColor.Negative, row = Some(offers.size))
        else Choice("LeaveHome", content.text("home.office.leave"), color = ChoiceColor.Negative, row = Some(offers.size))
      _      <- renderer.show(user, Screen(text, offers :+ back))
    } yield StateType.Home

  /** Кнопки покупки дома: цена — уже с зачётом сданного; у домика без дома —
    * две, за серебро и за дублоны. */
  private def offerButtons(h: House, current: Option[House]): List[Choice] = {
    val silver = House.silverPrice(h, current).map(p =>
      Choice("HomeOffer", content.format("home.office.offerSilver", "name" -> name(h), "price" -> money(p)),
        data = Map("h" -> h.key, "pay" -> PaySilver)))
    val gold = Choice("HomeOffer", content.format("home.office.offerDoubloons", "name" -> name(h),
        "price" -> money(House.doubloonPrice(h, current))),
      color = ChoiceColor.Primary, data = Map("h" -> h.key, "pay" -> PayDoubloons))
    silver.toList :+ gold
  }

  /** Рассказ Юноны о доме, особая реплика про зачёт, если дом уже есть, и
    * вопрос: подписываем? */
  private def offer(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      deal(ua, home) match {
        case None => office(user, renderer)
        case Some((h, pay, price)) =>
          val trade   = home.house.fold("")(old =>
            "\n\n" + content.format(s"home.houses.${h.key}.tradeFrom.${old.key}", "price" -> money(price)))
          val confirm = content.format("home.office.confirm", "name" -> name(h), "price" -> priceText(price, pay)) +
            home.house.fold("")(old => " " + content.format("home.office.confirmTrade", "old" -> name(old)))
          renderer.show(user, Screen(content.text(s"home.houses.${h.key}.pitch") + trade + "\n\n" + confirm, List(
            Choice("HomeBuy", content.text("home.office.sign"), color = ChoiceColor.Positive,
              data = Map("h" -> h.key, "pay" -> pay), row = Some(0)),
            Choice("HomeOffice", content.text("home.office.think"), color = ChoiceColor.Negative, row = Some(0))
          ))).as(StateType.Home)
      }
    }

  /** Подписать: деньги — сначала, дом — потом. Не хватило — Юнона говорит
    * сколько, и ничего не списано. Старый дом уходит в управление сам: дом у
    * героя всегда один. */
  private def buy(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      res  <- deal(ua, home) match {
        case None => office(user, renderer)
        case Some((h, pay, price)) =>
          charge(hero, pay, price).flatMap {
            case Left(lack) =>
              renderer.show(user, Screen(content.format("home.office.lack", "lack" -> priceText(lack, pay)), Nil)) *>
                office(user, renderer)
            case Right(_) =>
              saveHome(user, home.copy(tier = h.tier, metJunona = true)) *>
                renderer.show(user, Screen(content.text(s"home.houses.${h.key}.bought"), Nil)) *>
                showHouse(user, renderer)
          }
      }
    } yield res

  /** Что за сделка в кнопке: дом, который сейчас продаётся, способ платы и цена. */
  private def deal(ua: UserAction, home: HomeData): Option[(House, String, Long)] =
    for {
      h     <- payload(ua, "h").flatMap(House.byKey).filter(House.forSale(home.house).contains)
      pay   <- payload(ua, "pay")
      price <- pay match {
        case PaySilver    => House.silverPrice(h, home.house)
        case PayDoubloons => Some(House.doubloonPrice(h, home.house))
        case _            => None
      }
    } yield (h, pay, price)

  /** Списать цену: серебро — общим кошелём (с ячейкой), дублоны — с героя.
    * Не хватило — `Left` с тем, сколько не хватает. */
  private def charge(hero: Hero, pay: String, price: Long): Task[Either[Long, Unit]] =
    if (pay == PaySilver)
      purse.wallet(hero).flatMap { w =>
        if (!w.canAfford(price)) ZIO.left(price - w.total)
        else purse.charge(hero.userId, hero, price).as(Right(()))
      }
    else if (hero.doubloons < price) ZIO.left(price - hero.doubloons)
    else heroDao.updateDoubloons(hero.userId, hero.doubloons - price).as(Right(()))

  // ── Дом ─────────────────────────────────────────────────────────────────────

  private def showHouse(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          val buttons = List(
            Some(Choice("HomeBed", content.text("home.house.bed"), color = ChoiceColor.Positive, row = Some(0))),
            Option.when(h.tier >= House.Townhouse.tier)(Choice("HomeLibrary", content.text("home.house.library"), row = Some(0))),
            Some(Choice("HomeOffice", content.text("home.house.office"), row = Some(1))),
            Some(Choice("LeaveHome", content.text("home.house.leave"), color = ChoiceColor.Negative, row = Some(2)))
          ).flatten
          renderer.show(user, Screen(content.text(s"home.houses.${h.key}.inside"), buttons)).as(StateType.Home)
      }
    }

  /** Библиотека — у каждого дома своя: маленькая в добротном доме, большая
    * личная на острове. В домике её нет. */
  private def library(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house.filter(_.tier >= House.Townhouse.tier) match {
        case None    => showHouse(user, renderer)
        case Some(h) => renderer.show(user, Screen(content.text(s"home.houses.${h.key}.library"), List(backToHouse))).as(StateType.Home)
      }
    }

  // ── Кровать ─────────────────────────────────────────────────────────────────

  private def bed(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          val text = content.text(s"home.houses.${h.key}.bed") + "\n\n" +
            content.format("home.bed.info", "sleep" -> duration(h.sleepMs),
              "rested" -> duration(h.restedMs), "pct" -> HomeRates.RestedExpPct.toString)
          renderer.show(user, Screen(text, List(
            Choice("HomeSleep", content.text("home.bed.sleep"), color = ChoiceColor.Positive, row = Some(0)),
            backToHouse.copy(row = Some(1))
          ))).as(StateType.Home)
      }
    }

  /** Лечь спать: отметка в `scene_data`, будильник — таймером. */
  private def sleep(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          for {
            now <- nowMs
            _   <- heroDao.writeSceneData(user.userId, Json.obj(SleepStartKey -> now.asJson))
            _   <- scheduler.schedule(user.userId, now + h.sleepMs, TaskKind.HomeSleep, StateType.Home, WakeAction)
            s   <- showSleeping(user, renderer)
          } yield s
      }
    }

  private def showSleeping(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- sleepStartedAt(user)
      home    <- loadHome(user)
      left     = (started, home.house) match {
                   case (Some(s), Some(h)) => (h.sleepMs - (now - s)).max(0L)
                   case _                  => 0L
                 }
      _       <- renderer.show(user, Screen(content.format("home.bed.sleeping", "remaining" -> remaining(left)),
                   List(Choice(WakeKey, content.text("home.bed.wake"), color = ChoiceColor.Negative))))
    } yield StateType.Home

  /** Встать: выспался — травмы проходят, силы возвращаются, и ещё «выспался»;
    * рано — сперва спросить. Будит и таймер — тем же действием. */
  private def wake(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- sleepStartedAt(user)
      home    <- loadHome(user)
      res     <- (started, home.house) match {
        case (Some(s), Some(h)) if now - s >= h.sleepMs => wokeRested(user, h, now, renderer)
        case (Some(s), Some(h)) =>
          renderer.show(user, Screen(
            content.format("home.bed.confirmWake", "remaining" -> remaining(h.sleepMs - (now - s))),
            List(
              Choice("HomeWakeYes", content.text("home.bed.wakeYes"), color = ChoiceColor.Negative, row = Some(0)),
              Choice("HomeWakeNo", content.text("home.bed.wakeNo"), row = Some(0))))).as(StateType.Home)
        case _ => clearSleep(user) *> enter(user, renderer).as(StateType.Home)
      }
    } yield res

  /** Как в снятой комнате таверны: травмы прочь, HP, броня и энергия — до
    * верха, отряд отдохнул вместе с героем. Сверху — «выспался». */
  private def wokeRested(user: User, h: House, now: Long, renderer: Renderer): Task[StateType] =
    for {
      hero    <- getHero(user)
      healed   = hero.copy(traumaUntil = None, traumaNames = Nil)
      _       <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(
                   hp = healed.effectiveMaxHp(now), armor = healed.effectiveMaxArmor(now), energy = healed.maxEnergy(now)))
      _       <- heroDao.updateSquad(user.userId, hero.squad.restored(hero.lvl))
      _       <- heroDao.updateTrauma(user.userId, None, Nil)
      _       <- heroDao.updateStatBoosts(user.userId,
                   hero.statBoosts.add(StatBoost(HomeRates.RestedBoost, ParamsBuff.zero, now + h.restedMs), now))
      _       <- clearSleep(user)
      _       <- renderer.show(user, Screen(content.format("home.bed.woke",
                   "rested" -> duration(h.restedMs), "pct" -> HomeRates.RestedExpPct.toString), Nil))
      s       <- showHouse(user, renderer)
    } yield s

  /** Встал раньше: ни лечения, ни бафа — просто снова дома. */
  private def wakeEarly(user: User, renderer: Renderer): Task[StateType] =
    clearSleep(user) *> showHouse(user, renderer)

  private def clearSleep(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null) *> scheduler.cancel(user.userId, TaskKind.HomeSleep)

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  private def backToHouse: Choice =
    Choice("HomeMain", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(0))

  private def name(h: House): String = content.text(s"home.houses.${h.key}.name")

  private def priceText(price: Long, pay: String): String =
    content.format(if (pay == PaySilver) "home.office.silver" else "home.office.doubloons", "price" -> money(price))

  private def sleepStartedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.hcursor.get[Long](SleepStartKey).toOption))

  private def loadHome(user: User): Task[HomeData] =
    heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  private def saveHome(user: User, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(user.userId, home.asJson)

  private def actionOf(ua: UserAction): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object HomeState {

  /** Способ платы в кнопке покупки. */
  val PaySilver: String    = "s"
  val PayDoubloons: String = "d"

  /** Когда герой лёг спать (`scene_data`). */
  val SleepStartKey: String = "homeSleepStartedAt"

  /** Кнопка «Встать» — её же шлёт будильник. */
  val WakeKey: String    = "HomeWake"
  val WakeAction: String = s"""{"action":"$WakeKey"}"""

  /** Что можно нажать, пока герой спит. */
  val WakeActions: Set[String] = Set(WakeKey, "HomeWakeYes", "HomeWakeNo")

  /** Деньги с разрядами: 20 000. */
  def money(n: Long): String = n.toString.reverse.grouped(3).mkString(" ").reverse

  /** Сколько длится сон или «выспался»: «2 ч», «1 ч 30 мин», «45 мин». */
  def duration(ms: Long): String = {
    val mins = (ms / 60000L).max(0L)
    val h    = mins / 60L
    val m    = mins % 60L
    if (h > 0 && m > 0) s"$h ч $m мин" else if (h > 0) s"$h ч" else s"$m мин"
  }

  /** Сколько осталось спать — с минутами и, под конец, секундами. */
  def remaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val h    = secs / 3600L
    val m    = (secs % 3600L) / 60L
    if (h > 0) s"$h ч $m мин" else if (m > 0) s"$m мин" else s"$secs с"
  }
}
