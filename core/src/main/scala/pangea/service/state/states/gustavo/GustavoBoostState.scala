package pangea.service.state.states.gustavo

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.stats.StatBoost
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.state.{NpcQuestLog, State, UserAction}
import zio.Task

/** Экран зелий-бафов Густаво (бык/медоед/кошка/злобомозг). Каждое даёт +15% соответствующей
 *  характеристике на час. Первое зелье каждого типа бесплатно, дальше 100 × уровень. Пока баф
 *  этого типа активен, повтор недоступен — Густаво «готовит новое зелье». Сами бафы пишутся в
 *  общий [[pangea.model.stats.StatBoosts]] героя; израсходованные бесплатные — в [[GustavoData]]. */
case class GustavoBoostState(
  heroDao: HeroDao,
  content: SceneContent
,
  bank:    Option[BankRepository] = None
) extends State with GustavoScene {

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "BoostBuy" -> Target.Run { (u, ua, r) => buy(u, ua, r) },
      "Back"     -> Target.Goto(StateType.Gustavo)
    ),
    fallback = Target.Run { (u, _, r) => render(u, r).as(StateType.GustavoBoost) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Gustavo, StateType.GustavoBoost)

  override def enter(user: User, renderer: Renderer): Task[Unit] = render(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def render(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- nowMs
      hero   <- getHero(user)
      data   <- loadData(user)
      treat  <- questTreat(user)
      choices = BoostStat.all.map(bs => boostButton(bs, hero, data, now, treat)) :+
                  content.choice("Back", "gustavo.boostBack")
      _ <- renderer.show(user, Screen(content.text("gustavo.boost.intro"), choices))
    } yield ()

  /** Поблажка от Густаво по заданию: зелье выветрилось раньше, чем герой побил
    * троих, — следующее наливается бесплатно, какое бы ни выбрал. */
  private def questTreat(user: User): Task[Boolean] =
    NpcQuestLog.load(heroDao, user.userId).map(q => q.onStep(NpcQuest.Gustavo, 1) && q.of(NpcQuest.Gustavo).bonus)

  /** Кнопка характеристики: красная «готовится» пока баф активен, зелёная «бесплатно» пока не
    * потрачено бесплатное зелье этого типа (или Густаво угощает по заданию), иначе синяя с ценой. */
  private def boostButton(bs: BoostStat, hero: Hero, data: GustavoData, now: Long, treat: Boolean): Choice = {
    val stat = Map("stat" -> bs.key)
    hero.statBoosts.remainingMs(bs.boostName, now) match {
      case Some(left) =>
        content.choice("BoostBuy", "gustavo.boostBusyLabel", "stat" -> bs.label, "mins" -> minsOf(left))
          .copy(data = stat, color = ChoiceColor.Negative)
      case None if treat || !data.freeBoostsUsed.contains(bs.key) =>
        content.choice("BoostBuy", "gustavo.boostFreeLabel", "stat" -> bs.label)
          .copy(data = stat, color = ChoiceColor.Positive)
      case None =>
        content.choice("BoostBuy", "gustavo.boostPaidLabel", "stat" -> bs.label, "cost" -> cost(hero).toString)
          .copy(data = stat)
    }
  }

  private def buy(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadStat(ua).flatMap(BoostStat.byKey) match {
      case None     => render(user, renderer).as(StateType.GustavoBoost)
      case Some(bs) =>
        for {
          now   <- nowMs
          hero  <- getHero(user)
          data  <- loadData(user)
          treat <- questTreat(user)
          _ <- hero.statBoosts.remainingMs(bs.boostName, now) match {
                 case Some(left) => // повтор того же типа, пока прежний баф действует
                   renderer.show(user, Screen(content.format("gustavo.boostBusy",
                     "potion" -> bs.potion, "mins" -> minsOf(left)), Nil))
                 case None =>
                   // Угощение по заданию не тратит бесплатное зелье этого типа.
                   val free  = !data.freeBoostsUsed.contains(bs.key)
                   val price = if (free || treat) 0L else cost(hero)
                   purse.wallet(hero).flatMap { wallet =>
                     if (price > 0 && !wallet.canAfford(price))
                       renderer.show(user, Screen(content.format("gustavo.boostNotEnoughSilver",
                         "potion" -> bs.potion, "cost" -> price.toString), Nil))
                     else
                       applyBoost(user, hero, data, bs, free && !treat, price, now, renderer) *>
                         questPotionTaken(user, now)
                   }
               }
          _ <- render(user, renderer)
        } yield StateType.GustavoBoost
    }

  private def applyBoost(
    user: User, hero: Hero, data: GustavoData, bs: BoostStat,
    free: Boolean, price: Long, now: Long, renderer: Renderer
  ): Task[Unit] = {
    val newBoosts = hero.statBoosts.add(StatBoost(bs.boostName, bs.buff, now + GustavoData.BoostDurationMs), now)
    val newFree   = if (free) data.freeBoostsUsed :+ bs.key else data.freeBoostsUsed
    val msgKey    = if (price == 0L) "gustavo.boostAppliedFree" else "gustavo.boostApplied"
    purse.charge(user.userId, hero, price) *>
      heroDao.updateStatBoosts(user.userId, newBoosts) *>
      heroDao.writeGustavoData(user.userId, data.copy(freeBoostsUsed = newFree).asJson) *>
      renderer.show(user, Screen(content.format(msgKey, "potion" -> bs.potion, "stat" -> bs.label), Nil))
  }

  /** Выпитое зелье закрывает первый шаг задания Густаво: дальше счёт побитых. */
  private def questPotionTaken(user: User, now: Long): Task[Unit] =
    NpcQuestLog.modify(heroDao, user.userId) { q =>
      if (!q.onStep(NpcQuest.Gustavo, 1)) q
      else q.update(NpcQuest.Gustavo)(_.copy(step = 2, counter = 0L, since = now, bonus = false))
    }.unit

  private def payloadStat(ua: UserAction): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("stat")))
}
