package pangea.service.state.states.gustavo

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Random, Task}

/** Экран зелья лечения травм Густаво. За 100 × уровень золота снимает случайную активную
 *  травму, после чего зелье уходит на кулдаун 30 минут. Нет активных травм — «Пошёл отсюда,
 *  шутник» без списаний. По завершении возвращает в меню [[GustavoState]]. */
case class GustavoHealState(
  heroDao: HeroDao,
  content: SceneContent
) extends State with GustavoScene {

  private val branch = new Branch(
    routes = Map(
      "BuyPotion" -> Target.Run { (u, _, r) => buy(u, r) },
      "Back"      -> Target.Goto(StateType.Gustavo)
    ),
    fallback = Target.Goto(StateType.Gustavo)
  )

  override def targetStates: Set[StateType] = Set(StateType.Gustavo)

  override def enter(user: User, renderer: Renderer): Task[Unit] = show(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def show(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      data <- loadData(user)
      hero <- getHero(user)
      _ <- healRemaining(data, now) match {
             case Some(left) =>
               renderer.show(user, Screen(content.format("gustavo.cooldown", "mins" -> minsOf(left)),
                 List(content.choice("Back", "gustavo.offerBack"))))
             case None =>
               val choices = List(
                 content.choice("BuyPotion", "gustavo.buyLabel"),
                 content.choice("Back",      "gustavo.offerBack")
               )
               renderer.show(user, Screen(content.format("gustavo.offer", "cost" -> cost(hero).toString), choices, inline = true))
           }
    } yield ()

  /** После покупки (или отказа) возвращаемся в меню Густаво — его `enter` перерисует
    * кнопки (кнопка лечения станет красной на свежем кулдауне). */
  private def buy(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      data <- loadData(user)
      _ <- healRemaining(data, now) match {
             case Some(left) =>
               renderer.show(user, Screen(content.format("gustavo.cooldown", "mins" -> minsOf(left)), Nil))
             case None => applyPotion(user, hero, data, now, renderer)
           }
    } yield StateType.Gustavo

  private def applyPotion(user: User, hero: Hero, data: GustavoData, now: Long, renderer: Renderer): Task[Unit] = {
    val active = hero.activeTraumas(now)
    val price  = cost(hero)
    if (active.isEmpty)
      renderer.show(user, Screen(content.text("gustavo.noTraumas"), Nil))
    else if (hero.gold < price)
      renderer.show(user, Screen(content.format("gustavo.notEnoughGold", "cost" -> price.toString), Nil))
    else
      for {
        idx      <- Random.nextIntBetween(0, active.length)
        healed    = active(idx)
        newNames  = removeFirst(hero.traumaNames, healed.name)
        newUntil  = if (newNames.isEmpty) None else hero.traumaUntil
        _        <- heroDao.updateGold(user.userId, hero.gold - price)
        _        <- heroDao.updateTrauma(user.userId, newUntil, newNames)
        _        <- heroDao.writeGustavoData(user.userId,
                      data.copy(healCooldownUntil = Some(now + GustavoData.HealCooldownMs)).asJson)
        _        <- renderer.show(user, Screen(content.format("gustavo.healed", "trauma" -> healed.name), Nil))
      } yield ()
  }

  private def healRemaining(data: GustavoData, now: Long): Option[Long] =
    data.healCooldownUntil.filter(_ > now).map(_ - now)

  private def removeFirst(names: List[String], name: String): List[String] =
    names.indexOf(name) match {
      case -1 => names
      case i  => names.patch(i, Nil, 1)
    }
}
