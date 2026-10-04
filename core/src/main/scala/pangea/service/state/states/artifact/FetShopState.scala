package pangea.service.state.states.artifact

import io.circe.jawn
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.hero.Hero
import pangea.model.item.{Item, MaterialKind}
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{CityExit, NpcQuestDialog, State, UserAction}
import zio.{Task, ZIO}

/** Лавка Фета в Торговом доме: сборные артефакты Азата. Каждый стоит
 *  [[HeroArtifacts.StepPriceDoubloons]] дублонов, и за ту же цену Фет трижды
 *  его улучшит — каждая ступень добавляет мест внутри. */
case class FetShopState(
  heroDao:       HeroDao,
  artifacts:     ArtifactRepository,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  /** «Живая кожа»: кусок с Гнилого Джо — из такого Фет доращивает сумку. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Fet, "Fet")

  private val branch = new Branch(
    routes = Map(
      "FetShop"     -> Target.Run { (u, _, r)  => showShop(u, r).as(StateType.FetShop) },
      quest.questAction   -> Target.Run { (u, _, r) => questTalk(u, r) },
      quest.acceptAction  -> Target.Run { (u, _, r) => quest.accept(u, r) *> showShop(u, r).as(StateType.FetShop) },
      quest.declineAction -> Target.Run { (u, _, r) => showShop(u, r).as(StateType.FetShop) },
      "FetGoods"    -> Target.Run { (u, ua, r) => withKind(ua)(showGoods(u, _, r)).as(StateType.FetShop) },
      "FetBuy"      -> Target.Run { (u, ua, r) => withKind(ua)(confirm(u, _, r)).as(StateType.FetShop) },
      "FetBuyYes"   -> Target.Run { (u, ua, r) => withKind(ua)(buy(u, _, r)).as(StateType.FetShop) },
      "LeaveFetShop" -> Target.Goto(StateType.TradeHouse),
      CityExit.route
    ),
    fallback = Target.Run { (u, _, r) => showShop(u, r).as(StateType.FetShop) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("fet.intro"), Nil)) *> showShop(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showShop(user: User, renderer: Renderer): Task[Unit] =
    quest.load(user).flatMap { quests =>
      val goods = ArtifactKind.values.toList.zipWithIndex.map { case (k, i) =>
        Choice("FetGoods", content.text(s"artifact.${k.key}.title"), data = Map("kind" -> k.entryName), row = Some(i))
      }
      val questRow = ArtifactKind.values.size
      val leaveRow = questRow + quest.button(quests).size
      renderer.show(user, Screen(content.text("fet.counter"),
        goods ++ quest.button(quests).map(_.copy(row = Some(questRow))).toList ++ List(
          Choice("LeaveFetShop", content.text("fet.leave"),
            color = ChoiceColor.Negative, row = Some(leaveRow)),
          CityExit.button(content, Some(leaveRow)))))
    }

  // ── «Живая кожа» ──────────────────────────────────────────────

  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    quest.load(user).flatMap { quests =>
      if (quests.isDone(NpcQuest.Fet)) showShop(user, renderer)
      else if (!quests.isTaken(NpcQuest.Fet)) quest.offer(user, renderer, quest.text("intro"))
      else turnIn(user, renderer)
    }.as(StateType.FetShop)

  /** Сдача: кожа уходит Фету, герой получает золото и опыт. */
  private def turnIn(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      _ <- inv.items.data.find(FetShopState.ghoulSkin) match {
        case None => renderer.show(user, Screen(quest.text("step1Fail"), Nil)) *> showShop(user, renderer)
        case Some(skin) =>
          for {
            _    <- inventoryRepo.removeItem(skin.id, hero.id).mapError(e => new Throwable(e.toString))
            _    <- heroDao.updateDoubloons(user.userId, hero.doubloons + FetShopState.QuestDoubloons)
            done <- quest.complete(user, hero, identity)
            (_, expLine) = done
            _    <- renderer.show(user, Screen(quest.text("outro"), Nil))
            _    <- renderer.show(user, Screen(quest.format("reward",
                      "doubloons" -> FetShopState.QuestDoubloons.toString, "exp" -> expLine), Nil))
            _    <- showShop(user, renderer)
          } yield ()
      }
    } yield ()

  /** Карточка товара: рассказ Фета, цена и что сейчас с артефактом у героя. */
  private def showGoods(user: User, kind: ArtifactKind, renderer: Renderer): Task[Unit] =
    mine(user, kind).flatMap { a =>
      val state =
        if (!a.owned) content.format("fet.stateNone", "slots" -> kind.slotsPerTier.toString)
        else if (a.canUpgrade) content.format("fet.stateOwned",
          "tier" -> a.tier.toString, "maxTier" -> HeroArtifacts.MaxTier.toString,
          "capacity" -> a.capacity.toString, "next" -> (a.capacity + kind.slotsPerTier).toString)
        else content.format("fet.stateFull", "capacity" -> a.capacity.toString)
      val buyLabel =
        if (!a.owned) content.format("fet.buyLabel", "price" -> HeroArtifacts.StepPriceDoubloons.toString)
        else content.format("fet.upgradeLabel", "price" -> HeroArtifacts.StepPriceDoubloons.toString)
      val buttons =
        Option.when(a.canUpgrade)(Choice("FetBuy", Choice.fit(buyLabel),
          color = ChoiceColor.Positive, data = Map("kind" -> kind.entryName), row = Some(0))).toList :+
          Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1))
      renderer.show(user, Screen(content.text(s"artifact.${kind.key}.pitch") + "\n\n" + state, buttons))
    }

  private def confirm(user: User, kind: ArtifactKind, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user, kind)
      price = HeroArtifacts.StepPriceDoubloons
      _ <- if (!a.canUpgrade) showGoods(user, kind, renderer)
           else if (hero.doubloons < price)
             renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
           else
             renderer.show(user, Screen(
               content.format(if (a.owned) "fet.confirmUpgrade" else "fet.confirmBuy",
                 "title" -> content.text(s"artifact.${kind.key}.title"), "price" -> price.toString),
               List(
                 Choice("FetBuyYes", content.text("fet.confirmYes"), color = ChoiceColor.Positive,
                   data = Map("kind" -> kind.entryName), row = Some(0)),
                 Choice("FetShop", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
               ), inline = true))
    } yield ()

  private def buy(user: User, kind: ArtifactKind, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user, kind)
      price = HeroArtifacts.StepPriceDoubloons
      _ <- if (!a.canUpgrade) showGoods(user, kind, renderer)
           else if (hero.doubloons < price)
             renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
           else
             heroDao.updateDoubloons(user.userId, hero.doubloons - price) *>
               artifacts.upgrade(hero.id, kind).mapError(asThrowable).flatMap { grown =>
                 renderer.show(user, Screen(content.format(if (a.owned) "fet.upgraded" else "fet.bought",
                   "title" -> content.text(s"artifact.${kind.key}.title"),
                   "tier" -> grown.tier.toString, "maxTier" -> HeroArtifacts.MaxTier.toString,
                   "capacity" -> grown.capacity.toString), Nil))
               } *> showGoods(user, kind, renderer)
    } yield ()

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def backRow: List[Choice] =
    List(Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def withKind(ua: UserAction)(f: ArtifactKind => Task[Unit]): Task[Unit] =
    ua.payload
      .flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("kind")))
      .flatMap(ArtifactKind.withNameOption) match {
        case Some(kind) => f(kind)
        case None       => ZIO.unit
      }

  private def mine(user: User, kind: ArtifactKind): Task[Artifact] =
    getHero(user).flatMap(h => artifacts.get(h.id).mapError(asThrowable)).map(_.of(kind))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object FetShopState {

  /** Сколько золота Фет отсыпает за кусок живой кожи. */
  val QuestDoubloons: Long = 25L

  /** Кожа упыря — остаётся только от Гнилого Джо. */
  def ghoulSkin(item: Item): Boolean = item.material.contains(MaterialKind.GhoulSkin)
}
