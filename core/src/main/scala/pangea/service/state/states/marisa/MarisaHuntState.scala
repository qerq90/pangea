package pangea.service.state.states.marisa

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.Rarity
import pangea.model.schedule.TaskKind
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.marisa.MarisaHuntState._
import pangea.service.state.{MarisaQuest, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Поход к тайнику Кельвина по карте из письма Марисе. Из инвентаря (карта
  * активируется только в городе) герой уходит за город на десять минут; по
  * таймеру — тайник (серебро, дублоны, пять повреждённых камней, пурпурная
  * вещь четвёртого уровня — через общий экран добычи) и сразу коллектор:
  * заплатить долг Кельвина или драться. Бой — сюжетный (`story = collector`):
  * без опыта и добычи, кроме 500 серебра и 10 дублонов; смерть в нём разбирает
  * DeathState. Если герой взял с собой Марису, после боя она просит отдать ей
  * долг — «Спаситель Марисы» или «Мерзавец». Так или иначе — в город, письмо и
  * карта уходят, задание закрыто. */
case class MarisaHuntState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  scheduler:     Scheduler,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "HuntDone"      -> Target.Run { (u, _, r) => huntDone(u, r) },
      "PayCollector"  -> Target.Run { (u, _, r) => payCollector(u, r) },
      "FightCollector" -> Target.Run { (u, _, r) => fightCollector(u, r) },
      "GiveMarisa"    -> Target.Run { (u, _, r) => giveMarisa(u, r) },
      "KeepSilver"    -> Target.Run { (u, _, r) => keepSilver(u, r) }
    ),
    fallback = Target.Run { (u, _, r) => onTick(u, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Loot, StateType.Battle, StateType.GlobalMap, StateType.MarisaHunt)

  /** Вход: с чистой сценой не бывает — инвентарь кладёт [[Progress]] до перехода.
    * Идём по шагу: ожидание, коллектор после тайника, развязка после боя. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    readProgress(user).flatMap {
      case Some(p) if p.step == Step.Collector  => showCollector(user, renderer, p)
      case Some(p) if p.step == Step.AfterFight => afterFight(user, renderer, p)
      case Some(p) =>
        for {
          now <- nowMs
          started = p.startedAt.getOrElse(now)
          _   <- ZIO.when(p.startedAt.isEmpty)(
                   writeProgress(user, p.copy(startedAt = Some(now))) *>
                     scheduler.schedule(user.userId, now + HuntDurationMs, TaskKind.MarisaHunt, StateType.MarisaHunt, HuntDoneAction))
          _   <- ZIO.when(p.withMarisa && p.startedAt.isEmpty)(renderer.show(user, Screen(content.text("marisa.hunt.thanksForHonesty"), Nil)))
          _   <- renderer.show(user, Screen(
                   content.format("marisa.hunt.enter", "duration" -> formatRemaining(HuntDurationMs - (now - started))), Nil))
        } yield ()
      case None => renderer.show(user, Screen(content.text("marisa.hunt.enter"), Nil))
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Дорога ──────────────────────────────────────────────────────────────────

  private def onTick(user: User, renderer: Renderer): Task[StateType] =
    for {
      now <- nowMs
      p   <- readProgress(user)
      res <- p match {
        case Some(pr) if pr.step == Step.Collector  => showCollector(user, renderer, pr).as(StateType.MarisaHunt)
        case Some(pr) if pr.step == Step.AfterFight => afterFight(user, renderer, pr).as(StateType.MarisaHunt)
        case Some(pr) if pr.startedAt.exists(s => now - s >= HuntDurationMs) => huntDone(user, renderer)
        case Some(pr) =>
          val remaining = pr.startedAt.map(s => HuntDurationMs - (now - s)).getOrElse(HuntDurationMs)
          renderer.show(user, Screen(content.format("marisa.hunt.wait", "remaining" -> formatRemaining(remaining)), Nil))
            .as(StateType.MarisaHunt)
        case None => ZIO.succeed(StateType.GlobalMap)
      }
    } yield res

  /** Тайник: добыча через общий экран, возврат сюда — на коллектора. */
  private def huntDone(user: User, renderer: Renderer): Task[StateType] =
    for {
      p    <- requireProgress(user)
      _    <- scheduler.cancel(user.userId, TaskKind.MarisaHunt)
      seed <- Random.nextLong
      (gear, _) = ItemGenerator.createItemAtLevel(MarisaQuest.CacheGearLevel, Rarity.Purple, Rng(seed))
      loot  = LootData(
                items       = MarisaQuest.cacheGems :+ gear,
                silvers     = List(MarisaQuest.CacheSilver),
                doubloons   = MarisaQuest.CacheDoubloons,
                returnState = Some(StateType.MarisaHunt),
                eventData   = Some(p.copy(step = Step.Collector).asJson))
      _    <- renderer.show(user, Screen(content.text("marisa.hunt.cache"), Nil))
      _    <- heroDao.writeSceneData(user.userId, loot.asJson)
    } yield StateType.Loot

  // ── Коллектор ───────────────────────────────────────────────────────────────

  private def showCollector(user: User, renderer: Renderer, p: Progress): Task[Unit] =
    getHero(user).flatMap { hero =>
      val text = (if (p.withMarisa) content.text("marisa.hunt.collectorWithMarisa") + "\n\n" + content.text("marisa.hunt.marisaAdvice")
                  else content.text("marisa.hunt.collector")) +
        (if (MarisaQuest.canPayDebt(hero)) "" else "\n\n" + content.text("marisa.hunt.cantPay"))
      val pay = Option.when(MarisaQuest.canPayDebt(hero))(content.choice("PayCollector", "marisa.hunt.payLabel"))
      renderer.show(user, Screen(text, pay.toList :+ content.choice("FightCollector", "marisa.hunt.fightLabel")))
    }

  /** Заплатить долг Кельвина: с Марисой — «Спаситель Марисы». */
  private def payCollector(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- requireProgress(user)
      res  <- if (!MarisaQuest.canPayDebt(hero)) showCollector(user, renderer, p).as(StateType.MarisaHunt)
              else for {
                paid <- MarisaQuest.payDebt(heroDao, user.userId, hero)
                _    <- renderer.show(user, Screen(content.text("marisa.hunt.debtPaid"), Nil))
                _    <- ZIO.when(p.withMarisa)(MarisaQuest.grant(heroDao, content, user, paid, Achievement.MarisaSavior, renderer))
                _    <- close(user, paid, renderer)
              } yield StateType.GlobalMap
    } yield res

  /** Бой с коллектором: человек-вор второго уровня с прибавкой, зовётся
    * Коллектором. Добыча (500 серебра и 10 дублонов) возвращает сюда. */
  private def fightCollector(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- requireProgress(user)
      base  = MonsterGenerator.generateOfRaceAndRarity(MarisaQuest.CollectorLevel, MarisaQuest.CollectorRace, MarisaQuest.CollectorRarity)
      mob   = MarisaQuest.collector(base)
      pct  <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      battle = SoloPveBattle.from(mob, hero).withStartEnergy(pct)
                 .copy(story = Some(MarisaQuest.CollectorStory), customName = Some(content.text("marisa.hunt.collectorName")))
      routing = LootData(Nil, Nil, returnState = Some(StateType.MarisaHunt), eventData = Some(p.copy(step = Step.AfterFight).asJson))
      _    <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _    <- heroDao.writeSceneData(user.userId, routing.asJson)
      _    <- renderer.show(user, Screen(content.text("marisa.hunt.fightStart"), Nil))
    } yield StateType.Battle

  /** После победы: без Марисы — в город; с Марисой — она просит долг себе. */
  private def afterFight(user: User, renderer: Renderer, p: Progress): Task[Unit] =
    if (!p.withMarisa)
      getHero(user).flatMap(hero => renderer.show(user, Screen(content.text("marisa.hunt.collectorDown"), Nil)) *> close(user, hero, renderer))
    else
      getHero(user).flatMap { hero =>
        val canGive = MarisaQuest.canPayDebt(hero)
        renderer.show(user, Screen(
          content.text("marisa.hunt.collectorDownWithMarisa") + (if (canGive) "" else "\n\n" + content.text("marisa.hunt.cantPay")),
          Option.when(canGive)(content.choice("GiveMarisa", "marisa.hunt.giveLabel")).toList :+
            content.choice("KeepSilver", "marisa.hunt.keepLabel")))
      }

  private def giveMarisa(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      p    <- requireProgress(user)
      res  <- if (!MarisaQuest.canPayDebt(hero)) afterFight(user, renderer, p).as(StateType.MarisaHunt)
              else for {
                paid <- MarisaQuest.payDebt(heroDao, user.userId, hero)
                _    <- MarisaQuest.grant(heroDao, content, user, paid, Achievement.MarisaSavior, renderer)
                _    <- close(user, paid, renderer)
              } yield StateType.GlobalMap
    } yield res

  private def keepSilver(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- renderer.show(user, Screen(content.text("marisa.hunt.keptSilver"), Nil))
      _    <- MarisaQuest.grant(heroDao, content, user, hero, Achievement.Scoundrel, renderer)
      _    <- close(user, hero, renderer)
    } yield StateType.GlobalMap

  /** Закрыть задание: письмо и карта уходят, сцена чистится, в город. */
  private def close(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    for {
      line <- MarisaQuest.finish(heroDao, inventoryRepo, content, user.userId, hero)
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
      _    <- renderer.show(user, Screen(line, Nil))
    } yield ()

  // ── Хелперы ─────────────────────────────────────────────────────────────────

  private def readProgress(user: User): Task[Option[Progress]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))

  private def requireProgress(user: User): Task[Progress] =
    readProgress(user).flatMap(ZIO.fromOption(_)).orElseFail(new Throwable(s"No Marisa hunt for user ${user.userId}"))

  private def writeProgress(user: User, p: Progress): Task[Unit] =
    heroDao.writeSceneData(user.userId, p.asJson)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val m    = secs / 60
    val s    = secs % 60
    if (m > 0) s"${m}мин ${s}с" else s"${s}с"
  }

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MarisaHuntState {
  val HuntDurationMs: Long   = 10L * 60L * 1000L
  val HuntDoneAction: String = """{"action":"HuntDone"}"""

  object Step {
    val Road       = "road"
    val Collector  = "collector"
    val AfterFight = "afterFight"
  }

  /** Ход похода в `scene_data`: шаг, взял ли Марису, когда вышли. */
  final case class Progress(step: String, withMarisa: Boolean, startedAt: Option[Long] = None)
  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj(
      "marisaStep" -> p.step.asJson,
      "withMarisa" -> p.withMarisa.asJson,
      "startedAt"  -> p.startedAt.asJson
    )
    implicit val decoder: Decoder[Progress] = (c: HCursor) =>
      for {
        step  <- c.get[String]("marisaStep")
        withM <- c.getOrElse[Boolean]("withMarisa")(false)
        start <- c.getOrElse[Option[Long]]("startedAt")(None)
      } yield Progress(step, withM, start)
  }
}
