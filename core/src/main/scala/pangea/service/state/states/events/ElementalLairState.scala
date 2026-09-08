package pangea.service.state.states.events

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.monster.{Elemental, Monster, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.{Random, Task, ZIO}

/**
 * «Логово Элементаля» — подход к минибоссу. Событие двухшаговое: сперва игрок
 * лишь чувствует присутствие существа, и только подойдя ближе видит, какой это
 * элементаль, и решает — драться или уйти.
 *
 * Выбранный вид элементаля хранится в `scene_data`, поэтому уход в «Персонаж»
 * (сменить снаряжение перед боем) и возврат оттуда показывают ту же сцену с тем
 * же элементалем, а не начинают событие заново.
 */
case class ElementalLairState(heroDao: HeroDao, content: SceneContent) extends State {
  import ElementalLairState._

  private val branch = new Branch(
    routes = Map(
      "ApproachElemental" -> Target.Run { (u, _, r) => approach(u, r) },
      "AttackElemental"   -> Target.Run { (u, _, r) => attack(u, r) },
      "OpenCharacter"     -> Target.Run { (u, _, _) =>
        CharacterMenu.open(heroDao, u.userId, StateType.ElementalLair)
      },
      "LeaveLair"         -> Target.Run { (u, _, r) => leave(u, r) }
    ),
    fallback = Target.Run { (u, _, r) => showCurrent(u, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.ElementalLair)

  override def enter(user: User, renderer: Renderer): Task[Unit] = showCurrent(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Экран по текущему состоянию сцены: пока элементаль не выбран — первый
    * экран, после подхода — он же с кнопками боя (сюда же возвращает «Назад» из
    * «Персонажа»). */
  private def showCurrent(user: User, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap {
      case Some(scene) => showElemental(user, scene.kind, firstMeeting = false, renderer)
      case None        => renderer.show(user, content.screen("elementalLair.enter")).as(StateType.ElementalLair)
    }

  private def approach(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      kind  <- scene.map(s => ZIO.succeed(s.kind)).getOrElse(randomElemental)
      lore  <- readLore(user)
      _     <- writeScene(user, LairScene(kind))
      // Первая встреча даёт особую реплику и открывает кнопку у трактирщика.
      _     <- ZIO.when(!lore.metElemental)(
                 heroDao.writeLoreData(user.userId, lore.copy(metElemental = true).asJson))
      res   <- showElemental(user, kind, firstMeeting = !lore.metElemental, renderer)
    } yield res

  private def showElemental(user: User, kind: String, firstMeeting: Boolean, renderer: Renderer): Task[StateType] = {
    val elemental = Elemental.byName(kind).getOrElse(Elemental.Fire)
    val text =
      content.format("elementalLair.approach", "elemental" -> elemental.label.toLowerCase) +
        (if (firstMeeting) "\n\n" + content.text("elementalLair.firstMeeting") else "")
    renderer.show(user, Screen(text, List(
      content.choice("AttackElemental", "elementalLair.attack").copy(row = Some(0)),
      content.choice("OpenCharacter", "common.character").copy(row = Some(1)),
      content.choice("LeaveLair", "elementalLair.leave").copy(row = Some(2))
    ))).as(StateType.ElementalLair)
  }

  private def attack(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- readScene(user)
      res <- scene match {
        case None => showCurrent(user, renderer)
        case Some(s) =>
          val elemental = Elemental.byName(s.kind).getOrElse(Elemental.Fire)
          val lvl       = Elemental.bossLvl(hero.lvl)
          val monster   = Monster(0L, lvl, Race.Elemental, Rarity.Legendary, elemental.stats(lvl))
          val battle    = SoloPveBattle.from(monster, hero).copy(elementalKind = Some(s.kind))
          heroDao.writeActiveBattle(user.userId, battle.asJson) *>
            // scene_data освобождаем: дальше им распоряжается бой и экран добычи.
            heroDao.writeSceneData(user.userId, Json.Null).as(StateType.Battle)
      }
    } yield res

  private def leave(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("elementalLair.left"), Nil)).as(StateType.Dungeon)

  private def randomElemental: Task[String] =
    Random.nextIntBounded(Elemental.values.size).map(Elemental.values(_).entryName)

  private def readScene(user: User): Task[Option[LairScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LairScene].toOption))

  private def writeScene(user: User, scene: LairScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def readLore(user: User): Task[LoreData] =
    heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object ElementalLairState {

  /** Какой элементаль выбран в этом логове (имя варианта [[Elemental]]). */
  final case class LairScene(kind: String)
  object LairScene {
    implicit val encoder: Encoder[LairScene] = deriveEncoder
    implicit val decoder: Decoder[LairScene] = deriveDecoder
  }
}
