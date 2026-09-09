package pangea.service.state.states.events

import io.circe.syntax.EncoderOps
import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.monster.{MiniBoss, Monster, Rarity}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.{Task, ZIO}

/**
 * Встреча с Гнилым Джо — минибоссом раскопок. Приходит из [[treasure.TreasureDigState]]:
 * вместо схрона игрок откапывает нечто гниющее и решает — драться, переодеться
 * или уйти. Отдельного «подхода», как в логове элементаля, тут нет: Джо уже
 * стоит перед ним.
 *
 * Первая встреча даёт особую реплику и открывает у трактирщика кнопку с его
 * историей (см. [[LoreData.metJoe]]).
 */
case class RottenJoeState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "AttackJoe"     -> Target.Run { (u, _, r) => attack(u, r) },
      "OpenCharacter" -> Target.Run { (u, _, _) =>
        CharacterMenu.open(heroDao, u.userId, StateType.RottenJoe)
      },
      "LeaveJoe"      -> Target.Run { (u, _, r) => leave(u, r) }
    ),
    fallback = Target.Run { (u, _, r) => show(u, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.RottenJoe)

  override def enter(user: User, renderer: Renderer): Task[Unit] = show(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Экран встречи. Первую реплику показываем отдельным сообщением и только один
    * раз за всю игру — дальше Джо уже знакомый. */
  private def show(user: User, renderer: Renderer): Task[StateType] =
    for {
      lore <- readLore(user)
      _    <- renderer.show(user, Screen(content.text("rottenJoe.enter"), Nil))
      _    <- ZIO.when(!lore.metJoe)(
                renderer.show(user, Screen(content.text("rottenJoe.firstMeeting"), Nil)) *>
                  heroDao.writeLoreData(user.userId, lore.copy(metJoe = true).asJson))
      _    <- renderer.show(user, Screen(content.text("rottenJoe.choose"), List(
                content.choice("AttackJoe", "rottenJoe.attack").copy(row = Some(0)),
                content.choice("OpenCharacter", "common.character").copy(row = Some(1)),
                content.choice("LeaveJoe", "rottenJoe.leave").copy(row = Some(2))
              )))
    } yield StateType.RottenJoe

  private def attack(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      lvl   = MiniBoss.bossLvl(hero.lvl)
      joe   = MiniBoss.RottenJoe
      monster = Monster(0L, lvl, joe.race, Rarity.Legendary, joe.stats(lvl))
      battle  = SoloPveBattle.from(monster, hero).copy(bossKind = Some(joe.entryName))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      // scene_data освобождаем: дальше им распоряжается бой и экран добычи.
      _ <- heroDao.writeSceneData(user.userId, Json.Null)
      _ <- renderer.show(user, Screen(content.text("rottenJoe.attackLine"), Nil))
    } yield StateType.Battle

  private def leave(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("rottenJoe.left"), Nil)).as(StateType.Dungeon)

  private def readLore(user: User): Task[LoreData] =
    heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
