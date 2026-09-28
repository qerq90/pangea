package pangea.service.state.states.events

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CharacterMenu, State, UserAction}
import pangea.service.state.states.events.RaceRevengeState._
import zio.{Random, Task, ZIO}

/** Расплата: раса, которой герой проредил ряды, присылает за ним своих.
  *
  * Выходят трое: именной — тот самый легендарный, чьё имя знает вся раса, — и
  * двое старших при нём; в конце первого раунда старшие кличут родню, как
  * всегда делают мифические и легендарные. Уровень у них геройский, а не по
  * этажу: пришли именно за ним.
  *
  * Драться необязательно. Можно попробовать откреститься — мол, обознались, —
  * но верят редко ([[PersuadePct]]); поверят — разойдутся молча, не поверят —
  * начнётся то же самое, только без первого слова.
  *
  * Кто именно пришёл, лежит в `scene_data`: уход в «Персонаж» за снаряжением и
  * возврат показывают ту же сцену, а не начинают её заново. */
case class RaceRevengeState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "RevengeFight"    -> Target.Run { (u, _, r) => fight(u, r) },
      "RevengePersuade" -> Target.Run { (u, _, r) => persuade(u, r) },
      "OpenCharacter"   -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.RaceRevenge) }
    ),
    fallback = Target.Run { (u, _, r) => showCurrent(u, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.RaceRevenge)

  override def enter(user: User, renderer: Renderer): Task[Unit] = showCurrent(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Сцена уже выбрана — показываем её же; нет — выбираем, кто пришёл и как. */
  private def showCurrent(user: User, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap {
      case Some(scene) => show(user, scene, renderer)
      case None        => ZIO.succeed(StateType.Dungeon)
    }

  private def show(user: User, scene: RevengeScene, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      race  = Race.withName(scene.race)
      named = MonsterGenerator.generateOfRaceAndRarity(hero.lvl.toInt, race, Rarity.Legendary)
      lines = content.list("revenge.scenes")
      text  = lines(scene.scene % lines.size)
                .replace("{race}", race.toString)
                .replace("{name}", named.name)
      _ <- renderer.show(user, Screen(text, List(
             content.choice("RevengeFight", "revenge.fight").copy(color = ChoiceColor.Negative, row = Some(0)),
             content.choice("RevengePersuade", "revenge.persuade").copy(row = Some(1)),
             content.choice("OpenCharacter", "common.character").copy(row = Some(2)))))
    } yield StateType.RaceRevenge

  /** Попробовать откреститься. Верят редко, и второй попытки не будет: не
    * поверили — те же трое, тот же бой. */
  private def persuade(user: User, renderer: Renderer): Task[StateType] =
    withScene(user) { scene =>
      Random.nextIntBetween(1, 101).flatMap { roll =>
        if (roll <= PersuadePct)
          heroDao.writeSceneData(user.userId, Json.Null) *>
            renderer.show(user, Screen(content.format("revenge.believed",
              "race" -> Race.withName(scene.race).toString), Nil)).as(StateType.Dungeon)
        else
          renderer.show(user, Screen(content.text("revenge.notBelieved"), Nil)) *> start(user, scene, renderer)
      }
    }

  private def fight(user: User, renderer: Renderer): Task[StateType] =
    withScene(user)(scene => start(user, scene, renderer))

  /** Трое против одного: именной и двое старших при нём. Подкрепление они зовут
    * сами — как всякие мифические и легендарные (см. `BattleState.summons`). */
  private def start(user: User, scene: RevengeScene, renderer: Renderer): Task[StateType] =
    for {
      hero    <- getHero(user)
      race     = Race.withName(scene.race)
      lvl      = hero.lvl.toInt
      monsters = MonsterGenerator.generateOfRaceAndRarity(lvl, race, Rarity.Legendary) ::
                   List.fill(Elders)(MonsterGenerator.generateOfRaceAndRarity(lvl, race, Rarity.Mythical))
      energies <- ZIO.foreach(monsters)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      battle   = SoloPveBattle.fromGroup(monsters, hero, energies)
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, Json.Null)
      _ <- renderer.show(user, Screen(content.format("revenge.battle",
             "name" -> monsters.head.name, "race" -> race.toString), Nil))
    } yield StateType.Battle

  private def withScene(user: User)(f: RevengeScene => Task[StateType]): Task[StateType] =
    readScene(user).flatMap {
      case Some(scene) => f(scene)
      case None        => ZIO.succeed(StateType.Dungeon)
    }

  private def readScene(user: User): Task[Option[RevengeScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[RevengeScene].toOption))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object RaceRevengeState {

  /** Шанс (в %), что герою поверят и разойдутся без боя. */
  val PersuadePct: Int = 5

  /** Сколько старших приходит с именным. */
  val Elders: Int = 2

  /** Кто пришёл и каким текстом: сцена переживает уход в «Персонаж». Сколько
    * их сочтено, в сцене не хранится — вслух это число всё равно не называют. */
  final case class RevengeScene(race: String, scene: Int)

  object RevengeScene {
    implicit val encoder: Encoder[RevengeScene] = deriveEncoder
    implicit val decoder: Decoder[RevengeScene] = deriveDecoder
  }

  /** Монстры расплаты — для тех, кто собирает встречу снаружи (лабиринт). */
  def gather(race: Race, heroLvl: Long): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(heroLvl.toInt, race, Rarity.Legendary) ::
      List.fill(Elders)(MonsterGenerator.generateOfRaceAndRarity(heroLvl.toInt, race, Rarity.Mythical))
}
