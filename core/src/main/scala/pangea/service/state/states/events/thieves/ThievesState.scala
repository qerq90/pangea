package pangea.service.state.states.events.thieves

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.Race
import pangea.model.quest.BoardKind
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{BoardProgress, GangGrudge, State, UserAction}
import zio.{Random, Task, ZIO}

/** Засада в подворотне: кто вышел на дело и чем кончилось.
  *
  * Живёт в `scene_data`, поэтому кодеки рукописные. `fought` ставится, когда
  * герой уже сцепился с ворами, — вернувшись из боя победителем, он получает
  * кошельки, а объявление закрывается.
  */
final case class ThievesScene(race: String, lvl: Long, count: Int, fought: Boolean = false)

object ThievesScene {
  implicit val encoder: Encoder[ThievesScene] = (s: ThievesScene) => Json.obj(
    "race" -> s.race.asJson, "lvl" -> s.lvl.asJson,
    "count" -> s.count.asJson, "fought" -> s.fought.asJson)

  implicit val decoder: Decoder[ThievesScene] = (c: HCursor) =>
    for {
      race   <- c.get[String]("race")
      lvl    <- c.get[Long]("lvl")
      count  <- c.getOrElse[Int]("count")(3)
      fought <- c.getOrElse[Boolean]("fought")(false)
    } yield ThievesScene(race, lvl, count, fought)
}

/** «Разбойники в городе» — выездное задание с доски гильдии.
  *
  * Герой уходит из гильдии дорогой ([[pangea.service.state.states.road.QuestRoadState]])
  * и садится ждать в подворотне у таверны. Ближе к ночи выходят трое-шестеро
  * одной расы, уровнем в задание: драка начинается сама, выбирать не из чего.
  *
  * С каждого снятый кошелёк герой уносит с собой — что с ним делать, решит в
  * сумке. А банда этого не забудет: уцелевшие узнают, кто им помешал, и через
  * несколько вылазок в лабиринт пришлют за ним своего именного
  * (см. [[GangGrudge]]).
  */
case class ThievesState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map("ThievesFight" -> Target.Run { (u, _, r) => resume(u, r) }),
    fallback = Target.Run { (u, _, r) => resume(u, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Battle, StateType.Loot, StateType.GlobalMap, StateType.Thieves)

  override def enter(user: User, renderer: Renderer): Task[Unit] = resume(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Сцены нет — герой тут случайно, вернём в город. Не дрался — драка
    * начинается; вернулся из боя — забирает кошельки. */
  private def resume(user: User, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap {
      case None                       => home(user, renderer)
      case Some(s) if !s.fought       => ambush(user, s, renderer)
      case Some(s)                    => spoils(user, s, renderer)
    }

  /** Воры выходят из темноты. Разговаривать с ними не о чем. */
  private def ambush(user: User, scene: ThievesScene, renderer: Renderer): Task[StateType] =
    for {
      hero    <- getHero(user)
      race     = Race.withName(scene.race)
      lvl      = scene.lvl.max(1L).toInt
      seeds   <- ZIO.foreach(List.fill(scene.count)(()))(_ => Random.nextLong)
      monsters = seeds.map(seed => MonsterGenerator.generateOfRace(lvl, race, pangea.domain.Rng(seed))._1)
      energies <- ZIO.foreach(monsters)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      battle   = SoloPveBattle.fromGroup(monsters, hero, energies)
      // Воры в городе: звать со стороны им некого, вся шайка уже здесь.
      _ <- heroDao.writeActiveBattle(user.userId, battle.copy(noKin = true).asJson)
      _ <- heroDao.writeSceneData(user.userId, LootData(Nil, Nil,
             returnState = Some(StateType.Thieves),
             eventData   = Some(scene.copy(fought = true).asJson)).asJson)
      _ <- renderer.show(user, Screen(content.format("thieves.ambush",
             "count" -> scene.count.toString, "race" -> race.genitivePlural), Nil))
    } yield StateType.Battle

  /** Драка позади: кошельки по числу воров, отметка на доске и обида банды. */
  private def spoils(user: User, scene: ThievesScene, renderer: Renderer): Task[StateType] =
    for {
      race   <- ZIO.succeed(Race.withName(scene.race))
      purses  = List.fill(scene.count)(ThievesState.purse(scene.lvl))
      closed <- BoardProgress.markDone(heroDao, user.userId, BoardKind.Thieves)
      _      <- renderer.show(user, Screen(content.format("thieves.won",
                  "count" -> scene.count.toString, "gang" -> gangName(race)), Nil))
      _      <- ZIO.when(closed)(renderer.show(user, Screen(content.text("questBoard.doneThieves"), Nil)))
      // Уцелевшие запомнят, кто им помешал.
      after  <- Random.nextIntBetween(GangGrudge.MinExplores, GangGrudge.MaxExplores + 1)
      _      <- GangGrudge.remember(heroDao, user.userId, race, after)
      _      <- heroDao.writeSceneData(user.userId, LootData(
                  items = purses, silvers = Nil, returnState = Some(StateType.GlobalMap)).asJson)
    } yield StateType.Loot

  private def gangName(race: Race): String = content.text(s"thieves.gang.${race.entryName}")

  private def home(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("thieves.lost"), Nil)).as(StateType.GlobalMap)

  private def readScene(user: User): Task[Option[ThievesScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[ThievesScene].toOption))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object ThievesState {

  /** Сколько воров выходит на дело. */
  val MinThieves: Int = 3
  val MaxThieves: Int = 6

  /** Краденый кошелёк уровнем в того вора, с которого снят: от уровня считается
    * и благодарность гильдии, если кошелёк вернуть. */
  def purse(lvl: Long): Item = Item(
    id = -1L,
    name = MaterialKind.StolenPurse.displayName,
    lvl = lvl.max(1L),
    rarity = ItemRarity.Gray,
    itemType = ItemType.Material,
    attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
    details = ItemDetails.Material(MaterialKind.StolenPurse)
  )
}
