package pangea.service.state.states.events.thieves

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
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

/** Засада в подворотне: кто вышел на дело и сколько их.
  *
  * Живёт в `scene_data`, поэтому кодеки рукописные. После драки сцена
  * возвращается сюда же через `eventData` экрана добычи — по ней вторая нода
  * и знает, сколько срезать кошельков.
  */
final case class ThievesScene(race: String, lvl: Long, count: Int)

object ThievesScene {
  implicit val encoder: Encoder[ThievesScene] = (s: ThievesScene) => Json.obj(
    "race" -> s.race.asJson, "lvl" -> s.lvl.asJson, "count" -> s.count.asJson)

  implicit val decoder: Decoder[ThievesScene] = (c: HCursor) =>
    for {
      race  <- c.get[String]("race")
      lvl   <- c.get[Long]("lvl")
      count <- c.getOrElse[Int]("count")(3)
    } yield ThievesScene(race, lvl, count)
}

/** «Разбойники в городе» — выездное задание с доски гильдии.
  *
  * Эффект-нода: герой уже досидел в подворотне до ночи, выбирать ему не из
  * чего — воры выходят сами, и отсюда сразу в [[StateType.Battle]]
  * (`autoAdvance`). В `scene_data` кладётся роутинг добычи: после победы —
  * к [[ThievesSpoilsState]], туда же уезжает сама сцена.
  *
  * Выходят трое-шестеро одной расы, уровнем в задание; звать со стороны им
  * некого — вся шайка уже здесь.
  */
case class ThievesState(heroDao: HeroDao, content: SceneContent) extends State {

  override def targetStates: Set[StateType] = Set(StateType.Battle)

  override def autoAdvance: Option[StateType] = Some(StateType.Battle)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      raw   <- heroDao.readSceneData(user.userId)
      scene <- ZIO.fromOption(raw.flatMap(_.as[ThievesScene].toOption))
                 .orElseFail(new Throwable(s"No thieves scene for user ${user.userId}"))
      race   = Race.withName(scene.race)
      lvl    = scene.lvl.max(1L).toInt
      seeds   <- ZIO.foreach(List.fill(scene.count)(()))(_ => Random.nextLong)
      monsters = seeds.map(seed => MonsterGenerator.generateOfRace(lvl, race, pangea.domain.Rng(seed))._1)
      energies <- ZIO.foreach(monsters)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      battle   = SoloPveBattle.fromGroup(monsters, hero, energies).copy(noKin = true)
      routing  = LootData(Nil, Nil,
                   returnState = Some(StateType.ThievesSpoils),
                   eventData   = Some(scene.asJson))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, routing.asJson)
      _ <- renderer.show(user, Screen(content.format("thieves.ambush",
             "count" -> scene.count.toString, "race" -> race.genitivePlural), Nil))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] = {
    val _ = (ua, renderer)
    ZIO.succeed(StateType.Battle)
  }

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

/** Эффект-нода после драки с ворами: кошелёк с каждого, отметка на доске и
  * обида банды. Кошельки уходят на общий экран добычи (`autoAdvance` →
  * [[StateType.Loot]]), а оттуда герой возвращается в город.
  *
  * Сцена приезжает сюда в `eventData` экрана добычи: бой её не теряет.
  */
case class ThievesSpoilsState(heroDao: HeroDao, content: SceneContent) extends State {

  override def targetStates: Set[StateType] = Set(StateType.Loot, StateType.GlobalMap)

  override def autoAdvance: Option[StateType] = Some(StateType.Loot)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[ThievesScene].toOption)).flatMap {
      // Сцены нет — драки будто и не было: отдавать нечего, но и бросать
      // героя в пустом экране добычи незачем.
      case None => heroDao.writeSceneData(user.userId,
        LootData(Nil, Nil, returnState = Some(StateType.GlobalMap)).asJson)
      case Some(scene) =>
        val race   = Race.withName(scene.race)
        val purses = List.fill(scene.count)(ThievesState.purse(scene.lvl))
        for {
          closed <- BoardProgress.markDone(heroDao, user.userId, BoardKind.Thieves)
          _      <- renderer.show(user, Screen(content.format("thieves.won",
                      "count" -> scene.count.toString, "gang" -> gangName(race)), Nil))
          _      <- ZIO.when(closed)(renderer.show(user, Screen(content.text("questBoard.doneThieves"), Nil)))
          // Уцелевшие запомнят, кто им помешал.
          after  <- Random.nextIntBetween(GangGrudge.MinExplores, GangGrudge.MaxExplores + 1)
          _      <- GangGrudge.remember(heroDao, user.userId, race, after)
          _      <- heroDao.writeSceneData(user.userId, LootData(
                      items = purses, silvers = Nil, returnState = Some(StateType.GlobalMap)).asJson)
        } yield ()
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] = {
    val _ = (user, ua, renderer)
    ZIO.succeed(StateType.Loot)
  }

  private def gangName(race: Race): String = content.text(s"thieves.gang.${race.entryName}")
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
