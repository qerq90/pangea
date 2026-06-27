package pangea.service.state.states.guild

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Hero, MasterHornBoosts}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/**
 * Мастер Горн прокачивает шесть характеристик за репутацию: Броню, Уклонение,
 * Атаку, Защиту, Концентрацию, Вместимость инвентаря. На каждой клавише —
 * confirm-экран с ценой; формула цены — `a(n) = 35·1.2^(n−1) − 30`, округление
 * вверх, где `n` — порядковый номер следующего улучшения этой характеристики
 * для героя (см. `hero.masterHornBoosts`).
 */
case class MasterHornState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {
  import MasterHornState._

  private val branch = new Branch(
    routes = Map(
      "ImproveArmor"         -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Armor) },
      "ImproveEvasion"       -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Evasion) },
      "ImproveAttack"        -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Attack) },
      "ImproveDefence"       -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Defence) },
      "ImproveAccuracy"      -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Accuracy) },
      "ImproveConcentration" -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Concentration) },
      "ImproveInventory"     -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Inventory) },
      "ConfirmImprove"       -> Target.Run { (u, _, r) => confirmImprove(u, r) },
      "CancelImprove"        -> Target.Run { (u, _, r) => enter(u, r).as(StateType.MasterHorn) },
      "LeaveMasterHorn"      -> Target.Goto(StateType.TrainingHall)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.MasterHorn) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    // Сбрасываем выбор стата при возврате на главный экран — confirm-сценарий
    // считается завершённым (или прерванным CancelImprove).
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, content.screen("guild.masterHorn.menu"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Confirm-экран: запоминаем выбранный стат в scene_data и показываем «Вы
  // можете улучшить X на +1 за N очков» с двумя кнопками.
  private def askImprove(user: User, renderer: Renderer, stat: Stat): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- heroDao.writeSceneData(user.userId, Json.obj(StatKey -> stat.entryName.asJson))
      _    <- renderer.show(user, Screen(
                content.format("guild.masterHorn.confirm.text",
                  "stat" -> stat.label,
                  "cost" -> cost(hero, stat).toString),
                content.screen("guild.masterHorn.confirm").choices))
    } yield StateType.MasterHorn

  // Применяет прокачку: проверяет репутацию, списывает её, повышает
  // характеристику на +1 и инкрементирует счётчик прокачек этого стата.
  // После прокачки/неудачи остаёмся на confirm-экране того же стата — игрок может
  // продолжить вкачивать одну и ту же характеристику без возврата в меню.
  private def confirmImprove(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero      <- getHero(user)
      sceneData <- heroDao.readSceneData(user.userId)
      statOpt    = sceneData
                     .flatMap(_.hcursor.get[String](StatKey).toOption)
                     .flatMap(Stat.withNameOption)
      next <- statOpt match {
        case None =>
          // Sentinel: scene_data пустой — confirm-сценарий не начат. Возвращаемся в меню.
          renderer.show(user, Screen(content.text("guild.masterHorn.notReady"), Nil)) *>
            enter(user, renderer).as(StateType.MasterHorn)
        case Some(stat) =>
          val price = cost(hero, stat)
          if (hero.guildReputation < price)
            renderer.show(user, Screen(
              content.format("guild.masterHorn.notEnough", "cost" -> price.toString), Nil)) *>
              askImprove(user, renderer, stat)
          else
            applyBoost(user, hero, stat, price, renderer) *>
              askImprove(user, renderer, stat)
      }
    } yield next

  // Бусты Горна не пишутся в `fightStats` напрямую — они хранятся в
  // `masterHornBoosts` и прибавляются на чтение в `Hero.fightStatsWith` /
  // `maxArmor`. Единственное исключение — Inventory: вместимость живёт в
  // отдельном репозитории.
  private def applyBoost(user: User, hero: Hero, stat: Stat, price: Long, renderer: Renderer): Task[Unit] =
    for {
      _ <- heroDao.updateGuildReputation(user.userId, hero.guildReputation - price)
      _ <- ZIO.when(stat == Stat.Inventory)(inventoryRepo.increaseCapacity(hero.id, 1L).orElse(ZIO.unit))
      newBoosts = bumped(hero.masterHornBoosts, stat)
      _ <- heroDao.updateMasterHornBoosts(user.userId, newBoosts)
      _ <- renderer.show(user, Screen(
        content.format("guild.masterHorn.applied",
          "stat" -> stat.label, "cost" -> price.toString), Nil))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MasterHornState {
  import enumeratum._

  sealed abstract class Stat(val label: String) extends EnumEntry
  object Stat extends Enum[Stat] {
    val values = findValues
    case object Armor         extends Stat("Броня")
    case object Evasion       extends Stat("Уклонение")
    case object Attack        extends Stat("Атака")
    case object Defence       extends Stat("Защита")
    case object Accuracy      extends Stat("Точность")
    case object Concentration extends Stat("Концентрация")
    case object Inventory     extends Stat("Вместимость инвентаря")
  }

  /** Сколько раз герой уже улучшал этот стат у Мастера Горна. */
  def boostsFor(hero: Hero, stat: Stat): Long = boostsFor(hero.masterHornBoosts, stat)

  private def boostsFor(b: MasterHornBoosts, stat: Stat): Long = stat match {
    case Stat.Armor         => b.armor
    case Stat.Evasion       => b.evasion
    case Stat.Attack        => b.attack
    case Stat.Defence       => b.defence
    case Stat.Accuracy      => b.accuracy
    case Stat.Concentration => b.concentration
    case Stat.Inventory     => b.inventory
  }

  private def bumped(b: MasterHornBoosts, stat: Stat): MasterHornBoosts = stat match {
    case Stat.Armor         => b.copy(armor         = b.armor + 1)
    case Stat.Evasion       => b.copy(evasion       = b.evasion + 1)
    case Stat.Attack        => b.copy(attack        = b.attack + 1)
    case Stat.Defence       => b.copy(defence       = b.defence + 1)
    case Stat.Accuracy      => b.copy(accuracy      = b.accuracy + 1)
    case Stat.Concentration => b.copy(concentration = b.concentration + 1)
    case Stat.Inventory     => b.copy(inventory     = b.inventory + 1)
  }

  /**
   * Цена следующей прокачки в очках репутации:
   *   `a(n) = ceil(35 × 1.2^(n−1) − 30)`,
   * где `n = boostsFor(hero, stat) + 1`.
   * Первая прокачка стоит 5, далее каждая дороже. Минимум — 1.
   */
  def cost(hero: Hero, stat: Stat): Long = {
    val n     = boostsFor(hero, stat) + 1
    val raw   = 35.0 * math.pow(1.2, n - 1) - 30.0
    math.ceil(raw).toLong.max(1L)
  }

  private val StatKey = "masterHornStat"
}
