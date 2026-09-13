package pangea.service.state.states.guild

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Hero, MasterHornBoosts}
import pangea.model.item.{ItemType, Rarity}
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{NpcQuestDialog, State, UserAction}
import zio.{Task, ZIO}

/**
 * Мастер Горн прокачивает характеристики (Броня, Уклонение, Атака, Защита,
 * Точность, Энергия, Вместимость инвентаря) за репутацию охотников И серебро
 * одновременно — обе валюты списываются в одинаковом числе. На каждой клавише —
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

  /** «Ржавая вилка»: набрать сто репутации трофеями — и одно улучшение даром.
    * Завязка смотрит на оружие героя. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Horn, "Horn")

  private val branch = new Branch(
    routes = Map(
      quest.questAction   -> Target.Run { (u, _, r) => questTalk(u, r) },
      quest.acceptAction  -> Target.Run { (u, _, r) => quest.accept(u, r) *> enter(u, r).as(StateType.MasterHorn) },
      quest.declineAction -> Target.Run { (u, _, r) => enter(u, r).as(StateType.MasterHorn) },
      "ImproveArmor"         -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Armor) },
      "ImproveEvasion"       -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Evasion) },
      "ImproveAttack"        -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Attack) },
      "ImproveDefence"       -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Defence) },
      "ImproveAccuracy"      -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Accuracy) },
      "ImproveEnergy"        -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Energy) },
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
      quest.load(user).flatMap { quests =>
        val base = content.screen("guild.masterHorn.menu")
        val (front, back) = base.choices.partition(_.id != "LeaveMasterHorn")
        renderer.show(user, base.copy(choices = front ++ quest.button(quests).toList ++ back))
      }

  // ── Задание Горна ──────────────────────────────────────────────────────────

  /** Кнопка задания: завязка по оружию героя, пока не взято; счёт репутации на
    * первом шаге; на втором — напоминание, что улучшение даром. */
  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- quest.load(user)
      p       = quests.of(NpcQuest.Horn)
      _ <- if (p.done) enter(user, renderer)
           else if (!p.taken) quest.offer(user, renderer, quest.format("intro", "verdict" -> verdict(hero)))
           else if (p.step == 1)
             renderer.show(user, Screen(quest.format("step1Fail", "rep" -> p.counter.toString), Nil)) *> enter(user, renderer)
           else renderer.show(user, Screen(quest.text("step2Hint"), Nil)) *> enter(user, renderer)
    } yield StateType.MasterHorn

  /** Что Горн думает об оружии героя: серое и белое — хлам, лучше — «хоть
    * что-то», без оружия — отдельная реплика. */
  private def verdict(hero: Hero): String = {
    val weapon = hero.equipment.weapon
    if (weapon.itemType == ItemType.NoItem) quest.text("verdictBareHands")
    else if (weapon.rarity == Rarity.Gray || weapon.rarity == Rarity.White) quest.format("verdictJunk", "weapon" -> weapon.name)
    else quest.format("verdictDecent", "weapon" -> weapon.name)
  }

  /** Улучшение даром — пока задание на втором шаге. */
  private def freeImprove(user: User): Task[Boolean] =
    quest.load(user).map(_.onStep(NpcQuest.Horn, 2))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Confirm-экран: запоминаем выбранный стат в scene_data и показываем «Вы
  // можете улучшить X на +1 за N очков» с двумя кнопками.
  private def askImprove(user: User, renderer: Renderer, stat: Stat): Task[StateType] =
    for {
      hero <- getHero(user)
      free <- freeImprove(user)
      _    <- heroDao.writeSceneData(user.userId, Json.obj(StatKey -> stat.entryName.asJson))
      text  = if (free) quest.format("confirmFree", "stat" -> stat.label, "step" -> stat.step.toString)
              else content.format("guild.masterHorn.confirm.text",
                     "stat" -> stat.label,
                     "step" -> stat.step.toString,
                     "cost" -> cost(hero, stat).toString)
      _    <- renderer.show(user, Screen(text, content.screen("guild.masterHorn.confirm").choices))
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
          freeImprove(user).flatMap { free =>
            if (free)
              // Задание Горна: этот раз — даром, и это его развязка.
              writeBoost(user, hero, stat, 0L) *> questFinish(user, hero, stat, renderer)
            else if (hero.guildReputation < price)
              renderer.show(user, Screen(
                content.format("guild.masterHorn.notEnough", "cost" -> price.toString), Nil)) *>
                askImprove(user, renderer, stat)
            else if (hero.silver < price)
              renderer.show(user, Screen(
                content.format("guild.masterHorn.notEnoughSilver", "cost" -> price.toString), Nil)) *>
                askImprove(user, renderer, stat)
            else
              applyBoost(user, hero, stat, price, renderer) *>
                askImprove(user, renderer, stat)
          }
      }
    } yield next

  /** Развязка задания Горна после бесплатного улучшения: рассказ и опыт. */
  private def questFinish(user: User, hero: Hero, stat: Stat, renderer: Renderer): Task[StateType] =
    for {
      done <- quest.complete(user, hero, identity)
      (_, expLine) = done
      weapon = hero.equipment.weapon
      closing = if (weapon.itemType == ItemType.NoItem) quest.text("closingBareHands")
                else quest.format("closingWeapon", "weapon" -> weapon.name)
      outro   = quest.format("outro", "closing" -> closing)
      _    <- renderer.show(user, Screen(outro, Nil))
      _    <- renderer.show(user, Screen(quest.format("reward", "stat" -> stat.label, "step" -> stat.step.toString, "exp" -> expLine), Nil))
      _    <- enter(user, renderer)
    } yield StateType.MasterHorn

  // Бусты Горна не пишутся в `fightStats` напрямую — они хранятся в
  // `masterHornBoosts` и прибавляются на чтение в `Hero.fightStatsWith` /
  // `maxArmor`. Единственное исключение — Inventory: вместимость живёт в
  // отдельном репозитории. Значение буста — накопленный прирост стата
  // (`stat.step` за прокачку), поэтому его можно прибавлять к статам напрямую.
  //
  // Цена списывается ОБЕИМИ валютами — репутацией и серебром — в одинаковом
  // числе (см. `cost`): прокачка стоит `price` очков репутации И `price` серебра.
  private def applyBoost(user: User, hero: Hero, stat: Stat, price: Long, renderer: Renderer): Task[Unit] = {
    val remainingRep    = hero.guildReputation - price
    val remainingSilver = hero.silver - price
    for {
      _ <- writeBoost(user, hero, stat, price)
      _ <- renderer.show(user, Screen(
        content.format("guild.masterHorn.applied",
          "stat" -> stat.label, "step" -> stat.step.toString,
          "cost" -> price.toString, "remaining" -> remainingRep.toString,
          "remainingSilver" -> remainingSilver.toString), Nil))
    } yield ()
  }

  /** Записи прокачки без сообщения: списание обеих валют, вместимость, буст. */
  private def writeBoost(user: User, hero: Hero, stat: Stat, price: Long): Task[Unit] =
    for {
      _ <- heroDao.updateGuildReputation(user.userId, hero.guildReputation - price)
      _ <- heroDao.updateSilver(user.userId, hero.silver - price)
      _ <- ZIO.when(stat == Stat.Inventory)(inventoryRepo.increaseCapacity(hero.id, stat.step).orElse(ZIO.unit))
      _ <- heroDao.updateMasterHornBoosts(user.userId, bumped(hero.masterHornBoosts, stat))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MasterHornState {
  import enumeratum._

  // `step` — на сколько единиц растёт характеристика за одну прокачку. Боевые
  // статы качаются по +3, а Энергия и Вместимость инвентаря — по +1.
  sealed abstract class Stat(val label: String, val step: Long) extends EnumEntry
  object Stat extends Enum[Stat] {
    val values = findValues
    case object Armor         extends Stat("Броня", 3)
    case object Evasion       extends Stat("Уклонение", 3)
    case object Attack        extends Stat("Атака", 3)
    case object Defence       extends Stat("Защита", 3)
    case object Accuracy      extends Stat("Точность", 3)
    case object Energy        extends Stat("Энергия", 1)
    case object Inventory     extends Stat("Вместимость инвентаря", 1)
  }

  /** Накопленный прирост стата от Мастера Горна (кратен `stat.step`). */
  def boostsFor(hero: Hero, stat: Stat): Long = boostsFor(hero.masterHornBoosts, stat)

  private def boostsFor(b: MasterHornBoosts, stat: Stat): Long = stat match {
    case Stat.Armor         => b.armor
    case Stat.Evasion       => b.evasion
    case Stat.Attack        => b.attack
    case Stat.Defence       => b.defence
    case Stat.Accuracy      => b.accuracy
    case Stat.Energy        => b.energy
    case Stat.Inventory     => b.inventory
  }

  private def bumped(b: MasterHornBoosts, stat: Stat): MasterHornBoosts = stat match {
    case Stat.Armor         => b.copy(armor         = b.armor     + stat.step)
    case Stat.Evasion       => b.copy(evasion       = b.evasion   + stat.step)
    case Stat.Attack        => b.copy(attack        = b.attack    + stat.step)
    case Stat.Defence       => b.copy(defence       = b.defence   + stat.step)
    case Stat.Accuracy      => b.copy(accuracy      = b.accuracy  + stat.step)
    case Stat.Energy        => b.copy(energy        = b.energy    + stat.step)
    case Stat.Inventory     => b.copy(inventory     = b.inventory + stat.step)
  }

  /**
   * Цена следующей прокачки в очках репутации:
   *   `a(n) = ceil(35 × 1.2^(n−1) − 30)`,
   * где `n` — порядковый номер следующей прокачки этого стата
   * (`boostsFor(hero, stat) / stat.step + 1`, так как буст хранит накопленный
   * прирост, а не число прокачек).
   * Первая прокачка стоит 5, далее каждая дороже. Минимум — 1.
   */
  def cost(hero: Hero, stat: Stat): Long = {
    val n     = boostsFor(hero, stat) / stat.step + 1
    val raw   = 35.0 * math.pow(1.2, (n - 1).toDouble) - 30.0
    math.ceil(raw).toLong.max(1L)
  }

  private val StatKey = "masterHornStat"
}
