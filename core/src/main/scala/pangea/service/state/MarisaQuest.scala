package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent}
import pangea.generator.item.GemGenerator
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{GemKind, Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.NpcQuest
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.InventoryRepository
import pangea.service.purse.Purse
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Письмо Марисе» — всё, что нужно сюжету в разных сценах: выдать письмо и
  * карту, узнать, есть ли они у героя, закрыть задание (забрать оба), собрать
  * коллектора, выдать достижение. Сам ход задания — [[NpcQuest.Marisa]] в
  * `npc_quests`; сцены — инвентарь, Трактирщик, Жрец, [[pangea.service.state.states.marisa.MarisaSearchState]],
  * [[pangea.service.state.states.marisa.MarisaHuntState]]. */
object MarisaQuest {

  /** Сложность задания — три кости. */
  val LetterDifficulty: Int = 3

  /** Ключ сюжетного боя с коллектором (см. `SoloPveBattle.story`). */
  val CollectorStory: String = "collector"

  /** Что остаётся после коллектора — и больше ничего. */
  val CollectorSilver: Long    = 500L
  val CollectorDoubloons: Long = 10L

  /** Долг Кельвина с учётом прошедшего времени. */
  val DebtSilver: Long    = 14644L
  val DebtDoubloons: Long = 105L

  /** Смерть от коллектора: серебра уходит не меньше этого, дублонов — ровно столько. */
  val DeathSilverMin: Long   = 15000L
  val DeathDoubloons: Long   = 105L

  /** Тайник Кельвина. */
  val CacheSilver: Long       = 17000L
  val CacheDoubloons: Long    = 120L
  val CacheGemGrade: Int      = 2 // «Поврежденный»
  val CacheGems: List[GemKind] = List(GemKind.Ruby, GemKind.Emerald, GemKind.Topaz, GemKind.Sapphire, GemKind.Skull)
  val CacheGearLevel: Long    = 4L

  /** Коллектор: человек-вор второго уровня с +75 HP и +100 брони, зовётся Коллектором. */
  val CollectorLevel: Int       = 2
  val CollectorHpBonus: Long    = 75L
  val CollectorArmorBonus: Long = 100L

  def cacheGems: List[Item] = CacheGems.map(GemGenerator.item(_, CacheGemGrade))

  def collector(base: Monster): Monster =
    base.copy(fightStats = base.fightStats.copy(
      hp    = base.fightStats.hp + CollectorHpBonus,
      armor = base.fightStats.armor + CollectorArmorBonus))

  val CollectorRace: Race     = Race.Human
  val CollectorRarity: Rarity = Rarity.Uncommon

  /** Есть ли у героя такой сюжетный предмет (см. [[QuestSupport.hasItem]]). */
  def has(items: List[Item], kind: QuestItemKind): Boolean = QuestSupport.hasItem(items, kind)

  /** Положить сюжетный предмет, если его ещё нет (см. [[QuestSupport.giveItem]]). */
  def give(inventoryRepo: InventoryRepository, itemRepo: ItemRepository, hero: Hero, kind: QuestItemKind): Task[Boolean] =
    QuestSupport.giveItem(inventoryRepo, itemRepo, hero, kind)

  /** Пятидесятый убитый: письмо в сумку и задание на первый шаг. Если задание
    * уже начато или закрыто — ничего. */
  def giveLetter(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository, userId: UserId, hero: Hero): Task[Boolean] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      quests <- NpcQuestLog.load(heroDao, userId)
      fresh   = !quests.isTaken(NpcQuest.Marisa) && !quests.isDone(NpcQuest.Marisa)
      given  <- if (!fresh) ZIO.succeed(false)
                else give(inventoryRepo, itemRepo, hero, QuestItemKind.MarisaLetter) <*
                       NpcQuestLog.save(heroDao, userId, quests.take(NpcQuest.Marisa, now))
    } yield given

  /** Письмо вскрыто (или Мариса отдала карту): карта в сумке, `bonus` поднят. */
  def revealMap(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository, userId: UserId, hero: Hero): Task[Boolean] =
    give(inventoryRepo, itemRepo, hero, QuestItemKind.KelvinMap) <*
      NpcQuestLog.modify(heroDao, userId)(_.update(NpcQuest.Marisa)(_.copy(bonus = true)))

  /** Закрыть задание: письмо и карта уходят из сумки, задание — в выполненные.
    * Возвращает строку для игрока. */
  def finish(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent, userId: UserId, hero: Hero): Task[String] =
    QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.MarisaLetter, QuestItemKind.KelvinMap)) *>
      NpcQuestLog.modify(heroDao, userId)(_.finish(NpcQuest.Marisa)).as(content.text("marisa.questDone"))

  /** Достижение — один раз; строка о нём показывается сразу (см. [[QuestSupport.grant]]). */
  def grant(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, a: Achievement, renderer: Renderer): Task[Hero] =
    QuestSupport.grant(heroDao, content, user, hero, a, renderer)

  /** Хватает ли на долг Кельвина: серебро считается вместе с ячейкой в
    * Торговом доме, дублоны — только свои. */
  def canPayDebt(hero: Hero, silver: Long): Boolean = silver >= DebtSilver && hero.doubloons >= DebtDoubloons

  def payDebt(purse: Purse, userId: UserId, hero: Hero): Task[Hero] =
    for {
      charged <- purse.charge(userId, hero, DebtSilver)
      paid     = charged.getOrElse(hero).copy(doubloons = hero.doubloons - DebtDoubloons)
      _       <- purse.heroDao.updateDoubloons(userId, paid.doubloons)
    } yield paid
}
