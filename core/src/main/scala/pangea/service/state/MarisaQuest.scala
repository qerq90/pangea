package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.item.GemGenerator
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{GemKind, Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.NpcQuest
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Письмо Марисе» — всё, что нужно сюжету в разных сценах: выдать письмо и
  * карту, узнать, есть ли они у героя, закрыть задание (забрать оба), собрать
  * коллектора, выдать достижение. Сам ход задания — [[NpcQuest.Marisa]] в
  * `npc_quests`; сцены — инвентарь, Трактирщик, Жрец, [[pangea.service.state.states.marisa.MarisaSearchState]],
  * [[pangea.service.state.states.marisa.MarisaHuntState]]. */
object MarisaQuest {

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

  /** Есть ли у героя такой сюжетный предмет. */
  def has(items: List[Item], kind: QuestItemKind): Boolean = items.exists(_.questItem.contains(kind))

  /** Положить сюжетный предмет, если его ещё нет. Возвращает, положили ли. */
  def give(inventoryRepo: InventoryRepository, itemRepo: ItemRepository, hero: Hero, kind: QuestItemKind): Task[Boolean] =
    for {
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      given <- if (has(inv.items.data, kind)) ZIO.succeed(false)
               else itemRepo.persist(hero.id, QuestItemKind.item(kind))
                      .flatMap(it => inventoryRepo.addItem(hero.id, it).mapError(e => new Throwable(e.toString)))
                      .as(true)
    } yield given

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
    for {
      inv <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      ids  = inv.items.data.filter(i => i.questItem.exists(k => k == QuestItemKind.MarisaLetter || k == QuestItemKind.KelvinMap)).map(_.id).toSet
      _   <- ZIO.when(ids.nonEmpty)(inventoryRepo.removeItems(ids, hero.id).mapError(e => new Throwable(e.toString)))
      _   <- NpcQuestLog.modify(heroDao, userId)(_.finish(NpcQuest.Marisa))
    } yield content.text("marisa.questDone")

  /** Достижение — один раз; строка о нём показывается сразу. */
  def grant(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, a: Achievement, renderer: Renderer): Task[Hero] =
    if (hero.hasAchievement(a)) ZIO.succeed(hero)
    else {
      val updated = hero.withAchievement(a)
      heroDao.updateAchievements(user.userId, updated.achievements) *>
        renderer.show(user, Screen(content.format("marisa.achievement", "title" -> a.title, "bonus" -> a.bonusLine), Nil))
          .as(updated)
    }

  /** Хватает ли на долг Кельвина. */
  def canPayDebt(hero: Hero): Boolean = hero.silver >= DebtSilver && hero.doubloons >= DebtDoubloons

  def payDebt(heroDao: HeroDao, userId: UserId, hero: Hero): Task[Hero] = {
    val paid = hero.copy(silver = hero.silver - DebtSilver, doubloons = hero.doubloons - DebtDoubloons)
    heroDao.updateSilver(userId, paid.silver) *> heroDao.updateDoubloons(userId, paid.doubloons).as(paid)
  }
}
