package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, QuestItemKind, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.Task

/** «Деревня Мурлоков» — всё, что нужно сюжету в разных сценах: отметить сотого
  * убитого, выдать карту, узнать, на каком шаге задание, собрать мобов налёта,
  * отобрать снаряжение для сдачи, выковать клинок, закрыть задание. Сам ход
  * задания — [[NpcQuest.Murloc]] в `npc_quests`; сцены —
  * [[pangea.service.state.states.murloc.MurlocElderState]] (старейшина в
  * лабиринте) и [[pangea.service.state.states.murloc.MurlocVillageState]]
  * (карта: налёт или помощь). У героя-мурлока свои реплики: `key` подставляет
  * вариант `<ключ>Kin`. */
object MurlocQuest {

  /** Ключ сюжетного боя — налёта на деревню (см. `SoloPveBattle.story`). В
    * отличие от коллектора, налёт награждается как обычный бой в лабиринте —
    * опыт и добыча с каждого павшего по уровню героя. */
  val RaidStory: String = "murlocRaid"

  /** Сложность на карте — гроб. */
  val MapDifficulty: Int = 10

  /** Строй налёта: десять мурлоков второго–третьего ранга; за строем ждут
    * двенадцать третьего–четвёртого и последним — пятого (Старый Мрачноглаз). */
  val RaidFront: Int                    = 10
  val RaidFrontRarities: List[Rarity]   = List(Rarity.Uncommon, Rarity.Rare)
  val RaidQueue: Int                    = 12
  val RaidQueueRarities: List[Rarity]   = List(Rarity.Rare, Rarity.Mythical)
  val RaidLastRarity: Rarity            = Rarity.Legendary

  /** Клинок старого мурлока — легендарное оружие ровно уровня героя. */
  val BladeName: String        = "Клинок старого мурлока"
  val BladeRarity: ItemRarity  = ItemRarity.Orange

  /** Герой той же крови — старейшина говорит с ним иначе. */
  def isKin(hero: Hero): Boolean = hero.race == Race.Murloc

  /** Ключ текста с поправкой на кровь героя: мурлоку — `<key>Kin`. */
  def key(hero: Hero, base: String): String = if (isKin(hero)) base + "Kin" else base

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Сотня набрана, старейшина ещё не вышел. */
  def elderPending(q: NpcQuests): Boolean = q.onStep(NpcQuest.Murloc, 1)

  /** Карта на руках: задание идёт (в том числе когда клинок ждёт в деревне). */
  def mapOnHands(q: NpcQuests): Boolean = q.onStep(NpcQuest.Murloc, 2) || q.onStep(NpcQuest.Murloc, 3)

  /** Двадцать сдано, клинок не влез в сумку и ждёт в деревне. */
  def rewardWaits(q: NpcQuests): Boolean = q.onStep(NpcQuest.Murloc, 3)

  /** Сколько снаряжения уже сдано. */
  def handed(q: NpcQuests): Long = q.of(NpcQuest.Murloc).counter

  /** Сотый убитый: задание на первый шаг («старейшина ждёт»), если оно ещё не
    * начато и не закрыто. Возвращает, ждёт ли старейшина ПОСЛЕ этого — и когда
    * отметили сейчас, и когда он ждал с прошлого боя. */
  def markPending(heroDao: HeroDao, userId: UserId, now: Long): Task[Boolean] =
    NpcQuestLog.modify(heroDao, userId) { q =>
      if (q.isTaken(NpcQuest.Murloc) || q.isDone(NpcQuest.Murloc)) q else q.take(NpcQuest.Murloc, now)
    }.map(elderPending)

  /** Старейшина ушёл: карта в сумку, задание на второй шаг. */
  def giveMap(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository, userId: UserId, hero: Hero): Task[Boolean] =
    QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.MurlocVillageMap) <*
      NpcQuestLog.modify(heroDao, userId)(_.update(NpcQuest.Murloc)(_.copy(step = 2)))

  /** Закрыть задание: карта уходит из сумки, задание — в выполненные. Возвращает
    * строку для игрока. */
  def finish(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent, userId: UserId, hero: Hero): Task[String] =
    QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.MurlocVillageMap)) *>
      NpcQuestLog.modify(heroDao, userId)(_.finish(NpcQuest.Murloc)).as(content.text("murlocVillage.questDone"))

  // ── Снаряжение для старейшины ───────────────────────────────────────────────

  /** Что старейшина берёт: оружие и нагрудники — из сумки (надетое он не трогает). */
  val GearTypes: Set[ItemType] = Set(ItemType.Weapon, ItemType.ChestPlate)

  def gear(items: List[Item]): List[Item] = items.filter(i => GearTypes.contains(i.itemType))

  /** Кнопки сдачи: по цветам и всё разом. Обе фиолетовые редкости — одна
    * кнопка, оранжевое (легендарное) — только через «всё». */
  sealed abstract class GearGroup(val action: String, val labelKey: String, val rarities: Set[ItemRarity]) {
    def matches(item: Item): Boolean = rarities.contains(item.rarity)
  }
  object GearGroup {
    case object Gray   extends GearGroup("GiveGray",   "murlocVillage.village.giveGray",   Set(ItemRarity.Gray, ItemRarity.White))
    case object Green  extends GearGroup("GiveGreen",  "murlocVillage.village.giveGreen",  Set(ItemRarity.Green))
    case object Blue   extends GearGroup("GiveBlue",   "murlocVillage.village.giveBlue",   Set(ItemRarity.Blue))
    case object Purple extends GearGroup("GivePurple", "murlocVillage.village.givePurple", Set(ItemRarity.Purple, ItemRarity.Violet))
    case object All    extends GearGroup("GiveAll",    "murlocVillage.village.giveAll",    ItemRarity.values.toSet)

    val values: List[GearGroup] = List(Gray, Green, Blue, Purple, All)

    def byAction(action: String): Option[GearGroup] = values.find(_.action == action)
  }

  /** Что из группы уйдёт старейшине: не больше, чем осталось до цели, сначала
    * похуже — по редкости, потом по уровню. */
  def pick(items: List[Item], group: GearGroup, remaining: Long): List[Item] =
    gear(items).filter(group.matches)
      .sortBy(i => (ItemRarity.values.indexOf(i.rarity), i.lvl))
      .take(remaining.max(0L).toInt)

  /** Клинок старого мурлока: легендарное оружие ровно уровня героя, со своим именем. */
  def blade(hero: Hero, rng: Rng): Item =
    ItemGenerator.createItemOfType(ItemType.Weapon, hero.lvl, BladeRarity, rng)._1.copy(name = BladeName)

  // ── Налёт ───────────────────────────────────────────────────────────────────

  /** Мурлоки деревни уровня героя: строй и очередь за ним. */
  def raidMonsters(lvl: Int, rng: Rng): (List[Monster], List[Monster], Rng) = {
    def roll(n: Int, pool: List[Rarity], r: Rng): (List[Monster], Rng) =
      (1 to n).foldLeft((List.empty[Monster], r)) { case ((acc, rr), _) =>
        val (rarity, next) = rr.pick(pool)
        (acc :+ MonsterGenerator.generateOfRaceAndRarity(lvl, Race.Murloc, rarity), next)
      }
    val (front, r1) = roll(RaidFront, RaidFrontRarities, rng)
    val (queue, r2) = roll(RaidQueue, RaidQueueRarities, r1)
    (front, queue :+ MonsterGenerator.generateOfRaceAndRarity(lvl, Race.Murloc, RaidLastRarity), r2)
  }

}
