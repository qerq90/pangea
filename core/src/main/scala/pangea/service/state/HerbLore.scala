package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.model.hero.{Hero, Knowledge, LoreData}
import pangea.model.item.{Item, MaterialKind, QuestItemKind}
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Травы и знания о них — общее для поляны, Густаво и инвентаря: что герой
  * узнаёт в сорванном цветке, почём Густаво берёт травы, как читается трактат. */
object HerbLore {

  /** Шанс редкой травы на поляне, в процентах; остальное — простые. */
  val RareHerbPct: Int = 2

  /** Цена травы у Густаво: множитель травы × это. «Странный цветок» — по цене сена. */
  val HerbPriceUnit: Long    = 10L
  val StrangeFlowerPrice: Long = 5L

  /** Трактаты: цена у Густаво и знание, которое они дают. */
  val Treatise1Price: Long = 15000L
  val Treatise2Price: Long = 30000L

  /** Догадаться самому при странном цветке: интеллект ÷ 4 процентов. По книге —
    * интеллект ÷ 2, и каждая неудача прибавляет к следующему броску ещё 2 %. Не
    * осилил — книгу можно открыть снова через час. */
  val InsightIntDivisor: Int   = 4
  val ReadingIntDivisor: Int   = 2
  val ReadingFailureBonusPct: Int = 2
  val ReadingCooldownMs: Long  = 60L * 60L * 1000L

  def knowledgeOf(book: QuestItemKind): Option[Knowledge] = book match {
    case QuestItemKind.FlowerTreatise1 => Some(Knowledge.FlowersRank1)
    case QuestItemKind.FlowerTreatise2 => Some(Knowledge.FlowersRank2)
    case _                             => None
  }

  def isTreatise(kind: QuestItemKind): Boolean = knowledgeOf(kind).isDefined

  /** Что герой видит в сорванной траве: её саму, если знает ранг, иначе — странный цветок. */
  def recognised(lore: LoreData, herb: MaterialKind): MaterialKind =
    if (Knowledge.forHerbRank(herb.herbRank).forall(lore.knows)) herb else MaterialKind.StrangeFlower

  def price(kind: MaterialKind): Long =
    if (kind == MaterialKind.StrangeFlower) StrangeFlowerPrice else kind.herbModifier * HerbPriceUnit

  def herbs(items: List[Item]): List[Item] = items.filter(_.material.exists(_.isHerb))

  def readLore(heroDao: HeroDao, userId: UserId): Task[LoreData] =
    heroDao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  def writeLore(heroDao: HeroDao, userId: UserId, lore: LoreData): Task[Unit] =
    heroDao.writeLoreData(userId, lore.asJson)

  /** Трактаты о том, что герой уже знает, ему больше не нужны — каким бы путём
    * знание ни пришло. Такие книги уходят из сумки, а их счёт неудач и срок
    * «переварить» стираются; итоговые знания пишутся, если что-то изменилось.
    * Возвращает записанные знания и выброшенные книги. */
  def settleBooks(
      heroDao: HeroDao,
      inventoryRepo: InventoryRepository,
      userId: UserId,
      hero: Hero,
      lore: LoreData
  ): Task[(LoreData, List[Item])] =
    for {
      inv     <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      obsolete = inv.items.data.filter(i => i.questItem.flatMap(knowledgeOf).exists(lore.knows))
      _       <- ZIO.foreachDiscard(obsolete)(i => inventoryRepo.removeItem(i.id, hero.id).mapError(e => new Throwable(e.toString)))
      // Книга в сумке — значит, куплена: Густаво это помнит и после того, как она ушла.
      settled  = obsolete.flatMap(_.questItem).foldLeft(lore)((l, k) => l.bookMastered(k.entryName).bookBought(k.entryName))
      _       <- ZIO.when(settled != lore)(writeLore(heroDao, userId, settled))
    } yield (settled, obsolete)

  /** Порог броска (1..100) на догадку по странному цветку. */
  def insightChance(hero: Hero, nowMs: Long): Long = hero.effectiveBaseStats(nowMs).int / InsightIntDivisor

  /** Порог броска (1..100) на освоение трактата: интеллект ÷ 2 плюс 2 % за каждую
    * прошлую неудачу с этой книгой. */
  def readingChance(hero: Hero, nowMs: Long, failures: Int): Long =
    hero.effectiveBaseStats(nowMs).int / ReadingIntDivisor + failures.toLong * ReadingFailureBonusPct
}
