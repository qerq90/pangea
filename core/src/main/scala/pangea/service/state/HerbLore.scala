package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Hero, Knowledge, LoreData}
import pangea.model.item.{Item, MaterialKind, QuestItemKind}
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.InventoryRepository
import zio.{Random, Task, ZIO}

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

  /** Догадаться самому при странном цветке: о простых травах — интеллект ÷ 4
    * процентов, о редких — вдвое реже, интеллект ÷ 8. По книге — интеллект ÷ 2, и
    * каждая неудача прибавляет к следующему броску ещё 2 %. Не осилил — книгу
    * можно открыть снова через час. */
  val InsightIntDivisor: Int   = 4
  val InsightRareIntDivisor: Int = 8
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

  /** Травы из вещей — каждая вместе со своим видом: цена и прочее считаются по
    * виду, и доставать его из вещи второй раз не нужно. */
  def herbs(items: List[Item]): List[(Item, MaterialKind)] =
    items.flatMap(i => i.material.filter(_.isHerb).map(i -> _))

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

  /** О чём можно догадаться по странному цветку: о простых травах, пока их не
    * знаешь; зная их, — о редких (странным тогда бывает только редкий цветок).
    * Знаешь всё — догадываться не о чем. */
  def insightTarget(lore: LoreData): Option[Knowledge] =
    if (!lore.knows(Knowledge.FlowersRank1)) Some(Knowledge.FlowersRank1)
    else if (!lore.knows(Knowledge.FlowersRank2)) Some(Knowledge.FlowersRank2)
    else None

  /** Порог броска (1..100) на догадку по странному цветку: о простых травах —
    * интеллект ÷ 4, о редких — интеллект ÷ 8. */
  def insightChance(hero: Hero, nowMs: Long, knowledge: Knowledge = Knowledge.FlowersRank1): Long =
    hero.effectiveBaseStats(nowMs).int /
      (if (knowledge == Knowledge.FlowersRank2) InsightRareIntDivisor else InsightIntDivisor)

  /** Догадка по сорванному цветку — на поляне и в пещере одна и та же. Только
    * странный цветок и только когда есть о чём догадываться (иначе броска нет и
    * случайное число не тратится). Удача — знание «сам дошёл», купленный трактат о
    * нём тихо уходит из сумки, игроку — строка. Возвращает знания после. */
  def insight(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent, renderer: Renderer,
              user: User, hero: Hero, lore: LoreData, kind: MaterialKind, nowMs: Long): Task[LoreData] =
    insightTarget(lore).filter(_ => kind == MaterialKind.StrangeFlower) match {
      case None => ZIO.succeed(lore)
      case Some(knowledge) =>
        Random.nextIntBetween(1, 101).flatMap { roll =>
          if (roll > insightChance(hero, nowMs, knowledge)) ZIO.succeed(lore)
          else {
            val learned = lore.learn(knowledge, alone = true)
            for {
              _       <- writeLore(heroDao, user.userId, learned)
              // Купленный трактат об этом уходит из сумки тихо — герою о нём не напоминаем.
              settled <- settleBooks(heroDao, inventoryRepo, user.userId, hero, learned)
              _       <- renderer.show(user, Screen(
                           content.text(if (knowledge == Knowledge.FlowersRank2) "knowledge.insightRare" else "knowledge.insight") +
                             "\n" + content.format("knowledge.gained", "title" -> knowledge.title), Nil))
            } yield settled._1
          }
        }
    }

  /** Порог броска (1..100) на освоение трактата: интеллект ÷ 2 плюс 2 % за каждую
    * прошлую неудачу с этой книгой. */
  def readingChance(hero: Hero, nowMs: Long, failures: Int): Long =
    hero.effectiveBaseStats(nowMs).int / ReadingIntDivisor + failures.toLong * ReadingFailureBonusPct
}
