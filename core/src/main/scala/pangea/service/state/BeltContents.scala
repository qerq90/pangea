package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.item.{FlaskGenerator, MaterialGenerator}
import pangea.model.hero.HeroId
import pangea.model.item.{BeltSlot, BrewKind, Item, ItemDetails, MaterialKind, RoseKind}
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** Содержимое пояса принадлежит герою, а не самому поясу: с героя пояс никогда
  * не уходит с вещами внутри. Надели новый поверх старого — всё переезжает в
  * новый ([[swap]]); сняли — выкладывается в сумку отдельными вещами ([[strip]]).
  * Хранится оно по-прежнему в надетом поясе, поэтому бой, пополнение у Густаво
  * и «На пояс» ничего об этом не знают. */
object BeltContents {

  def slotsOf(belt: Item): List[BeltSlot] = belt.details match {
    case b: ItemDetails.Belt => b.slots
    case _                   => Nil
  }

  private def withSlots(belt: Item, slots: List[BeltSlot]): Item =
    belt.copy(details = ItemDetails.Belt(slots))

  /** Вещь из ячейки: фляга и роза — со своими зарядами, пыль, отвар и святая
    * вода — если не потрачены. Пустая ячейка помнит, чем была полна, но вернуть
    * из неё нечего — None. Пустая фляга остаётся флягой: её наполнят. */
  def item(slot: BeltSlot): Option[Item] = slot match {
    case f: BeltSlot.Flask =>
      val fresh = FlaskGenerator.item(f.kind, f.rarity)
      Some(fresh.copy(details = fresh.details match {
        case d: ItemDetails.Flask => d.copy(charges = f.charges)
        case other                => other
      }))
    case d: BeltSlot.Dust      => Option.unless(d.spent)(MaterialGenerator.item(d.kind))
    case b: BeltSlot.Brew      => Option.unless(b.spent)(BrewKind.item(b.kind))
    case h: BeltSlot.HolyWater => Option.unless(h.spent)(MaterialGenerator.item(MaterialKind.HolyWater))
    case r: BeltSlot.Rose =>
      val fresh = RoseKind.item(r.kind)
      Option.unless(r.withered)(fresh.copy(details = fresh.details match {
        case d: ItemDetails.Rose => d.copy(charges = r.charges)
        case other               => other
      }))
  }

  /** Итог смены пояса: новый — с содержимым, старый — пустой, `spilled` — что не
    * поместилось в новый и уходит в сумку вещами; `moved` — было ли что
    * перекладывать. */
  final case class Swap(next: Item, worn: Item, spilled: List[Item], moved: Boolean)

  /** Новый пояс `next` надевают вместо `worn`: содержимое переезжает — полные
    * ячейки первыми, пустые последними, следом то, что уже лежало в новом (пояса,
    * собранные до этого правила). Что не вошло по ёмкости нового — вещами в
    * сумку; лишние пустые ячейки просто пропадают. */
  def swap(worn: Item, next: Item): Swap = {
    val carried       = slotsOf(worn)
    val (full, spent) = (carried ++ slotsOf(next)).partition(_.uses > 0)
    val (kept, extra) = (full ++ spent).splitAt(ItemDetails.Belt.capacityFor(next.rarity))
    Swap(withSlots(next, kept), withSlots(worn, Nil), extra.flatMap(item), moved = carried.nonEmpty)
  }

  /** Пояс снимают: сам он — пустым, содержимое — вещами в сумку. */
  def strip(belt: Item): (Item, List[Item]) =
    (withSlots(belt, Nil), slotsOf(belt).flatMap(item))

  /** Сохранить вещи из ячеек, прежде чем класть их в сумку: у свежей вещи id
    * ещё нет (-1), а сумка различает вещи по id — две такие слились бы в одну, и
    * выбросив одну, игрок потерял бы обе. Без хранилища (старые тесты) — как есть. */
  def persist(itemRepo: Option[ItemRepository], heroId: HeroId, items: List[Item]): Task[List[Item]] =
    itemRepo.fold[Task[List[Item]]](ZIO.succeed(items))(repo => ZIO.foreach(items)(repo.persist(heroId, _)))

  /** Строка о переезде содержимого при смене пояса, с переводом строки впереди;
    * перекладывать было нечего — пусто. */
  def movedLine(content: SceneContent, moved: Boolean, spilled: List[Item]): String =
    if (!moved) ""
    else if (spilled.isEmpty) "\n" + content.text("inventory.beltMoved")
    else "\n" + content.format("inventory.beltMovedSpilled", "names" -> spilled.map(_.name).mkString(", "))
}
