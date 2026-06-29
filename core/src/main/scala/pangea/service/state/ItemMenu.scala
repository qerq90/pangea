package pangea.service.state

import pangea.engine.Choice
import pangea.model.item.Item

/**
 * Утилиты для экранов «список предметов кнопками» (бочка, инвентарь, снаряжение,
 * продажа у Ришелье). Один предмет = одна кнопка, по одной на ряд. Лейблы
 * автоматически режутся до лимита кнопки ВК.
 */
object ItemMenu {
  /** Жёсткий лимит лейбла кнопки в ВК. Что не влезло — обрезаем многоточием. */
  val MaxButtonLen: Int = 40

  /** Лимит ВК: до 10 рядов на клавиатуре. Последний ряд занят навигацией
   *  ([[NavRow]]) — туда вместе уходят «Назад», «Предыдущий», «Следующий», —
   *  значит под предметы остаётся 9 рядов, по одному предмету на ряд. */
  val DefaultPageSize: Int = 9

  /** Индекс ряда с навигационными кнопками: всегда последний из доступных. */
  val NavRow: Int = 9

  def truncate(s: String): String =
    if (s.length <= MaxButtonLen) s else s.take(MaxButtonLen - 1) + "…"

  /** Лейбл по умолчанию: `<имя> Ур.<уровень>`, обрезанный по лимиту. */
  def itemButtonLabel(item: Item): String =
    truncate(s"${item.name} Ур.${item.lvl}")

  /** Кнопки «по одной на ряд» начиная с `baseRow`. ID кнопки — `<prefix><id>`. */
  def itemButtons(items: List[Item], prefix: String, baseRow: Int = 0): List[Choice] =
    items.zipWithIndex.map { case (it, idx) =>
      Choice(s"$prefix${it.id}", itemButtonLabel(it), row = Some(baseRow + idx))
    }

  /** Срез страницы + итоговое число страниц. Page нормализуется в [0, total-1]. */
  def page[T](items: List[T], page: Int, pageSize: Int = DefaultPageSize): (List[T], Int, Int) = {
    val totalPages = ((items.size + pageSize - 1) / pageSize).max(1)
    val p          = page.max(0).min(totalPages - 1)
    (items.slice(p * pageSize, p * pageSize + pageSize), totalPages, p)
  }
}
