package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.{Gem, Item, ItemDetails, ItemType, MaterialKind, Rarity, TrophyKind}

/** Чистое ядро крафта в кубе Азата. При «Активации» просчитываем рецепты от самого
 *  длинного к самому короткому; каждый рецепт применяется повторно, пока в пуле есть
 *  подходящие ингредиенты. Полученный предмет откладывается и в дальнейших рецептах
 *  НЕ участвует. Каждое успешное применение тратит 1 заряд куба.
 *
 *  Стартовые рецепты:
 *   - 3 камня одного вида и грейда → 1 камень того же вида грейдом выше;
 *   - 9 голов существ → «Левитирующая голова монстра»;
 *   - легендарный предмет + 2 мифрила → тот же слот на +1 уровень (имя сохраняется);
 *   - легендарный предмет + 1 мифрил → тот же слот того же уровня (характеристики
 *     пересчитываются заново). */
object CubeCraft {

  /** Итог активации: новое содержимое куба (остаток + результаты), число
   *  потраченных зарядов (= применённых рецептов) и итоговый Rng. */
  final case class Result(items: List[Item], chargesUsed: Int, rng: Rng) {
    def anyApplied: Boolean = chargesUsed > 0
  }

  private sealed trait Recipe {
    def size: Int
    /** Попытаться забрать из пула ингредиенты и произвести результат. */
    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)]
  }

  private def isHead(i: Item): Boolean = i.details match {
    case ItemDetails.Trophy(_, TrophyKind.Head) => true
    case _                                      => false
  }

  private def isMithril(i: Item): Boolean = i.material.contains(MaterialKind.Mithril)

  private def isLegendaryGear(i: Item): Boolean =
    i.rarity == Rarity.Orange && ItemType.equippable.contains(i.itemType)

  // 3 одинаковых камня (вид+грейд, грейд < макс) → 1 камень грейдом выше.
  private object GemUpgrade extends Recipe {
    val size = 3
    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val counts = pool.flatMap(_.gem).filter(_.grade < Gem.MaxGrade)
        .groupBy(g => (g.kind, g.grade)).view.mapValues(_.size).toMap
      counts.filter(_._2 >= 3).keys.toList
        .sortBy { case (kind, grade) => (kind.entryName, grade) }
        .headOption.map { case (kind, grade) =>
          val consumed = pool.filter(_.gem.contains(Gem(kind, grade))).take(3)
          (consumed, GemGenerator.item(kind, grade + 1), rng)
        }
    }
  }

  // 9 голов существ → «Левитирующая голова монстра».
  private object NineHeads extends Recipe {
    val size = 9
    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val heads = pool.filter(isHead)
      if (heads.size >= 9) Some((heads.take(9), MaterialGenerator.item(MaterialKind.LevitatingMonsterHead), rng))
      else None
    }
  }

  // Легендарный предмет + N мифрила → пересборка того же слота.
  private final case class LegendaryReforge(mithril: Int, levelDelta: Long, keepName: Boolean) extends Recipe {
    val size = 1 + mithril
    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val legendary = pool.find(isLegendaryGear)
      val mithrils  = pool.filter(isMithril)
      if (legendary.isDefined && mithrils.size >= mithril) {
        val leg           = legendary.get
        val consumed      = leg :: mithrils.take(mithril)
        val newLvl        = (leg.lvl + levelDelta).max(1L).min(150L)
        val (rebuilt, r2) = ItemGenerator.createItemOfType(leg.itemType, newLvl, Rarity.Orange, rng)
        val result        = if (keepName) rebuilt.withName(leg.name) else rebuilt
        Some((consumed, result, r2))
      } else None
    }
  }

  // От самого длинного рецепта к самому короткому.
  private val recipes: List[Recipe] = List(
    NineHeads,                                             // 9
    LegendaryReforge(mithril = 2, levelDelta = 1, keepName = true),  // 3
    GemUpgrade,                                            // 3
    LegendaryReforge(mithril = 1, levelDelta = 0, keepName = false)  // 2
  )

  def craft(items: List[Item], charges: Int, rng: Rng): Result = {
    var pool      = items
    var results   = List.empty[Item]
    var r         = rng
    var remaining = charges

    recipes.foreach { recipe =>
      var continue = true
      while (continue && remaining > 0) {
        recipe.tryMatch(pool, r) match {
          case Some((consumed, result, r2)) =>
            pool = removeEach(pool, consumed)
            results = results :+ result
            r = r2
            remaining -= 1
          case None => continue = false
        }
      }
    }
    Result(pool ++ results, charges - remaining, r)
  }

  // Удаляет каждый элемент `toRemove` из `pool` по первому совпадению.
  private def removeEach(pool: List[Item], toRemove: List[Item]): List[Item] =
    toRemove.foldLeft(pool) { (acc, item) =>
      acc.indexOf(item) match {
        case -1 => acc
        case i  => acc.patch(i, Nil, 1)
      }
    }
}
