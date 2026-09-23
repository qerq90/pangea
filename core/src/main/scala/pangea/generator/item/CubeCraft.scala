package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.{BrewKind, BrewRates, Gem, GemKind, Item, ItemDetails, ItemSet, ItemType, MaterialKind, Rarity, DivineKind, TrophyKind}
import pangea.model.monster.MiniBoss
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}

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
 *     пересчитываются заново);
 *   - любая надеваемая вещь + ингредиент стихии → та же вещь из набора этой
 *     стихии (характеристики сохраняются, меняется только название и набор). */
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
    /** Сколько одинаковых результатов даёт одно применение (отвары идут по две порции). */
    def portions: Int = 1
  }

  private def isHead(i: Item): Boolean = i.details match {
    case ItemDetails.Trophy(_, TrophyKind.Head, _) => true
    case _                                      => false
  }

  private def isMithril(i: Item): Boolean = i.material.contains(MaterialKind.Mithril)

  private def isLegendaryGear(i: Item): Boolean =
    i.rarity == Rarity.Orange && ItemType.equippable.contains(i.itemType) && i.divine.isEmpty

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

  // 3 одинаковые пыли → надколотый камень того же вида (обратный ход ломке).
  private object DustAssembly extends Recipe {
    val size = 3
    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val byKind = pool.flatMap(i => i.material.flatMap(m => m.gem.map(_ -> i)))
        .groupBy(_._1).view.mapValues(_.map(_._2)).toMap
      byKind.filter(_._2.sizeIs >= 3).keys.toList
        .sortBy(_.entryName)
        .headOption.map { kind =>
          (byKind(kind).take(3), GemGenerator.item(kind, Gem.MinGrade), rng)
        }
    }
  }

  // Оружие + 8 надколотых камней → божественное оружие (носится в доп. слоте,
  // как и прочие вещи этого слота). Камни могут быть разных видов: вид оружия
  // роллится по их числу — семь рубинов против одного черепа дают рубиновый
  // вариант в семь раз чаще. Уровень и редкость — от оружия, ушедшего в ковку.
  private object DivineForge extends Recipe {
    /** Сколько надколотых камней уходит в ковку. */
    val Gems: Int = 8
    val size: Int = 1 + Gems

    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val cracked = pool.filter(_.gem.exists(_.grade == Gem.MinGrade))
      pool.find(_.itemType == ItemType.Weapon) match {
        case Some(weapon) if cracked.sizeIs >= Gems =>
          val taken      = cracked.take(Gems)
          val (kind, r2) = pickKind(taken.flatMap(_.gem).map(_.kind), rng)
          Some((weapon :: taken, DivineKind.item(DivineKind.of(kind), weapon.lvl, weapon.rarity), r2))
        case _ => None
      }
    }

    /** Вид камня по весу: шанс каждого — его доля среди восьми. Порядок камней
      * в куче не важен, поэтому тянем из отсортированного списка. */
    private def pickKind(kinds: List[GemKind], rng: Rng): (GemKind, Rng) = {
      val sorted     = kinds.sortBy(_.entryName)
      val (roll, r2) = rng.between(0L, sorted.size.toLong)
      (sorted(roll.toInt.max(0).min(sorted.size - 1)), r2)
    }
  }

  // Пять вещей с одной руной → большая руна этого узора. Сами рунные камни в
  // счёт не идут: пять малых складываются в сумке, а большие переплавлять
  // незачем.
  private object RuneFold extends Recipe {
    val size: Int = RuneStone.PiecesPerBig

    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val byRune = pool.filter(i => i.runeStone.isEmpty && ItemType.equippable.contains(i.itemType))
        .flatMap(i => Rune.of(i).map(_ -> i))
        .groupBy(_._1).view.mapValues(_.map(_._2)).toMap
      byRune.collect { case (rune, its) if its.sizeIs >= size => rune }
        .toList.sortBy(_.key)
        .headOption
        .map(rune => (byRune(rune).take(size), RuneStone.item(rune, RuneStoneSize.Big), rng))
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

  // Любая надеваемая вещь + ингредиент стихии → та же вещь, но из набора этой
  // стихии. Характеристики, уровень и редкость сохраняются полностью — меняются
  // только принадлежность к набору и третье слово названия (титул уступает место
  // имени набора): «Выдающийся Топор Дворянина» → «Выдающийся Топор Дикого пламени».
  private object SetInfusion extends Recipe {
    val size = 2

    /** Ингредиент → набор, в который он переводит вещь. */
    private def setOf(i: Item): Option[ItemSet] =
      i.material.flatMap(m => MiniBoss.values.find(_.ingredient == m).map(_.set))

    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] =
      for {
        ingredient <- pool.find(i => setOf(i).isDefined)
        set        <- setOf(ingredient)
        // Вещь, которой этот набор ещё не присвоен: иначе рецепт крутился бы
        // впустую, тратя заряды на переименование в тот же самый набор.
        // Божественное оружие набор не берёт: перековывать его не во что, а
        // переименование стёрло бы и вид, и заряды.
        target     <- pool.find(i =>
                        i != ingredient && ItemType.equippable.contains(i.itemType) && !i.set.contains(set) && i.divine.isEmpty)
      } yield {
        val (name, r2) = ItemNameGenerator.setName(target.itemType, target.rarity, set, rng)
        (List(target, ingredient), target.copy(name = name, set = Some(set)), r2)
      }
  }

  // 3 вещи одного набора → ингредиент этого набора. Редкость вещей не важна:
  // в переплавку одинаково идут и синие, и фиолетовые. Так добывается и шкура
  // Белого волка — с самого волка её падает не больше одной.
  private object SetSalvage extends Recipe {
    val size = 3

    private def materialOf(set: ItemSet): Option[MaterialKind] =
      MiniBoss.values.find(_.set == set).map(_.ingredient)

    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val bySet = pool
        .filter(i => ItemType.equippable.contains(i.itemType))
        .flatMap(i => i.set.map(_ -> i))
        .groupBy(_._1).view.mapValues(_.map(_._2)).toMap
      bySet.collect { case (set, items) if items.sizeIs >= 3 => set }
        .toList.sortBy(_.entryName)
        .flatMap(set => materialOf(set).map(set -> _))
        .headOption
        .map { case (set, material) => (bySet(set).take(3), MaterialGenerator.item(material), rng) }
    }
  }

  // Три травы первого ранга по рецепту (см. BrewKind) → отвар. Рецепты делят
  // травы между собой, поэтому жадный «первый подходящий» сварил бы три
  // костоправных из трав, сложенных под три разных отвара. Вместо этого по
  // травам в кубе строится план: наибольшее число отваров, при равенстве — как
  // можно больше разных; каждое применение забирает первый шаг плана, а остаток
  // пула на следующем шаге даёт тот же план без него.
  private object HerbBrew extends Recipe {
    val size = 3
    override def portions: Int = BrewRates.Portions
    def tryMatch(pool: List[Item], rng: Rng): Option[(List[Item], Item, Rng)] = {
      val herbs  = pool.flatMap(i => i.material.filter(_.isHerb))
      val counts = herbs.groupBy(identity).view.mapValues(_.size).toMap
      bestPlan(counts).headOption.map { kind =>
        val consumed = kind.recipe.foldLeft(List.empty[Item]) { (acc, herb) =>
          acc :+ pool.find(i => i.material.contains(herb) && !acc.contains(i)).get
        }
        (consumed, BrewKind.item(kind), rng)
      }
    }

    /** Лучший план варки из этих трав: больше отваров, при равенстве — больше
      * разных видов, при равенстве — по порядку BrewKind. Пул мал, состояний по
      * счётчикам трав немного — поиск с памятью. */
    private def bestPlan(counts: Map[MaterialKind, Int]): List[BrewKind] = {
      val memo = scala.collection.mutable.Map.empty[Map[MaterialKind, Int], List[BrewKind]]
      def better(a: List[BrewKind], b: List[BrewKind]): Boolean =
        a.size > b.size || (a.size == b.size && a.distinct.size > b.distinct.size)
      def go(c: Map[MaterialKind, Int]): List[BrewKind] = memo.getOrElseUpdate(c, {
        BrewKind.values.toList.foldLeft(List.empty[BrewKind]) { (best, kind) =>
          val need = kind.recipe.groupBy(identity).view.mapValues(_.size).toMap
          if (!need.forall { case (h, n) => c.getOrElse(h, 0) >= n }) best
          else {
            val rest = need.foldLeft(c) { case (m, (h, n)) => m.updated(h, m(h) - n) }.filter(_._2 > 0)
            val plan = kind :: go(rest)
            if (better(plan, best)) plan else best
          }
        }
      })
      go(counts)
    }
  }

  // От самого длинного рецепта к самому короткому.
  private val recipes: List[Recipe] = List(
    DivineForge,                                            // 9
    NineHeads,                                             // 9
    RuneFold,                                              // 5
    LegendaryReforge(mithril = 2, levelDelta = 1, keepName = true),  // 3
    GemUpgrade,                                            // 3
    DustAssembly,                                          // 3
    SetSalvage,                                            // 3
    HerbBrew,                                              // 3
    LegendaryReforge(mithril = 1, levelDelta = 0, keepName = false), // 2
    SetInfusion                                            // 2
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
            results = results ++ List.fill(recipe.portions)(result)
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
