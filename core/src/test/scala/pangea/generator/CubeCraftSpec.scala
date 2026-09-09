package pangea.generator

import pangea.domain.Rng
import pangea.generator.item.{CubeCraft, GemGenerator, MaterialGenerator}
import pangea.model.item._
import zio.test._

object CubeCraftSpec extends ZIOSpecDefault {

  private def gem(kind: GemKind, grade: Int, id: Long = -1L): Item =
    GemGenerator.item(kind, grade).copy(id = id)

  private def head(id: Long): Item =
    Item(id, "Голова (Human)", 1L, Rarity.Gray, ItemType.Trophy,
      0, 0, 0, 0, 0, 0, details = ItemDetails.Trophy("Human", TrophyKind.Head))

  private def legendary(itemType: ItemType, lvl: Long, name: String, id: Long): Item =
    Item(id, name, lvl, Rarity.Orange, itemType, attack = 10, accuracy = 0, energy = 5,
      armor = 0, defence = 0, evasion = 0)

  private def mithril(id: Long): Item = MaterialGenerator.mithril.copy(id = id)

  private val rng = Rng(42L)

  override def spec = suite("CubeCraft")(

    test("9 надколотых рубинов → 3 повреждённых рубина (рецепт применён 3 раза)") {
      val items  = List.fill(9)(gem(GemKind.Ruby, 1))
      val result = CubeCraft.craft(items, charges = 50, rng)
      val gems   = result.items.flatMap(_.gem)
      assertTrue(result.chargesUsed == 3) &&
        assertTrue(result.items.size == 3) &&
        assertTrue(gems.forall(g => g.kind == GemKind.Ruby && g.grade == 2))
    },

    test("зарядов не хватает — применяется столько рецептов, сколько есть зарядов") {
      val items  = List.fill(9)(gem(GemKind.Ruby, 1))
      val result = CubeCraft.craft(items, charges = 2, rng)
      assertTrue(result.chargesUsed == 2) &&
        // 3 остатка грейда 1 + 2 результата грейда 2
        assertTrue(result.items.count(_.gem.exists(_.grade == 1)) == 3) &&
        assertTrue(result.items.count(_.gem.exists(_.grade == 2)) == 2)
    },

    test("9 голов существ → Левитирующая голова монстра") {
      val items  = (1 to 9).map(i => head(i.toLong)).toList
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.material.contains(MaterialKind.LevitatingMonsterHead))
    },

    test("нет подходящего рецепта — куб «гудит», ничего не меняется") {
      val items  = List(gem(GemKind.Ruby, 1, 1L), gem(GemKind.Sapphire, 1, 2L))
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 0) && assertTrue(!result.anyApplied) &&
        assertTrue(result.items.size == 2)
    },

    test("легендарка + 1 мифрил → тот же слот, тот же уровень, характеристики заново") {
      val items  = List(legendary(ItemType.Helmet, 5L, "Шлем лорда", 1L), mithril(2L))
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.itemType == ItemType.Helmet) &&
        assertTrue(result.items.head.rarity == Rarity.Orange) &&
        assertTrue(result.items.head.lvl == 5L)
    },

    test("легендарка + 2 мифрила → тот же слот на +1 уровень, имя сохраняется") {
      val items  = List(legendary(ItemType.Weapon, 5L, "Меч дворянина", 1L), mithril(2L), mithril(3L))
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.itemType == ItemType.Weapon) &&
        assertTrue(result.items.head.lvl == 6L) &&
        assertTrue(result.items.head.name == "Меч дворянина")
    },

    test("рецепт не переиспользует свой результат: 3 грейда, не 1 идеальный") {
      // 3 надколотых → 1 повреждённый; повреждённый (результат) НЕ идёт дальше.
      val items  = List.fill(3)(gem(GemKind.Topaz, 1))
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.gem.exists(_.grade == 2))
    },
    // ── Перевод вещи в набор ──────────────────────────────────────────────────
    test("вещь + вечно огненное железо → та же вещь набора «Дикое пламя»") {
      val axe = Item(1L, "🟣 Выдающийся Топор Дворянина", 30L, Rarity.Violet, ItemType.Weapon,
        attack = 42, accuracy = 7, energy = 3, armor = 0, defence = 2, evasion = 1)
      val iron = MaterialGenerator.item(MaterialKind.EverburningIron).copy(id = 2L)
      val result = CubeCraft.craft(List(axe, iron), charges = 50, rng)
      val made   = result.items.find(_.set.contains(ItemSet.WildFlame))
      assertTrue(result.chargesUsed == 1) &&
      assertTrue(made.isDefined) &&
      // характеристики, уровень, редкость и слот сохранены полностью
      assertTrue(made.exists(i => i.attack == 42 && i.accuracy == 7 && i.energy == 3 &&
                                  i.defence == 2 && i.evasion == 1)) &&
      assertTrue(made.exists(i => i.lvl == 30L && i.rarity == Rarity.Violet && i.itemType == ItemType.Weapon)) &&
      // третье слово названия уступило место имени набора
      assertTrue(made.exists(_.name.endsWith(ItemSet.WildFlame.title))) &&
      assertTrue(made.exists(!_.name.contains("Дворянина"))) &&
      // и вещь, и железо израсходованы
      assertTrue(!result.items.exists(_.material.contains(MaterialKind.EverburningIron))) &&
      assertTrue(result.items.size == 1)
    },

    test("вещь + магический камень → та же вещь набора «Каменный страж»") {
      val axe = Item(1L, "🟣 Выдающийся Топор Дворянина", 30L, Rarity.Violet, ItemType.Weapon,
        attack = 42, accuracy = 7, energy = 3, armor = 0, defence = 2, evasion = 1)
      val stone  = MaterialGenerator.item(MaterialKind.MagicStone).copy(id = 2L)
      val result = CubeCraft.craft(List(axe, stone), charges = 50, rng)
      val made   = result.items.find(_.set.contains(ItemSet.StoneGuard))
      assertTrue(result.chargesUsed == 1) &&
      assertTrue(made.isDefined) &&
      // характеристики, уровень, редкость и слот сохранены полностью
      assertTrue(made.exists(i => i.attack == 42 && i.accuracy == 7 && i.energy == 3 &&
                                  i.defence == 2 && i.evasion == 1)) &&
      assertTrue(made.exists(i => i.lvl == 30L && i.rarity == Rarity.Violet && i.itemType == ItemType.Weapon)) &&
      // третье слово названия уступило место имени набора
      assertTrue(made.exists(_.name.endsWith(ItemSet.StoneGuard.title))) &&
      assertTrue(made.exists(!_.name.contains("Дворянина"))) &&
      assertTrue(!result.items.exists(_.material.contains(MaterialKind.MagicStone))) &&
      assertTrue(result.items.size == 1)
    },

    test("без железа вещь в набор не переводится") {
      val axe = Item(1L, "🟣 Выдающийся Топор Дворянина", 30L, Rarity.Violet, ItemType.Weapon,
        attack = 42, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      val result = CubeCraft.craft(List(axe), charges = 50, rng)
      assertTrue(result.chargesUsed == 0) && assertTrue(result.items == List(axe))
    },

    test("железо не тратится впустую на вещь, которая уже в этом наборе") {
      val axe = Item(1L, "🟣 Выдающийся Топор Дикого пламени", 30L, Rarity.Violet, ItemType.Weapon,
        attack = 42, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        set = Some(ItemSet.WildFlame))
      val iron = MaterialGenerator.item(MaterialKind.EverburningIron).copy(id = 2L)
      val result = CubeCraft.craft(List(axe, iron), charges = 50, rng)
      assertTrue(result.chargesUsed == 0) &&
      assertTrue(result.items.exists(_.material.contains(MaterialKind.EverburningIron)))
    },

    test("ненадеваемое железом не переводится — камни и трофеи не годятся") {
      val iron = MaterialGenerator.item(MaterialKind.EverburningIron).copy(id = 2L)
      val result = CubeCraft.craft(List(gem(GemKind.Ruby, 1, id = 3L), head(4L), iron), charges = 50, rng)
      assertTrue(!result.items.exists(_.set.isDefined))
    }

  )
}
