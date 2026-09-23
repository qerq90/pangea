package pangea.generator

import pangea.domain.Rng
import pangea.generator.item.{CubeCraft, GemGenerator, MaterialGenerator}
import pangea.model.item._
import pangea.model.item.PassiveKind
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

  private def setItem(id: Long, itemType: ItemType, rarity: Rarity, set: ItemSet): Item =
    Item(id, "Вещь", 10L, rarity, itemType, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0, set = Some(set))

  private def dust(kind: MaterialKind, id: Long): Item =
    MaterialGenerator.item(kind).copy(id = id)

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

    test("оружие + 8 надколотых камней → божественное оружие того же уровня и редкости, под доп. слот") {
      val sword  = Item(1L, "🟣 Меч Дворянина", 37L, Rarity.Violet, ItemType.Weapon,
        attack = 100, accuracy = 10, energy = 0, armor = 0, defence = 0, evasion = 0)
      val result = CubeCraft.craft(sword :: List.fill(8)(gem(GemKind.Ruby, 1)), charges = 50, rng)
      val forged = result.items.head
      assertTrue(result.chargesUsed == 1 && result.items.size == 1) &&
      assertTrue(forged.name == "🟣 Топор Владыки Огня" && forged.itemType == ItemType.AdditionalWeapon) &&
      assertTrue(forged.lvl == 37L && forged.rarity == Rarity.Violet) &&
      assertTrue(forged.divine.exists(d => d.kind == DivineKind.FireLordAxe && d.charges == DivineKind.chargesFor(Rarity.Violet))) &&
      assertTrue(forged.attack == 0 && forged.accuracy == 0)
    },

    test("вид божественного оружия — по камням: семь рубинов против черепа дают рубиновую впятеро чаще, чужих видов нет") {
      val sword = Item(1L, "⚫ Меч", 5L, Rarity.Gray, ItemType.Weapon,
        attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      val pool  = sword :: (List.fill(7)(gem(GemKind.Ruby, 1)) :+ gem(GemKind.Skull, 1))
      val kinds = (1L to 400L).toList
        .flatMap(seed => CubeCraft.craft(pool, charges = 1, Rng(seed)).items.head.divine.map(_.kind))
      val ruby  = kinds.count(_ == DivineKind.FireLordAxe)
      val skull = kinds.count(_ == DivineKind.DarkLordSword)
      assertTrue(kinds.size == 400 && ruby + skull == 400) &&
      assertTrue(skull > 0 && ruby > skull * 3)
    },

    test("камни выше надколотых в ковку не идут, и без оружия божественного оружия не выйдет") {
      val sword   = Item(1L, "⚫ Меч", 5L, Rarity.Gray, ItemType.Weapon,
        attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      val damaged = CubeCraft.craft(sword :: List.fill(8)(gem(GemKind.Ruby, 2)), charges = 50, rng)
      val noSword = CubeCraft.craft(List.fill(8)(gem(GemKind.Skull, 1)), charges = 50, rng)
      assertTrue(damaged.items.forall(_.divine.isEmpty)) &&
      assertTrue(noSword.items.forall(_.divine.isEmpty))
    },

    test("божественное оружие куб не переплавляет: ни мифрил, ни ингредиент набора его не трогают") {
      val blade    = DivineKind.item(DivineKind.DarkLordSword, 40L, Rarity.Orange).copy(id = 9L)
      val reforged = CubeCraft.craft(List(blade, mithril(2L), mithril(3L)), charges = 50, rng)
      val infused  = CubeCraft.craft(List(blade, MaterialGenerator.item(MaterialKind.EverburningIron).copy(id = 4L)), charges = 50, rng)
      assertTrue(reforged.chargesUsed == 0 && reforged.items.contains(blade)) &&
      assertTrue(infused.chargesUsed == 0 && infused.items.contains(blade))
    },

    test("пять вещей с одной руной → большая руна этого узора; сами рунные камни в счёт не идут") {
      import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
      import pangea.model.skill.Skill
      def weapon(id: Long, skill: Skill): Item =
        Item(id, "Меч", 10L, Rarity.Blue, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
          armor = 0, defence = 0, evasion = 0, details = ItemDetails.Weapon(skill))
      def helm(id: Long, kind: PassiveKind): Item =
        Item(id, "Шлем", 10L, Rarity.Blue, ItemType.Helmet, attack = 0, accuracy = 0, energy = 0,
          armor = 5, defence = 0, evasion = 0, details = ItemDetails.Passive(kind))
      val five   = (1L to 5L).toList.map(weapon(_, Skill.CunningStrike))
      val folded = CubeCraft.craft(five, charges = 50, rng)
      val four   = CubeCraft.craft(five.take(4), charges = 50, rng)
      val mixed  = CubeCraft.craft(five.take(3) ++ List(helm(6L, PassiveKind.Healer), helm(7L, PassiveKind.Healer)), charges = 50, rng)
      val stones = CubeCraft.craft((11L to 15L).toList.map(i =>
                     RuneStone.item(Rune.Active(Skill.CunningStrike), RuneStoneSize.Small).copy(id = i)), charges = 50, rng)
      assertTrue(folded.chargesUsed == 1 && folded.items.size == 1) &&
      assertTrue(folded.items.head.runeStone.exists(d =>
        d.size == RuneStoneSize.Big && d.runeKey == Rune.Active(Skill.CunningStrike).key)) &&
      assertTrue(folded.items.head.name == "Большая руна Хитрого удара") &&
      assertTrue(four.chargesUsed == 0 && four.items.size == 4) &&                 // четырёх мало
      assertTrue(mixed.chargesUsed == 0 && mixed.items.size == 5) &&               // разные руны не смешиваются
      assertTrue(stones.chargesUsed == 0 && stones.items.size == 5)                // камни куб не складывает
    },

    test("9 голов существ → Левитирующая голова монстра") {
      val items  = (1 to 9).map(i => head(i.toLong)).toList
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.material.contains(MaterialKind.LevitatingMonsterHead))
    },

    test("3 рубиновые пыли → надколотый рубин") {
      val items  = (1 to 3).map(i => dust(MaterialKind.RubyDust, i.toLong)).toList
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.gem.contains(Gem(GemKind.Ruby, Gem.MinGrade)))
    },

    test("чёрный порошок собирается в надколотый череп, пыль разных видов — нет") {
      val powder = CubeCraft.craft((1 to 3).map(i => dust(MaterialKind.BlackPowder, i.toLong)).toList,
        charges = 50, rng)
      val mixed  = CubeCraft.craft(
        List(dust(MaterialKind.RubyDust, 1L), dust(MaterialKind.TopazDust, 2L), dust(MaterialKind.EmeraldDust, 3L)),
        charges = 50, rng)
      assertTrue(powder.items.head.gem.contains(Gem(GemKind.Skull, Gem.MinGrade))) &&
        // на три разные пыли рецепта нет — куб просто гудит
        assertTrue(mixed.chargesUsed == 0) &&
        assertTrue(mixed.items.size == 3)
    },

    test("двух пылей не хватает — нужен ровно комплект из трёх") {
      val result = CubeCraft.craft((1 to 2).map(i => dust(MaterialKind.TopazDust, i.toLong)).toList,
        charges = 50, rng)
      assertTrue(result.chargesUsed == 0) && assertTrue(result.items.size == 2)
    },

    test("3 вещи набора «Упырь» → кожа упыря, редкость вещей не важна") {
      val items = List(
        setItem(1L, ItemType.Helmet, Rarity.Blue,   ItemSet.Ghoul),
        setItem(2L, ItemType.Boots,  Rarity.Purple, ItemSet.Ghoul),
        setItem(3L, ItemType.Gloves, Rarity.Gray,   ItemSet.Ghoul))
      val result = CubeCraft.craft(items, charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.material.contains(MaterialKind.GhoulSkin))
    },

    test("каждый набор переплавляется в свой материал") {
      def salvage(set: ItemSet) = CubeCraft.craft(
        (1 to 3).map(i => setItem(i.toLong, ItemType.Helmet, Rarity.Blue, set)).toList,
        charges = 50, rng).items.headOption.flatMap(_.material)
      assertTrue(salvage(ItemSet.WildFlame).contains(MaterialKind.EverburningIron)) &&
        assertTrue(salvage(ItemSet.StoneGuard).contains(MaterialKind.MagicStone)) &&
        // три вещи «Охотника» — шкура Белого волка
        assertTrue(salvage(ItemSet.Hunter).contains(MaterialKind.WhiteWolfHide))
    },

    test("шкура Белого волка + вещь → та же вещь в наборе «Охотник»") {
      val helm = Item(1L, "🔵 Прочный Шлем Дворянина", 12L, Rarity.Blue, ItemType.Helmet,
        attack = 0, accuracy = 0, energy = 0, armor = 30, defence = 3, evasion = 0)
      val hide = MaterialGenerator.item(MaterialKind.WhiteWolfHide).copy(id = 2L)
      val result = CubeCraft.craft(List(helm, hide), charges = 50, rng)
      assertTrue(result.chargesUsed == 1) &&
        assertTrue(result.items.size == 1) &&
        assertTrue(result.items.head.set.contains(ItemSet.Hunter)) &&
        assertTrue(result.items.head.name.endsWith(ItemSet.Hunter.title)) &&
        assertTrue(result.items.head.armor == 30 && result.items.head.lvl == 12L && result.items.head.rarity == Rarity.Blue)
    },

    test("двух вещей набора мало, а вещи разных наборов не смешиваются") {
      val two = CubeCraft.craft(
        (1 to 2).map(i => setItem(i.toLong, ItemType.Helmet, Rarity.Blue, ItemSet.Ghoul)).toList,
        charges = 50, rng)
      val mixed = CubeCraft.craft(List(
        setItem(1L, ItemType.Helmet, Rarity.Blue, ItemSet.Ghoul),
        setItem(2L, ItemType.Boots,  Rarity.Blue, ItemSet.WildFlame),
        setItem(3L, ItemType.Gloves, Rarity.Blue, ItemSet.StoneGuard)), charges = 50, rng)
      assertTrue(two.chargesUsed == 0) && assertTrue(mixed.chargesUsed == 0)
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

    test("вещь + кожа упыря → та же вещь набора «Упырь»") {
      val axe = Item(1L, "🟣 Выдающийся Топор Дворянина", 30L, Rarity.Violet, ItemType.Weapon,
        attack = 42, accuracy = 7, energy = 3, armor = 0, defence = 2, evasion = 1)
      val skin   = MaterialGenerator.item(MaterialKind.GhoulSkin).copy(id = 2L)
      val result = CubeCraft.craft(List(axe, skin), charges = 50, rng)
      val made   = result.items.find(_.set.contains(ItemSet.Ghoul))
      assertTrue(result.chargesUsed == 1) &&
      assertTrue(made.isDefined) &&
      assertTrue(made.exists(i => i.attack == 42 && i.accuracy == 7 && i.energy == 3 &&
                                  i.defence == 2 && i.evasion == 1)) &&
      assertTrue(made.exists(i => i.lvl == 30L && i.rarity == Rarity.Violet && i.itemType == ItemType.Weapon)) &&
      assertTrue(made.exists(_.name.endsWith(ItemSet.Ghoul.title))) &&
      assertTrue(made.exists(!_.name.contains("Дворянина"))) &&
      assertTrue(!result.items.exists(_.material.contains(MaterialKind.GhoulSkin)))
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
