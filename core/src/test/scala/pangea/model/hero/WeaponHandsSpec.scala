package pangea.model.hero

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.inventory.Inventory
import pangea.model.item.{Grip, Item, ItemDetails, ItemSet, ItemType, Rarity}
import pangea.model.skill.Skill
import pangea.model.user.UserId
import pangea.service.state.states.InventoryState
import pangea.test.TestFixtures
import zio.test._

/** Две руки: одноручное оружие несёт 60% статов и встаёт в свободную руку,
  * двуручное — все сто и занимает обе. */
object WeaponHandsSpec extends ZIOSpecDefault {

  private val hero  = TestFixtures.hero(UserId(1L))
  private val empty = TestFixtures.emptyEquipment

  private def weapon(id: Long, grip: Grip, atk: Long = 10L): Item =
    Item(id, s"Оружие $id", 1L, Rarity.Blue, ItemType.Weapon,
      attack = atk, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(grip))

  private def armed(right: Item, left: Item = Item.NoItem): Hero =
    hero.copy(
      equipment  = empty.copy(weapon = right, leftHand = left),
      fightStats = hero.fightStats.copy(atk = hero.fightStats.atk + right.attack + left.attack))

  private def generated(seed: Long, grip: Option[Grip] = None): Item =
    ItemGenerator.createItemOfType(ItemType.Weapon, 30L, Rarity.Blue, Rng(seed), grip)._1

  override def spec = suite("Две руки")(

    test("одноручное несёт 60% статов того же оружия, двуручное — все сто") {
      val pairs = (1L to 300L).map(s => generated(s, Some(Grip.OneHanded)) -> generated(s, Some(Grip.TwoHanded)))
      def cut(v: Long): Long = v * 60L / 100L
      assertTrue(pairs.forall { case (one, two) =>
        one.attack == cut(two.attack) && one.accuracy == cut(two.accuracy) && one.energy == cut(two.energy) &&
          one.armor == cut(two.armor) && one.defence == cut(two.defence) && one.evasion == cut(two.evasion)
      }) &&
      assertTrue(pairs.forall { case (one, two) => !one.twoHanded && two.twoHanded }) &&
      // атака у оружия есть всегда — значит, режется и она
      assertTrue(pairs.forall { case (one, two) => one.attack > 0L && one.attack < two.attack })
    },

    test("хват выпадает поровну; двуручное зовётся двуручным, а двуручного кинжала нет") {
      val weapons = (1L to 2000L).map(s => generated(s))
      val two     = weapons.filter(_.twoHanded)
      val share   = two.size.toDouble / weapons.size
      // «🔵 Хороший Двуручный Меч Рыцаря»: хват — третьим словом, сразу перед оружием
      val wordThird = two.forall(_.name.split(" ")(2).startsWith("Двуручн"))
      val noDagger  = two.forall(!_.name.contains("Кинжал"))
      val spear     = two.exists(_.name.contains("Двуручное Копьё"))
      val oneClean  = weapons.filterNot(_.twoHanded).forall(!_.name.contains("Двуруч"))
      assertTrue(share > 0.45 && share < 0.55) &&
      assertTrue(weapons.forall(_.grip.isDefined)) &&
      assertTrue(wordThird && noDagger && spear && oneClean)
    },

    test("одноручное встаёт в свободную руку — сперва правую, потом левую; статы рук складываются") {
      val a = weapon(1L, Grip.OneHanded, atk = 10L)
      val b = weapon(2L, Grip.OneHanded, atk = 7L)
      val (eq1, f1, out1) = InventoryState.equip(hero, a, None)
      val (eq2, f2, out2) = InventoryState.equip(hero.copy(equipment = eq1, fightStats = f1), b, None)
      assertTrue(eq1.weapon == a && eq1.leftHand.itemType == ItemType.NoItem && out1.isEmpty) &&
      assertTrue(eq2.weapon == a && eq2.leftHand == b && out2.isEmpty) &&
      assertTrue(f2.atk == hero.fightStats.atk + 17L) &&
      assertTrue(eq2.handsFull && !eq2.unarmed)
    },

    test("обе руки заняты: меняется выбранная рука") {
      val a = weapon(1L, Grip.OneHanded)
      val b = weapon(2L, Grip.OneHanded)
      val c = weapon(3L, Grip.OneHanded)
      val (left, _, outL)  = InventoryState.equip(armed(a, b), c, Some(2))
      val (right, _, outR) = InventoryState.equip(armed(a, b), c, Some(1))
      assertTrue(left.weapon == a && left.leftHand == c && outL == List(b)) &&
      assertTrue(right.weapon == c && right.leftHand == b && outR == List(a))
    },

    test("двуручное снимает оба одноручных — и статы обоих уходят") {
      val a   = weapon(1L, Grip.OneHanded, atk = 10L)
      val b   = weapon(2L, Grip.OneHanded, atk = 7L)
      val big = weapon(3L, Grip.TwoHanded, atk = 30L)
      val (eq, f, out) = InventoryState.equip(armed(a, b), big, None)
      assertTrue(eq.weapon == big && eq.leftHand.itemType == ItemType.NoItem && eq.leftBusy) &&
      assertTrue(out == List(a, b)) &&
      assertTrue(f.atk == hero.fightStats.atk + 30L) &&
      // руку под двуручным не предлагают: она занята, а не полна
      assertTrue(!eq.handsFull)
    },

    test("одноручное поверх двуручного снимает двуручное — в какую руку его ни бери") {
      val big = weapon(3L, Grip.TwoHanded, atk = 30L)
      val a   = weapon(1L, Grip.OneHanded, atk = 10L)
      val (r, fr, outR) = InventoryState.equip(armed(big), a, None)
      val (l, _, outL)  = InventoryState.equip(armed(big), a, Some(2))
      assertTrue(r.weapon == a && r.leftHand.itemType == ItemType.NoItem && outR == List(big)) &&
      assertTrue(l.weapon.itemType == ItemType.NoItem && l.leftHand == a && outL == List(big)) &&
      assertTrue(fr.atk == hero.fightStats.atk + 10L)
    },

    test("двуручное поверх двух одноручных возвращает в сумку на вещь больше: в полную — не надеть") {
      val a   = weapon(1L, Grip.OneHanded)
      val b   = weapon(2L, Grip.OneHanded)
      def bag(n: Int) = Inventory(1L, hero.id, 20L, Inventory.Items(List.tabulate(n)(i => weapon(100L + i, Grip.OneHanded))))
      assertTrue(InventoryState.equipBlock(bag(20), 0L, List(a, b)).contains("inventory.handsNoRoom")) &&
      assertTrue(InventoryState.equipBlock(bag(19), 0L, List(a, b)).isEmpty) &&
      // вещь за вещь — как раньше, места не надо
      assertTrue(InventoryState.equipBlock(bag(20), 0L, List(a)).isEmpty)
    },

    test("без оружия — только когда обе руки пусты; реплики говорят о том, что в руках") {
      val a = weapon(1L, Grip.OneHanded)
      assertTrue(empty.unarmed && empty.mainWeapon.itemType == ItemType.NoItem) &&
      assertTrue(!empty.copy(leftHand = a).unarmed && empty.copy(leftHand = a).mainWeapon == a)
    },

    test("навыки берутся с оружия в обеих руках") {
      val skills = Skill.weaponSkills
      val a = weapon(1L, Grip.OneHanded).copy(details = ItemDetails.Weapon(skills.head))
      val b = weapon(2L, Grip.OneHanded).copy(details = ItemDetails.Weapon(skills(1)))
      val ids = armed(a, b).activeSkillSlots.map(_.itemId).toSet
      assertTrue(Set(1L, 2L).subsetOf(ids))
    },

    test("тринадцатая вещь набора лишняя: счёт упирается в двенадцать") {
      val hunter: ItemSet = ItemSet.Hunter
      def piece(id: Long) = weapon(id, Grip.OneHanded).copy(set = Some(hunter))
      val twelve   = TestFixtures.wearingSet(hunter, 12)
      val thirteen = twelve.copy(leftHand = piece(77L)).setCounts.getOrElse(hunter, 0)
      // а левая рука считается в набор наравне с правой
      val leftHand = TestFixtures.wearingSet(hunter, 11).copy(leftHand = piece(78L)).setCounts.getOrElse(hunter, 0)
      val full     = twelve.setCounts.getOrElse(hunter, 0)
      assertTrue(full == 12 && thirteen == 12 && leftHand == 12)
    },

    test("старые записи читаются: левой руки нет — она пуста, оружие без хвата — одноручное") {
      val eq      = empty.copy(weapon = weapon(1L, Grip.OneHanded))
      val oldEq   = eq.asJson.mapObject(_.remove("leftHand"))
      val oldItem = weapon(5L, Grip.TwoHanded).asJson.mapObject(_.remove("grip"))
      assertTrue(oldEq.as[Equipment].map(_.leftHand.itemType) == Right(ItemType.NoItem)) &&
      assertTrue(oldEq.as[Equipment].map(_.weapon.id) == Right(1L)) &&
      assertTrue(oldItem.as[Item].map(_.twoHanded) == Right(false)) &&
      assertTrue(eq.copy(leftHand = weapon(2L, Grip.OneHanded)).asJson.as[Equipment] ==
        Right(eq.copy(leftHand = weapon(2L, Grip.OneHanded))))
    }
  )
}
