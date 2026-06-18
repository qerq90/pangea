package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.{ItemType, Rarity}

object ItemNameGenerator {

  private val baseNames: Map[ItemType, List[String]] = Map(
    ItemType.Helmet           -> List("Шлем", "Капюшон", "Корона"),
    ItemType.ShoulderPads     -> List("Наплечники", "Эполеты"),
    ItemType.ChestPlate       -> List("Нагрудник", "Кираса", "Броня"),
    ItemType.Bracelets        -> List("Браслеты", "Наручи"),
    ItemType.Gloves           -> List("Перчатки", "Рукавицы"),
    ItemType.Pants            -> List("Штаны", "Шаровары"),
    ItemType.Boots            -> List("Сапоги", "Ботинки"),
    ItemType.Leggings         -> List("Поножи", "Гамаши"),
    ItemType.Amulet           -> List("Амулет", "Медальон"),
    ItemType.Ring             -> List("Кольцо", "Перстень"),
    ItemType.Belt             -> List("Пояс", "Ремень"),
    ItemType.Flask            -> List("Фляга", "Склянка"),
    ItemType.Weapon           -> List("Меч", "Топор", "Кинжал"),
    ItemType.AdditionalWeapon -> List("Кинжал", "Стилет"),
    ItemType.NoItem           -> List("Пусто")
  )

  private val rarityNames: Map[Rarity, String] = Map(
    Rarity.Gray   -> "",
    Rarity.White  -> "путника",
    Rarity.Green  -> "стража",
    Rarity.Blue   -> "воина",
    Rarity.Purple -> "ярости",
    Rarity.Violet -> "судьбы",
    Rarity.Orange -> "легенды"
  )

  def generate(itemType: ItemType, rarity: Rarity, rng: Rng): (String, Rng) = {
    val names          = baseNames.getOrElse(itemType, List("Предмет"))
    val (base, newRng) = rng.pick(names)
    val suffix         = rarityNames.getOrElse(rarity, "")
    val name           = if (suffix.isEmpty) base else s"$base $suffix"
    (name, newRng)
  }
}
