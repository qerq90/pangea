package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.{ItemType, Rarity}

/**
 * Генератор названий предметов: «<кружок-редкости> <тип> <титул>», напр.
 * «🟢 Кираса стражника». Цвет кружка кодирует редкость (см. [[rarityEmoji]]);
 * тип — с заглавной, титул — с маленькой; первое слово в названии (тип) —
 * единственное с заглавной буквы.
 */
object ItemNameGenerator {

  // Существительные для типа предмета (в именительном, как обычно пишутся).
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
    ItemType.Weapon           -> List("Меч", "Топор", "Кинжал", "Копьё"),
    ItemType.AdditionalWeapon -> List("Кинжал", "Стилет"),
    ItemType.NoItem           -> List("Пусто")
  )

  // Цветной кружок для редкости. Purple и Violet делят один цвет — в emoji нет
  // двух разных «фиолетовых кругов»; при необходимости легко развести через
  // подмену здесь.
  private val rarityEmoji: Map[Rarity, String] = Map(
    Rarity.Gray   -> "⚫",
    Rarity.White  -> "⚪",
    Rarity.Green  -> "🟢",
    Rarity.Blue   -> "🔵",
    Rarity.Purple -> "🟣",
    Rarity.Violet -> "🟣",
    Rarity.Orange -> "🟠"
  )

  // Титулы — в родительном падеже единственного числа, с маленькой буквы.
  private val titles = List("рыцаря", "командира", "дворянина", "аристократа", "гвардейца",
                            "кавалериста", "стражника", "охранника", "повара", "стрелка",
                            "разведчика", "оруженосца", "бойца", "вождя")

  def generate(itemType: ItemType, rarity: Rarity, rng: Rng): (String, Rng) = {
    val (base,  rng1) = rng.pick(baseNames.getOrElse(itemType, List("Предмет")))
    val (title, rng2) = rng1.pick(titles)
    val emoji         = rarityEmoji.getOrElse(rarity, "⚪")
    (s"$emoji $base $title", rng2)
  }
}
