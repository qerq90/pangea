package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.{ItemType, Rarity}

/**
 * Генератор названий предметов: «<качество> <тип> <титул> <раса>», напр.
 * «Сломанное Копьё командира эльфа». Качество выбирается по редкости и
 * согласуется по роду с типом предмета (м/ж/ср/мн.ч.); титул и раса всегда в
 * родительном падеже единственного числа.
 */
object ItemNameGenerator {

  sealed trait Gender
  object Gender {
    case object Masc extends Gender
    case object Fem  extends Gender
    case object Neut extends Gender
    case object Plur extends Gender
  }
  import Gender._

  // Тип предмета → его «существительные» с грамматическим родом для согласования.
  private val baseNames: Map[ItemType, List[(String, Gender)]] = Map(
    ItemType.Helmet           -> List("Шлем" -> Masc, "Капюшон" -> Masc, "Корона" -> Fem),
    ItemType.ShoulderPads     -> List("Наплечники" -> Plur, "Эполеты" -> Plur),
    ItemType.ChestPlate       -> List("Нагрудник" -> Masc, "Кираса" -> Fem, "Броня" -> Fem),
    ItemType.Bracelets        -> List("Браслеты" -> Plur, "Наручи" -> Plur),
    ItemType.Gloves           -> List("Перчатки" -> Plur, "Рукавицы" -> Plur),
    ItemType.Pants            -> List("Штаны" -> Plur, "Шаровары" -> Plur),
    ItemType.Boots            -> List("Сапоги" -> Plur, "Ботинки" -> Plur),
    ItemType.Leggings         -> List("Поножи" -> Plur, "Гамаши" -> Plur),
    ItemType.Amulet           -> List("Амулет" -> Masc, "Медальон" -> Masc),
    ItemType.Ring             -> List("Кольцо" -> Neut, "Перстень" -> Masc),
    ItemType.Belt             -> List("Пояс" -> Masc, "Ремень" -> Masc),
    ItemType.Flask            -> List("Фляга" -> Fem, "Склянка" -> Fem),
    ItemType.Weapon           -> List("Меч" -> Masc, "Топор" -> Masc, "Кинжал" -> Masc, "Копьё" -> Neut),
    ItemType.AdditionalWeapon -> List("Кинжал" -> Masc, "Стилет" -> Masc),
    ItemType.NoItem           -> List("Пусто" -> Neut)
  )

  // Прилагательные качества по редкости (мужская форма; согласование делает inflect).
  // Violet и Purple используют один пул (в спеке они объединены как «фиолетовые/пурпурные»).
  private val purpleAdjectives = List("Превосходный", "Высококлассный", "Первоклассный", "Впечатляющий", "Выдающийся")
  private val adjectivesByRarity: Map[Rarity, List[String]] = Map(
    Rarity.Gray   -> List("Сломанный", "Треснутый", "Скрученный", "Изношенный", "Поломанный",
                          "Разбитый", "Обломанный", "Поврежденный", "Раскрошенный", "Затёртый", "Деформированный"),
    Rarity.White  -> List("Обычный", "Стандартный", "Простенький", "Неприметный", "Средний",
                          "Заурядный", "Повседневный", "Простой", "Невыдающийся", "Типичный"),
    Rarity.Green  -> List("Интересный", "Необычный", "Свойственный", "Отличительный", "Своеобразный",
                          "Неординарный", "Удивительный", "Нестандартный"),
    Rarity.Blue   -> List("Хороший", "Надёжный", "Качественный", "Прочный", "Удобный",
                          "Стильный", "Добротный", "Функциональный", "Долговечный"),
    Rarity.Purple -> purpleAdjectives,
    Rarity.Violet -> purpleAdjectives,
    Rarity.Orange -> List("Лучший", "Идеальный", "Безупречный", "Эталонный", "Непревзойдённый", "Легендарный")
  )

  // Титулы и расы — всегда в родительном падеже единственного числа.
  private val titles = List("Рыцаря", "Командира", "Дворянина", "Аристократа", "Гвардейца",
                            "Кавалериста", "Стражника", "Охранника", "Повара", "Стрелка",
                            "Разведчика", "Оруженосца", "Бойца", "Вождя")
  private val races  = List("Эльфа", "Орка", "Гоблина", "Мурлока", "Демона", "Гнома",
                            "Аргонианца", "Человека", "Каджита", "Прохвоста", "Троглодита", "Скелета")

  def generate(itemType: ItemType, rarity: Rarity, rng: Rng): (String, Rng) = {
    val (adjMasc,       rng1) = rng.pick(adjectivesByRarity.getOrElse(rarity, List("Обычный")))
    val ((base, gender), rng2) = rng1.pick(baseNames.getOrElse(itemType, List("Предмет" -> Masc)))
    val (title,         rng3) = rng2.pick(titles)
    val (race,          rng4) = rng3.pick(races)
    val adj                   = inflect(adjMasc, gender)
    (s"$adj $base $title $race", rng4)
  }

  /**
   * Конвертирует прилагательное мужского рода (`-ый`/`-ий`/`-ой`) в нужную форму.
   * Правила правописания:
   *   - после задненёбных (г/к/х) и шипящих (ж/ш/ч/щ) во мн.ч. — `-ие` (Хорошие, Простенькие);
   *   - после шипящих в ср.р. — `-ее` (Хорошее), иначе — `-ое`;
   *   - женский род у всех окончаний — `-ая`.
   */
  private def inflect(masc: String, gender: Gender): String = {
    if (gender == Masc || masc.length < 2) return masc
    // Возвратные прилагательные на «-ийся» (Выдающийся, Невыдающийся): особый
    // ряд окончаний -аяся/-ееся/-иеся.
    if (masc.endsWith("ийся")) {
      val stem = masc.dropRight(4)
      return gender match {
        case Fem  => stem + "аяся"
        case Neut => stem + "ееся"
        case Plur => stem + "иеся"
        case Masc => masc
      }
    }
    val base = masc.dropRight(2)
    val ending = masc.takeRight(2)
    val prev = base.lastOption.getOrElse(' ').toLower
    val isHushing = "жшчщ".contains(prev)
    val isVelar   = "гкх".contains(prev)
    gender match {
      case Fem  => base + "ая"
      case Neut => base + (if (isHushing) "ее" else "ое")
      case Plur => ending match {
        case "ый"                                  => base + "ые"
        case "ий"                                  => base + "ие"
        case "ой" if isVelar || isHushing          => base + "ие"
        case "ой"                                  => base + "ые"
        case _                                     => masc
      }
      case Masc => masc
    }
  }
}
