package pangea.service.admin

import pangea.generator.item.{FlaskGenerator, GemGenerator, MaterialGenerator, TreasureMapGenerator}
import pangea.model.item._
import pangea.model.rune.{RuneStone, RuneStoneSize}

/** Всё, что админ-панель умеет выдать без вопросов о параметрах: каждая запись
  * — готовая вещь. Экипировка сюда не входит: у неё имя случайное, а статы
  * зависят от уровня и редкости, поэтому её собирают отдельной веткой панели
  * (тип → редкость → уровень).
  *
  * Каталог плоский и строится один раз: по нему и листают по категориям, и
  * ищут по куску названия. */
object AdminCatalog {

  /** Одна выдаваемая вещь: как её показать и что, собственно, выдать. */
  final case class Entry(id: String, label: String, item: Item)

  /** Раздел каталога. `id` едет в кнопке, поэтому он короткий и латиницей. */
  final case class Category(id: String, label: String, entries: List[Entry])

  private def entry(id: String, item: Item): Entry = Entry(id, item.name, item)

  /** Камни-усилителя всех видов и достоинств. */
  private val gems: List[Entry] =
    for {
      kind  <- GemKind.values.toList
      grade <- (Gem.MinGrade to Gem.MaxGrade).toList
    } yield entry(s"gem:${kind.entryName}:$grade", GemGenerator.item(kind, grade))

  /** Фляги: вид и редкость (от редкости зависит запас зарядов). */
  private val flasks: List[Entry] =
    for {
      kind   <- FlaskKind.values.toList
      rarity <- Rarity.values.toList
    } yield entry(s"flask:${kind.entryName}:${rarity.entryName}", FlaskGenerator.item(kind, rarity))

  private val brews: List[Entry] =
    BrewKind.values.toList.map(k => entry(s"brew:${k.entryName}", BrewKind.item(k)))

  private val materials: List[Entry] =
    MaterialKind.values.toList.map(k => entry(s"mat:${k.entryName}", MaterialGenerator.item(k)))

  private val runes: List[Entry] =
    for {
      rune <- RuneStone.all
      size <- RuneStoneSize.values.toList
    } yield entry(s"rune:${rune.key}:${size.entryName}", RuneStone.item(rune, size))

  private val roses: List[Entry] =
    RoseKind.values.toList.map(k => entry(s"rose:${k.entryName}", RoseKind.item(k)))

  private val quest: List[Entry] =
    QuestItemKind.values.toList.map(k => entry(s"quest:${k.entryName}", QuestItemKind.item(k)))

  private val maps: List[Entry] =
    MapZone.values.toList.map(z => entry(s"map:${z.entryName}", TreasureMapGenerator.full(z)))

  private val trophies: List[Entry] =
    for {
      race <- pangea.model.monster.Race.mortals.toList
      kind <- TrophyKind.values.toList.filter(_ != TrophyKind.Fang)
    } yield Entry(s"trophy:${race.entryName}:${kind.entryName}",
      s"${kind.displayName} ($race)",
      Item(id = -1L, name = s"${kind.displayName} ($race)", lvl = 1L, rarity = Rarity.Gray,
        itemType = ItemType.Trophy, attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        details = ItemDetails.Trophy(race.entryName, kind)))

  /** Божественное оружие: уровень и редкость у него от оружия, ушедшего в
    * ковку, — панель выдаёт легендарное сотого уровня. */
  private val divine: List[Entry] =
    DivineKind.values.toList.map(k =>
      entry(s"divine:${k.entryName}", DivineKind.item(k, DivineLvl, Rarity.Orange)))

  /** Уровень, с которым панель выдаёт божественное оружие. */
  val DivineLvl: Long = 100L

  val categories: List[Category] = List(
    Category("gem",    "💎 Камни",      gems),
    Category("flask",  "🧪 Фляги",      flasks),
    Category("brew",   "🍶 Отвары",     brews),
    Category("mat",    "🌿 Материалы",  materials),
    Category("rune",   "🪨 Руны",       runes),
    Category("rose",   "🌹 Розы",       roses),
    Category("divine", "⚜ Божественное", divine),
    Category("trophy", "🏆 Трофеи",     trophies),
    Category("map",    "🗺 Карты",      maps),
    Category("quest",  "★ Сюжетные",    quest))

  /** Весь каталог одной кучей — по нему идёт поиск. */
  val all: List[Entry] = categories.flatMap(_.entries)

  private val byId: Map[String, Entry] = all.map(e => e.id -> e).toMap

  def find(id: String): Option[Entry] = byId.get(id)

  def category(id: String): Option[Category] = categories.find(_.id == id)

  /** Поиск по куску названия, без оглядки на регистр. */
  def search(query: String): List[Entry] = {
    val q = query.trim.toLowerCase
    if (q.isEmpty) Nil else all.filter(_.label.toLowerCase.contains(q))
  }
}
