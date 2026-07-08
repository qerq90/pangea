package pangea.model.monster

import pangea.model.stats.FightStats

case class Monster(
  id: Long,
  lvl: Long,
  race: Race,
  rarity: Rarity,
  fightStats: FightStats,
  marked: Boolean = false // модификатор «Отмеченный тьмой»
) {
  def name: String = {
    val base = Monster.namesByRaceRarity.getOrElse(
      (race, rarity),
      s"$rarity $race"
    ) // fallback на случай новой расы без записи
    if (marked) s"${Monster.MarkedPrefix} $base" else base
  }
}

object Monster {
  val MarkedPrefix: String = "Отмеченный тьмой"

  /** Имена мобов «раса × редкость» — пафосные названия для встреч в подземелье.
    * Если пара отсутствует в таблице — fallback на «<Редкость> <Раса>».
    */
  val namesByRaceRarity: Map[(Race, Rarity), String] = Map(
    // ── Мурлок ─────────────────────────────────────────────────────────────
    (Race.Murloc, Rarity.Common)    -> "Мурлок-раб",
    (Race.Murloc, Rarity.Uncommon)  -> "Мурлок-солдат",
    (Race.Murloc, Rarity.Rare)      -> "Мурлок-теневой убийца",
    (Race.Murloc, Rarity.Mythical)  -> "Мурлок-вожак",
    (Race.Murloc, Rarity.Legendary) -> "Старый Мрачноглаз",

    // ── Эльф ───────────────────────────────────────────────────────────────
    (Race.Elf, Rarity.Common)    -> "Эльф сбежавший раб",
    (Race.Elf, Rarity.Uncommon)  -> "Эльф лучник",
    (Race.Elf, Rarity.Rare)      -> "Эльф следопыт",
    (Race.Elf, Rarity.Mythical)  -> "Эльф капитан",
    (Race.Elf, Rarity.Legendary) -> "Кельтас Солнечный Скиталец",

    // ── Орк ────────────────────────────────────────────────────────────────
    (Race.Orc, Rarity.Common)    -> "Орк раб",
    (Race.Orc, Rarity.Uncommon)  -> "Орк воин",
    (Race.Orc, Rarity.Rare)      -> "Живучий орк",
    (Race.Orc, Rarity.Mythical)  -> "Орк командир отряда",
    (Race.Orc, Rarity.Legendary) -> "Каркан Кровавый Топор",

    // ── Гоблин ─────────────────────────────────────────────────────────────
    (Race.Goblin, Rarity.Common)    -> "Гоблин немощный раб",
    (Race.Goblin, Rarity.Uncommon)  -> "Гоблин вор",
    (Race.Goblin, Rarity.Rare)      -> "Ловкий гоблин",
    (Race.Goblin, Rarity.Mythical)  -> "Хобгоблин — предводитель банды",
    (Race.Goblin, Rarity.Legendary) -> "Эззи Повелитель Гоблинов",

    // ── Демон ──────────────────────────────────────────────────────────────
    (Race.Demon, Rarity.Common)    -> "Бес",
    (Race.Demon, Rarity.Uncommon)  -> "Суккуб",
    (Race.Demon, Rarity.Rare)      -> "Рогатый демон",
    (Race.Demon, Rarity.Mythical)  -> "Адское отродье",
    (Race.Demon, Rarity.Legendary) -> "Асмодей",

    // ── Гном ───────────────────────────────────────────────────────────────
    (Race.Gnome, Rarity.Common)    -> "Гном-раб",
    (Race.Gnome, Rarity.Uncommon)  -> "Гном соратник",
    (Race.Gnome, Rarity.Rare)      -> "Гном костелом",
    (Race.Gnome, Rarity.Mythical)  -> "Гном предводитель",
    (Race.Gnome, Rarity.Legendary) -> "Гарми Череполом",

    // ── Человек ────────────────────────────────────────────────────────────
    (Race.Human, Rarity.Common)    -> "Человек-раб",
    (Race.Human, Rarity.Uncommon)  -> "Вор",
    (Race.Human, Rarity.Rare)      -> "Разбойник",
    (Race.Human, Rarity.Mythical)  -> "Капитан местных разбойников",
    (Race.Human, Rarity.Legendary) -> "Маршал Генри",

    // ── Каджит ─────────────────────────────────────────────────────────────
    (Race.Khajiit, Rarity.Common)    -> "Каджит-раб",
    (Race.Khajiit, Rarity.Uncommon)  -> "Тень пустыни",
    (Race.Khajiit, Rarity.Rare)      -> "Солдат серебряного когтя",
    (Race.Khajiit, Rarity.Mythical)  -> "Капитан серебряного когтя",
    (Race.Khajiit, Rarity.Legendary) -> "Рейкьян Неуловимый"
  )
}
