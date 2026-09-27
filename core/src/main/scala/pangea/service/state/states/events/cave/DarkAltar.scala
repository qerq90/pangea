package pangea.service.state.states.events.cave

import pangea.generator.monster.MonsterGenerator
import pangea.model.item.{Item, ItemDetails, TrophyKind}
import pangea.model.monster.{MiniBoss, Race, Rarity}
import pangea.model.squad.UndeadForm
import pangea.model.stats.FightStats

/** Алтарь тёмных сил: что он делает с тем, что на него положили.
  *
  * Трофей он поднимает обратно — тем, кем добыча была при жизни, только на
  * пятую часть слабее и уже без своей воли. Чем ценнее трофей, тем крупнее то,
  * что встаёт с камня: мешок с пожитками — раб, голова — солдат, талисман —
  * кто-то из старших, реликвия — вожак. Легендарных алтарю не поднять.
  *
  * Клык Белого волка — особая статья: с камня встаёт тот самый волк, которого
  * герой убил. Его уровень зашит в самом клыке (коэффициент трофея — шесть за
  * каждый уровень босса), поэтому ни трофею, ни дропу ничего добавлять не
  * пришлось; подобрать чужой клык тоже нельзя — трофеи не передаются. */
object DarkAltar {

  /** Имя того, что встаёт с камня по клыку. */
  val DarkWolfName: String = "Тёмный волк"

  /** На столько процентов поднятый слабее того, кем был при жизни. */
  val WeakenPct: Long = 20L

  /** Кого поднимает трофей. Клык идёт своей дорогой — см. [[wolfOf]]. */
  def rarityOf(kind: TrophyKind): Option[Rarity] = kind match {
    case TrophyKind.Sack     => Some(Rarity.Common)
    case TrophyKind.Head     => Some(Rarity.Uncommon)
    case TrophyKind.Talisman => Some(Rarity.Rare)
    case TrophyKind.Relic    => Some(Rarity.Mythical)
    case TrophyKind.Fang     => None
  }

  /** Что встанет с камня, если положить на него этот трофей. */
  def formOf(item: Item): Option[UndeadForm] = item.details match {
    case ItemDetails.Trophy(_, TrophyKind.Fang, coef) => Some(wolfOf(bossLvlOf(coef)))
    case ItemDetails.Trophy(race, kind, _) =>
      for {
        rarity <- rarityOf(kind)
        r      <- Race.withNameOption(race)
      } yield fromTrophy(r, rarity, item.lvl.max(1L))
    case _ => None
  }

  /** Поднятый по обычному трофею: моб той же расы и того уровня, что записан в
    * трофее, тира по ценности трофея — и на пятую часть слабее. */
  def fromTrophy(race: Race, rarity: Rarity, lvl: Long): UndeadForm = {
    val monster = MonsterGenerator.generateOfRaceAndRarity(lvl.toInt, race, rarity)
    UndeadForm(monster.name, lvl, weakened(monster.fightStats))
  }

  /** Тёмный волк того уровня, каким был убитый Белый. */
  def wolfOf(bossLvl: Long): UndeadForm =
    UndeadForm(DarkWolfName, bossLvl, weakened(MiniBoss.WhiteWolf.stats(bossLvl)))

  /** Уровень волка из коэффициента клыка: он и есть шесть за каждый уровень босса. */
  def bossLvlOf(coef: Option[Double]): Long =
    coef.map(c => (c / MiniBoss.WhiteWolf.FangCoefPerLvl).round.max(1L)).getOrElse(1L)

  private def weakened(s: FightStats): FightStats = {
    def cut(v: Long): Long = v * (100L - WeakenPct) / 100L
    FightStats(
      atk      = cut(s.atk).max(1L),
      hp       = cut(s.hp).max(1L),
      armor    = cut(s.armor),
      defence  = cut(s.defence),
      evasion  = cut(s.evasion),
      accuracy = cut(s.accuracy),
      energy   = cut(s.energy))
  }
}
