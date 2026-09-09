package pangea.model.battle

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats

/** Состояние активного навыка в рамках конкретного боя. Ключ — `itemId` предмета,
 *  на котором висит навык: разные предметы со «одним и тем же» Skill имеют
 *  независимые `cooldown` и `uses` (на будущее — например, два предмета с
 *  «Малым исцелением» катаются параллельно). */
case class SkillSlotState(
  itemId:   Long,
  skill:    Skill,
  cooldown: Int = 0,
  uses:     Int = 0
)

object SkillSlotState {
  implicit val encoder: Encoder[SkillSlotState] = deriveEncoder
  implicit val decoder: Decoder[SkillSlotState] = deriveDecoder
}

case class SoloPveBattle(
  monsterLvl:          Long,
  monsterRace:         String,
  monsterRarity:       String,
  monsterStats:        FightStats,
  monsterCurrentHp:    Long,
  monsterCurrentArmor: Long,
  heroBattleState:     HeroBattleState = HeroBattleState.empty,
  consumableUsedThisRound: Boolean = false, // за раунд можно выпить либо флягу, либо зелье пояса
  monsterMarked:       Boolean = false,
  skillSlots:          List[SkillSlotState] = Nil,
  effects:             BattleEffects = BattleEffects.empty, // тикающие статус-эффекты (яд/реген)
  toughnessUsed:       Boolean = false, // пассивка «Крепкость» срабатывает один раз за бой
  // Вид элементаля, если это бой с минибоссом (имя варианта Elemental). У
  // обычных мобов пусто — по нему бой и отличает босса от рядового врага.
  bossKind:       Option[String] = None,
  // Какую способность элементаль применит следующей (он ходит строго по кругу),
  // и сколько зарядов он уже собрал рядом с собой: у огненного это сферы огня,
  // у каменного — валуны. Механика у них одна, поэтому счётчик общий.
  bossTurn:       Int = 0,
  bossCharges:    Int = 0,
  // Сколько раз минибосс уже поднимался после обнуления HP (Гнилой Джо).
  bossRevives:    Int = 0,
  // Текущая энергия моба. У рядовых мобов не расходуется (их скиллы бесплатны),
  // а элементаль тратит её на способности и восстанавливает по столько-то за раунд.
  monsterCurrentEnergy: Long = 0L
) {

  /** Элементаль этого боя, если сражаемся с минибоссом. */
  def boss: Option[pangea.model.monster.MiniBoss] =
    bossKind.flatMap(pangea.model.monster.MiniBoss.byName)

  /** Обновляет эффекты боя, уважая иммунитеты расы моба: элементалю нельзя
   *  навесить яд или кровотечение — ему нечему течь и нечего травить. Все точки,
   *  накладывающие эффекты на моба, идут через этот метод, а не через `copy`,
   *  чтобы иммунитет нельзя было обойти, забыв про него в новом источнике. */
  def withEffects(e: BattleEffects): SoloPveBattle = {
    val noDots  = if (Race.immuneToDots(Race.withName(monsterRace))) e.copy(monsterPoison = None, monsterBleed = None) else e
    // Огненного элементаля вдобавок нельзя поджечь — он и так пламя.
    val noBurn  = if (boss.exists(_.immuneToBurn)) noDots.copy(monsterBurn = None) else noDots
    copy(effects = noBurn)
  }
  def toMonster: Monster =
    Monster(0L, monsterLvl, Race.withName(monsterRace), Rarity.withName(monsterRarity), monsterStats, monsterMarked)

  /** Имя моба для лога и экрана боя. У минибосса оно именное и приходит от вида
   *  («Огненный Элементаль»); у обычных мобов — из таблицы раса × редкость.
   *  Держим его ЗДЕСЬ, а не в [[Monster]]: тот читается из таблицы `monsters`
   *  целиком (`select *`), и лишнее поле сломало бы чтение. */
  def monsterName: String = boss.map(_.monsterName).getOrElse(toMonster.name)

  def rarity: Rarity = Rarity.withName(monsterRarity)

  /** Тик в конце хода игрока. `skipSlots` — itemId слотов, кулдауны которых не
   *  должны декрементиться (например, слот, скил из которого только что
   *  использован — его cd должен начать отсчитываться со СЛЕДУЮЩЕГО хода). */
  def tickBuffs(skipSlots: Set[Long] = Set.empty): SoloPveBattle = copy(
    heroBattleState         = heroBattleState.tick,
    consumableUsedThisRound = false,
    skillSlots              = skillSlots.map(s =>
      if (skipSlots.contains(s.itemId)) s
      else s.copy(cooldown = (s.cooldown - 1).max(0))
    ),
    // Временные стихийные эффекты тикают вместе с бафами: буст Воздуха и %-дебаф
    // защиты цели (комбо Молния+Холод) живут ограниченное число ходов.
    effects = effects.copy(
      airBoostTurns        = (effects.airBoostTurns - 1).max(0),
      // Оцепенение элементаля от холода тикает вместе с прочими временными эффектами.
      chilledTurns         = (effects.chilledTurns - 1).max(0),
      // Дебафы каменного элементаля живут теми же ходами.
      heroStunnedTurns     = (effects.heroStunnedTurns - 1).max(0),
      heroGroundedTurns    = (effects.heroGroundedTurns - 1).max(0),
      monsterWeakenedTurns = (effects.monsterWeakenedTurns - 1).max(0),
      monsterDefenceDebuff = effects.monsterDefenceDebuff.flatMap(_.ticked)
    )
  )

  def slotByItem(itemId: Long): Option[SkillSlotState] = skillSlots.find(_.itemId == itemId)

  def updateSlot(itemId: Long)(f: SkillSlotState => SkillSlotState): SoloPveBattle =
    copy(skillSlots = skillSlots.map(s => if (s.itemId == itemId) f(s) else s))
}

object SoloPveBattle {
  /** Сборка боя из моба и героя: статы/hp/броня берутся с моба, слоты активных
   *  навыков отдаёт сам герой (`hero.activeSkillSlots`). Единая точка входа в
   *  бой для всех событий. */
  def from(monster: Monster, hero: Hero): SoloPveBattle = SoloPveBattle(
    monsterLvl          = monster.lvl,
    monsterRace         = monster.race.entryName,
    monsterRarity       = monster.rarity.entryName,
    monsterStats        = monster.fightStats,
    monsterCurrentHp    = monster.fightStats.hp,
    monsterCurrentArmor = monster.fightStats.armor * monster.fightStats.defence.max(1L),
    monsterMarked       = monster.marked,
    skillSlots          = hero.activeSkillSlots,
    monsterCurrentEnergy = monster.fightStats.energy
  )

  implicit val encoder: Encoder[SoloPveBattle] = deriveEncoder

  implicit val decoder: Decoder[SoloPveBattle] = (c: HCursor) =>
    for {
      monsterLvl          <- c.get[Long]("monsterLvl")
      monsterRace         <- c.get[String]("monsterRace")
      monsterRarity       <- c.get[String]("monsterRarity")
      monsterStats        <- c.get[FightStats]("monsterStats")
      monsterCurrentHp    <- c.get[Long]("monsterCurrentHp")
      monsterCurrentArmor <- c.get[Long]("monsterCurrentArmor")
      heroBattleState     <- c.getOrElse[HeroBattleState]("heroBattleState")(HeroBattleState.empty)
      consumableUsed      <- c.getOrElse[Boolean]("consumableUsedThisRound")(false)
      monsterMarked       <- c.getOrElse[Boolean]("monsterMarked")(false)
      skillSlots          <- c.getOrElse[List[SkillSlotState]]("skillSlots")(Nil)
      effects             <- c.getOrElse[BattleEffects]("effects")(BattleEffects.empty)
      toughnessUsed       <- c.getOrElse[Boolean]("toughnessUsed")(false)
      bossKind       <- c.getOrElse[Option[String]]("bossKind")(None)
      bossTurn       <- c.getOrElse[Int]("bossTurn")(0)
      charges             <- c.getOrElse[Int]("bossCharges")(0)
      revives             <- c.getOrElse[Int]("bossRevives")(0)
      monsterEnergy       <- c.getOrElse[Long]("monsterCurrentEnergy")(0L)
    } yield SoloPveBattle(monsterLvl, monsterRace, monsterRarity, monsterStats,
                         monsterCurrentHp, monsterCurrentArmor, heroBattleState, consumableUsed, monsterMarked,
                         skillSlots, effects, toughnessUsed, bossKind, bossTurn, charges, revives, monsterEnergy)
}
