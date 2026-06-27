package pangea.model.battle

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
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

case class ActiveBattle(
  monsterLvl:          Long,
  monsterRace:         String,
  monsterRarity:       String,
  monsterStats:        FightStats,
  monsterCurrentHp:    Long,
  monsterCurrentArmor: Long,
  heroBattleState:     HeroBattleState = HeroBattleState.empty,
  flaskUsedThisRound:  Boolean = false,
  monsterMarked:       Boolean = false,
  skillSlots:          List[SkillSlotState] = Nil
) {
  def toMonster: Monster =
    Monster(0L, monsterLvl, Race.withName(monsterRace), Rarity.withName(monsterRarity), monsterStats, monsterMarked)

  def rarity: Rarity = Rarity.withName(monsterRarity)

  /** Тик в конце хода игрока. `skipSlots` — itemId слотов, кулдауны которых не
   *  должны декрементиться (например, слот, скил из которого только что
   *  использован — его cd должен начать отсчитываться со СЛЕДУЮЩЕГО хода). */
  def tickBuffs(skipSlots: Set[Long] = Set.empty): ActiveBattle = copy(
    heroBattleState    = heroBattleState.tick,
    flaskUsedThisRound = false,
    skillSlots         = skillSlots.map(s =>
      if (skipSlots.contains(s.itemId)) s
      else s.copy(cooldown = (s.cooldown - 1).max(0))
    )
  )

  def slotByItem(itemId: Long): Option[SkillSlotState] = skillSlots.find(_.itemId == itemId)

  def updateSlot(itemId: Long)(f: SkillSlotState => SkillSlotState): ActiveBattle =
    copy(skillSlots = skillSlots.map(s => if (s.itemId == itemId) f(s) else s))
}

object ActiveBattle {
  def fromMonster(monster: Monster): ActiveBattle = ActiveBattle(
    monsterLvl          = monster.lvl,
    monsterRace         = monster.race.entryName,
    monsterRarity       = monster.rarity.entryName,
    monsterStats        = monster.fightStats,
    monsterCurrentHp    = monster.fightStats.hp,
    monsterCurrentArmor = monster.fightStats.armor * monster.fightStats.defence.max(1L),
    monsterMarked       = monster.marked
  )

  implicit val encoder: Encoder[ActiveBattle] = deriveEncoder

  implicit val decoder: Decoder[ActiveBattle] = (c: HCursor) =>
    for {
      monsterLvl          <- c.get[Long]("monsterLvl")
      monsterRace         <- c.get[String]("monsterRace")
      monsterRarity       <- c.get[String]("monsterRarity")
      monsterStats        <- c.get[FightStats]("monsterStats")
      monsterCurrentHp    <- c.get[Long]("monsterCurrentHp")
      monsterCurrentArmor <- c.get[Long]("monsterCurrentArmor")
      heroBattleState     <- c.getOrElse[HeroBattleState]("heroBattleState")(HeroBattleState.empty)
      flaskUsedThisRound  <- c.getOrElse[Boolean]("flaskUsedThisRound")(false)
      monsterMarked       <- c.getOrElse[Boolean]("monsterMarked")(false)
      skillSlots          <- c.getOrElse[List[SkillSlotState]]("skillSlots")(Nil)
    } yield ActiveBattle(monsterLvl, monsterRace, monsterRarity, monsterStats,
                         monsterCurrentHp, monsterCurrentArmor, heroBattleState, flaskUsedThisRound, monsterMarked, skillSlots)
}
