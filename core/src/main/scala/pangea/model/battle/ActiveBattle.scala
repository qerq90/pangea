package pangea.model.battle

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.generic.semiauto.deriveEncoder
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats

case class ActiveBattle(
  monsterLvl:          Long,
  monsterRace:         String,
  monsterRarity:       String,
  monsterStats:        FightStats,
  monsterCurrentHp:    Long,
  monsterCurrentArmor: Long,
  heroBattleState:     HeroBattleState = HeroBattleState.empty,
  flaskUsedThisRound:  Boolean = false,
  monsterMarked:       Boolean = false
) {
  def toMonster: Monster =
    Monster(0L, monsterLvl, Race.withName(monsterRace), Rarity.withName(monsterRarity), monsterStats, monsterMarked)

  def rarity: Rarity = Rarity.withName(monsterRarity)

  def tickBuffs: ActiveBattle = copy(heroBattleState = heroBattleState.tick, flaskUsedThisRound = false)
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

  // getOrElse on heroBattleState for backwards-compatibility with existing JSONB records
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
    } yield ActiveBattle(monsterLvl, monsterRace, monsterRarity, monsterStats,
                         monsterCurrentHp, monsterCurrentArmor, heroBattleState, flaskUsedThisRound, monsterMarked)
}
