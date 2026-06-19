package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.ActiveBattle
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class BattleState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "Attack"      -> Target.Run { (user, _, renderer) => attack(user, renderer) },
      "UseFlask"    -> Target.Run { (user, _, renderer) => useFlask(user, renderer) },
      "Flee"        -> Target.Run { (user, _, renderer) => flee(user, renderer) },
      "ConfirmFlee" -> Target.Run { (user, _, renderer) => confirmFlee(user, renderer) },
      "CancelFlee"  -> Target.Run { (user, _, renderer) => showScreen(user, renderer).as(StateType.Battle) }
    ),
    fallback = Target.Run { (user, _, renderer) => showScreen(user, renderer).as(StateType.Battle) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    showScreen(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def attack(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero    <- getHero(user)
      battle  <- getBattle(user)
      eff       = hero.effectiveFightStats(now)
      buffedEff = battle.heroBattleState.applyTo(eff)
      monster   = battle.toMonster

      hitRoll   <- Random.nextIntBetween(1, 101)
      hitChance  = playerHitChance(buffedEff.accuracy, battle)

      result <- if (hitRoll <= (hitChance * 100).toInt) {
        for {
          spread <- Random.nextLongBetween(80L, 121L)
          noWeapon = hero.equipment.weapon.itemType == pangea.model.item.ItemType.NoItem
          weaponMod: Double = if (noWeapon) 0.5 else 1.0
          concBonus: Double = 1.0 + buffedEff.concentration * 0.005
          damage  = (((buffedEff.atk + hero.baseStats.str) * spread / 100L) * weaponMod * concBonus).toLong.max(1L)
          _      <- renderer.show(user, Screen(
                      content.format("battle.hit", "damage" -> damage.toString, "monster" -> monster.name), Nil))
          armorDmg  = math.min(battle.monsterCurrentArmor, damage)
          hpDmg     = damage - armorDmg
          newArmor  = battle.monsterCurrentArmor - armorDmg
          newHp     = (battle.monsterCurrentHp - hpDmg).max(0L)
          r <- if (newHp <= 0) victory(user, hero, battle, renderer)
               else monsterAttacks(user, hero, battle.copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor), now, renderer)
        } yield r
      } else {
        renderer.show(user, Screen(content.text("battle.miss"), Nil)) *>
          monsterAttacks(user, hero, battle, now, renderer)
      }
    } yield result

  private def monsterAttacks(user: User, hero: Hero, battle: ActiveBattle, nowMs: Long, renderer: Renderer): Task[StateType] =
    for {
      ticked   <- ZIO.succeed(battle.tickBuffs)
      _        <- heroDao.writeActiveBattle(user.userId, ticked.asJson)
      eff       = hero.effectiveFightStats(nowMs)
      buffedEff = ticked.heroBattleState.applyTo(eff)
      monster   = ticked.toMonster
      hitRoll  <- Random.nextIntBetween(1, 101)
      hitChance = mobHitChance(hero.baseStats.agi, buffedEff.evasion, ticked)

      result <- if (hitRoll <= (hitChance * 100).toInt) {
        for {
          spread      <- Random.nextLongBetween(80L, 121L)
          rawDamage    = (monster.fightStats.atk * spread / 100L).max(1L)
          buffReduct   = math.min(ticked.heroBattleState.armorBonus, rawDamage)
          afterBuff    = rawDamage - buffReduct
          armorAbsorb  = math.min(hero.fightStats.armor, afterBuff)
          hpDmg        = afterBuff - armorAbsorb
          newArmor     = hero.fightStats.armor - armorAbsorb
          newHp        = (hero.fightStats.hp - hpDmg).max(0L)
          newStats     = hero.fightStats.copy(hp = newHp, armor = newArmor)
          _           <- heroDao.updateFightStats(user.userId, newStats)
          _           <- renderer.show(user, Screen(
                           content.format("battle.mobHit", "damage" -> rawDamage.toString, "monster" -> monster.name), Nil))
          r <- if (newHp <= 0) heroDeath(user, renderer)
               else showScreen(user, renderer).as(StateType.Battle)
        } yield r
      } else {
        renderer.show(user, Screen(content.format("battle.mobMiss", "monster" -> monster.name), Nil)) *>
          showScreen(user, renderer).as(StateType.Battle)
      }
    } yield result

  private def victory(user: User, hero: Hero, battle: ActiveBattle, renderer: Renderer): Task[StateType] =
    for {
      expGained        <- ZIO.succeed((hero.dungeonLevel.toLong * battle.rarity.factor).toLong.max(1L))
      goldGained        = (hero.dungeonLevel.toLong * battle.rarity.factor).toLong.max(1L)
      newTotalExp       = hero.exp + expGained
      newLevel          = BattleState.computeLevel(newTotalExp)
      newUpgradePoints  = hero.upgradePoints + (newLevel - hero.lvl) * 4
      _                <- heroDao.clearActiveBattle(user.userId)
      _                <- heroDao.updateExpAndLevel(user.userId, newTotalExp, newLevel, newUpgradePoints)
      _                <- heroDao.updateGold(user.userId, hero.gold + goldGained)
      _                <- renderer.show(user, Screen(
                            content.format("battle.victory",
                              "monster" -> battle.toMonster.name,
                              "exp"     -> expGained.toString,
                              "gold"    -> goldGained.toString), Nil))
    } yield StateType.Dungeon

  private def heroDeath(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("battle.death"), Nil)).as(StateType.Death)

  private def flee(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, content.screen("battle.fleeConfirm")).as(StateType.Battle)

  private def confirmFlee(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero    <- getHero(user)
      battle  <- getBattle(user)
      eff       = hero.effectiveFightStats(now)
      buffedEff = battle.heroBattleState.applyTo(eff)
      monster   = battle.toMonster
      hitRoll  <- Random.nextIntBetween(1, 101)

      result <- if (hitRoll <= (mobHitChance(hero.baseStats.agi, buffedEff.evasion, battle) * 100).toInt) {
        for {
          spread      <- Random.nextLongBetween(80L, 121L)
          rawDamage    = (monster.fightStats.atk * spread / 100L).max(1L)
          buffReduct   = math.min(battle.heroBattleState.armorBonus, rawDamage)
          afterBuff    = rawDamage - buffReduct
          armorAbsorb  = math.min(hero.fightStats.armor, afterBuff)
          hpDmg        = afterBuff - armorAbsorb
          newArmor     = hero.fightStats.armor - armorAbsorb
          newHp        = (hero.fightStats.hp - hpDmg).max(0L)
          _           <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = newHp, armor = newArmor))
          _           <- renderer.show(user, Screen(
                           content.format("battle.mobHit", "damage" -> rawDamage.toString, "monster" -> monster.name), Nil))
          r <- if (newHp <= 0) heroDeath(user, renderer)
               else heroDao.clearActiveBattle(user.userId) *>
                      renderer.show(user, Screen(content.text("battle.fled"), Nil)).as(StateType.Dungeon)
        } yield r
      } else {
        heroDao.clearActiveBattle(user.userId) *>
          renderer.show(user, Screen(content.text("battle.fled"), Nil)).as(StateType.Dungeon)
      }
    } yield result

  private def useFlask(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      result <- if (hero.equipment.flask.itemType == pangea.model.item.ItemType.NoItem)
                  renderer.show(user, Screen(content.text("battle.noFlask"), Nil))
                    .as(StateType.Battle)
                else if (hero.flaskCharges <= 0)
                  renderer.show(user, Screen(content.text("battle.flaskEmpty"), Nil))
                    .as(StateType.Battle)
                else if (battle.flaskUsedThisRound)
                  renderer.show(user, Screen(content.text("battle.flaskAlreadyUsed"), Nil))
                    .as(StateType.Battle)
                else {
                  val maxHp    = hero.effectiveMaxHp(now)
                  val healAmt  = (maxHp / 4L).max(1L)
                  val newHp    = (hero.fightStats.hp + healAmt).min(maxHp)
                  val healed   = newHp - hero.fightStats.hp
                  val newStats = hero.fightStats.copy(hp = newHp)
                  for {
                    _ <- heroDao.updateFightStats(user.userId, newStats)
                    _ <- heroDao.updateFlaskCharges(user.userId, hero.flaskCharges - 1)
                    _ <- heroDao.writeActiveBattle(user.userId, battle.copy(flaskUsedThisRound = true).asJson)
                    _ <- renderer.show(user, Screen(
                           content.format("battle.flaskUsed",
                             "healed" -> healed.toString,
                             "hp"     -> newHp.toString,
                             "max"    -> maxHp.toString), Nil))
                    _ <- showScreen(user, renderer)
                  } yield StateType.Battle
                }
    } yield result

  private def showScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      _      <- renderer.show(user, buildBattleScreen(hero, battle, hero.effectiveMaxHp(now), now))
    } yield ()

  private def buildBattleScreen(hero: Hero, battle: ActiveBattle, maxHp: Long, nowMs: Long): Screen = {
    val eff             = hero.effectiveFightStats(nowMs)
    val buffedEff       = battle.heroBattleState.applyTo(eff)
    val mobHitPct       = (mobHitChance(hero.baseStats.agi, buffedEff.evasion, battle) * 100).toInt
    val monsterDodgePct = ((1.0 - playerHitChance(buffedEff.accuracy, battle)) * 100).toInt
    val text = content.format("battle.enter.text",
      "monster"      -> battle.toMonster.name,
      "monsterRace"  -> battle.toMonster.race.toString,
      "monsterHp"    -> battle.monsterCurrentHp.toString,
      "monsterMax"   -> battle.monsterStats.hp.toString,
      "monsterArmor"    -> battle.monsterCurrentArmor.toString,
      "monsterMaxArmor" -> battle.monsterStats.armor.toString,
      "monsterAtk"      -> battle.monsterStats.atk.toString,
      "mobHit"       -> s"$mobHitPct%",
      "monsterDodge" -> s"$monsterDodgePct%",
      "heroHp"       -> hero.fightStats.hp.toString,
      "heroMax"      -> maxHp.toString,
      "heroArmor"    -> hero.fightStats.armor.toString,
      "heroMaxArmor" -> hero.maxArmor.toString,
      "flaskCharges" -> hero.flaskCharges.toString
    )
    Screen(text, content.screen("battle.enter").choices)
  }

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getBattle(user: User): Task[ActiveBattle] =
    heroDao.readActiveBattle(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No active battle for user ${user.userId}"))
      .flatMap(json => ZIO.fromEither(json.as[ActiveBattle]))

  private def playerHitChance(accuracy: Long, battle: ActiveBattle): Double =
    BattleState.playerHitChance(accuracy, battle.monsterStats.evasion)

  private def mobHitChance(agi: Long, evasion: Long, battle: ActiveBattle): Double =
    BattleState.mobHitChance(agi, evasion, battle.monsterStats.accuracy)
}

object BattleState {
  def computeLevel(totalExp: Long): Long = {
    var remaining = totalExp
    var level     = 1L
    while (remaining >= level * 100 && level < 150) {
      remaining -= level * 100
      level     += 1
    }
    level
  }

  def playerHitChance(accuracy: Long, monsterEvasion: Long): Double =
    (accuracy.toDouble / monsterEvasion.toDouble).max(0.05).min(0.95)

  def mobHitChance(agi: Long, evasion: Long, monsterAccuracy: Long): Double = {
    val evade = agi * 2.5 + evasion.toDouble
    (monsterAccuracy.toDouble / evade).max(0.05).min(0.95)
  }
}
