package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.LootGenerator
import pangea.model.battle.ActiveBattle
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState
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
      mobDodge   = mobDodgeChance(buffedEff.accuracy, battle)
      heroHitPct = (100.0 - mobDodge).toInt

      result <- if (hitRoll > mobDodge) { // моб не увернулся → игрок попал
        for {
          spread <- Random.nextLongBetween(80L, 121L)
          noWeapon = hero.equipment.weapon.itemType == pangea.model.item.ItemType.NoItem
          weaponMod: Double = if (noWeapon) 0.5 else 1.0
          concBonus: Double = 1.0 + buffedEff.concentration * 0.005
          damage    = (((buffedEff.atk + hero.baseStats.str) * spread / 100L) * weaponMod * concBonus).toLong.max(1L)
          playerLine = content.format("battle.hit", "damage" -> damage.toString, "monster" -> monster.name)
          armorDmg  = math.min(battle.monsterCurrentArmor, damage)
          hpDmg     = damage - armorDmg
          newArmor  = battle.monsterCurrentArmor - armorDmg
          newHp     = (battle.monsterCurrentHp - hpDmg).max(0L)
          r <- if (newHp <= 0)
                 renderer.show(user, Screen(playerLine, Nil)) *> victory(user, hero, battle, renderer)
               else
                 monsterAttacks(user, hero, battle.copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor), now, renderer, playerLine)
        } yield r
      } else {
        val playerLine = content.format("battle.miss", "chance" -> heroHitPct.toString)
        monsterAttacks(user, hero, battle, now, renderer, playerLine)
      }
    } yield result

  /**
   * Ход моба после атаки игрока. `playerLine` — заранее посчитанная строка о
   * результате удара игрока; mob-line склеивается с ней и выводится одним
   * сообщением, чтобы избежать спама.
   */
  private def monsterAttacks(
    user:       User,
    hero:       Hero,
    battle:     ActiveBattle,
    nowMs:      Long,
    renderer:   Renderer,
    playerLine: String
  ): Task[StateType] =
    for {
      ticked   <- ZIO.succeed(battle.tickBuffs)
      _        <- heroDao.writeActiveBattle(user.userId, ticked.asJson)
      eff       = hero.effectiveFightStats(nowMs)
      buffedEff = ticked.heroBattleState.applyTo(eff)
      monster   = ticked.toMonster
      hitRoll  <- Random.nextIntBetween(1, 101)
      dodge     = playerDodgeChance(hero.baseStats.agi, buffedEff.evasion, buffedEff.defence, ticked)
      mobHitPct = (100.0 - dodge).toInt

      result <- if (hitRoll > dodge) { // игрок не увернулся → моб попал
        for {
          spread      <- Random.nextLongBetween(80L, 121L)
          rawDamage    = (monster.fightStats.atk * spread / 100L).max(1L)
          buffReduct   = math.min(ticked.heroBattleState.armorBonus, rawDamage)
          afterBuff    = rawDamage - buffReduct
          curArmor     = hero.fightStats.armor.max(0L) // текущая броня тратится как есть; травма режет её ПОТОЛОК (refill), не текущий запас
          armorAbsorb  = math.min(curArmor, afterBuff)
          hpDmg        = afterBuff - armorAbsorb
          newArmor     = curArmor - armorAbsorb
          newHp        = (hero.fightStats.hp - hpDmg).max(0L)
          newStats     = hero.fightStats.copy(hp = newHp, armor = newArmor)
          _           <- heroDao.updateFightStats(user.userId, newStats)
          mobLine      = content.format("battle.mobHit", "damage" -> rawDamage.toString, "monster" -> monster.name)
          _           <- renderer.show(user, Screen(playerLine + "\n\n" + mobLine, Nil))
          r <- if (newHp <= 0) heroDeath(user, renderer)
               else showScreen(user, renderer).as(StateType.Battle)
        } yield r
      } else {
        val mobLine = content.format("battle.mobMiss", "monster" -> monster.name, "chance" -> mobHitPct.toString)
        renderer.show(user, Screen(playerLine + "\n\n" + mobLine, Nil)) *>
          showScreen(user, renderer).as(StateType.Battle)
      }
    } yield result

  private def victory(user: User, hero: Hero, battle: ActiveBattle, renderer: Renderer): Task[StateType] =
    for {
      expGained        <- ZIO.succeed((hero.dungeonLevel.toLong * battle.rarity.factor).toLong.max(1L))
      leveled           = hero.gainExp(expGained)
      // лут катаем чистым ядром; начисление (инвентарь/золото) — в LootState
      seed             <- Random.nextLong
      monster           = battle.toMonster
      (drops, _)        = LootGenerator.roll(battle.rarity, monster.race, hero.dungeonLevel.toLong, Rng(seed))
      lootData          = LootState.LootData(
                            items = drops.collect {
                              case LootGenerator.LootDrop.Gear(i)   => i
                              case LootGenerator.LootDrop.Trophy(i) => i
                            },
                            golds = drops.collect { case LootGenerator.LootDrop.Gold(a, _) => a }
                          )
      _                <- heroDao.clearActiveBattle(user.userId)
      _                <- heroDao.updateExpAndLevel(user.userId, leveled.exp, leveled.lvl, leveled.upgradePoints)
      _                <- heroDao.writeSceneData(user.userId, lootData.asJson)
      // победа над «Отмеченным тьмой» на текущем этаже открывает путь вглубь
      newMaxDungeon     = math.min(150, hero.dungeonLevel + 1)
      unlocksDarkness   = battle.monsterMarked && newMaxDungeon > hero.maxDungeonLevel
      _                <- ZIO.when(unlocksDarkness)(heroDao.updateMaxDungeonLevel(user.userId, newMaxDungeon))
      _                <- renderer.show(user, Screen(
                            content.format("battle.victory",
                              "monster" -> monster.name,
                              "exp"     -> expGained.toString), Nil))
      _                <- ZIO.when(unlocksDarkness)(
                            renderer.show(user, Screen(content.text("battle.darknessConquered"), Nil))
                          )
      _                <- ZIO.when(leveled.lvl > hero.lvl)(
                            renderer.show(user, Screen(s"Вы получили новый уровень ${leveled.lvl}!", Nil))
                          )
    } yield StateType.Loot

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

      result <- if (hitRoll > playerDodgeChance(hero.baseStats.agi, buffedEff.evasion, buffedEff.defence, battle)) {
        for {
          spread      <- Random.nextLongBetween(80L, 121L)
          rawDamage    = (monster.fightStats.atk * spread / 100L).max(1L)
          buffReduct   = math.min(battle.heroBattleState.armorBonus, rawDamage)
          afterBuff    = rawDamage - buffReduct
          curArmor     = hero.fightStats.armor.max(0L)
          armorAbsorb  = math.min(curArmor, afterBuff)
          hpDmg        = afterBuff - armorAbsorb
          newArmor     = curArmor - armorAbsorb
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
      flask   = hero.equipment.flask
      result <- if (flask.itemType == pangea.model.item.ItemType.NoItem)
                  renderer.show(user, Screen(content.text("battle.noFlask"), Nil))
                    .as(StateType.Battle)
                else if (flask.charges.forall(_ <= 0))
                  renderer.show(user, Screen(content.text("battle.flaskEmpty"), Nil))
                    .as(StateType.Battle)
                else if (battle.flaskUsedThisRound)
                  renderer.show(user, Screen(content.text("battle.flaskAlreadyUsed"), Nil))
                    .as(StateType.Battle)
                else {
                  val spentFlask   = flask.copy(charges = flask.charges.map(_ - 1))
                  val newEquipment = hero.equipment.copy(flask = spentFlask)
                  val updatedBattle = battle.copy(flaskUsedThisRound = true)
                  flask.flaskEffect match {
                    case Some(pangea.model.item.FlaskEffect.HealPercent(pct)) =>
                      val maxHp    = hero.effectiveMaxHp(now)
                      val healAmt  = ((maxHp * pct / 100L)).max(1L)
                      val newHp    = (hero.fightStats.hp + healAmt).min(maxHp)
                      val healed   = newHp - hero.fightStats.hp
                      val newStats = hero.fightStats.copy(hp = newHp)
                      for {
                        _ <- heroDao.updateEquipmentAndFightStats(user.userId, newEquipment, newStats)
                        _ <- heroDao.writeActiveBattle(user.userId, updatedBattle.asJson)
                        _ <- renderer.show(user, Screen(
                               content.format("battle.flaskUsed",
                                 "healed" -> healed.toString,
                                 "hp"     -> newHp.toString,
                                 "max"    -> maxHp.toString), Nil))
                        _ <- showScreen(user, renderer)
                      } yield StateType.Battle
                    case Some(pangea.model.item.FlaskEffect.AddBuff(buff, rounds)) =>
                      val timedBuff    = buff.copy(turnsLeft = Some(rounds))
                      val newBattle    = updatedBattle.copy(heroBattleState = updatedBattle.heroBattleState.add(timedBuff))
                      for {
                        _ <- heroDao.updateEquipment(user.userId, newEquipment)
                        _ <- heroDao.writeActiveBattle(user.userId, newBattle.asJson)
                        _ <- renderer.show(user, Screen(content.text("battle.flaskBuff"), Nil))
                        _ <- showScreen(user, renderer)
                      } yield StateType.Battle
                    case None =>
                      renderer.show(user, Screen(content.text("battle.flaskNoEffect"), Nil))
                        .as(StateType.Battle)
                  }
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
    val playerDodgePct  = playerDodgeChance(hero.baseStats.agi, buffedEff.evasion, buffedEff.defence, battle).toInt
    val monsterDodgePct = mobDodgeChance(buffedEff.accuracy, battle).toInt
    val mobHitPct       = 100 - playerDodgePct
    val heroHitPct      = 100 - monsterDodgePct
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
      "heroHit"      -> s"$heroHitPct%",
      "heroDodge"    -> s"$playerDodgePct%",
      "heroHp"       -> hero.fightStats.hp.toString,
      "heroMax"      -> maxHp.toString,
      "heroArmor"    -> hero.fightStats.armor.min(hero.effectiveMaxArmor(nowMs)).toString,
      "heroMaxArmor" -> hero.effectiveMaxArmor(nowMs).toString,
      "flaskCharges" -> hero.equipment.flask.charges.getOrElse(0).toString
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

  // Уклонение игрока от удара моба: защита игрока в знаменателе, точность моба ×1.5.
  private def playerDodgeChance(agi: Long, evasion: Long, defence: Long, battle: ActiveBattle): Double =
    BattleState.dodgeChance(agi, evasion, defence, battle.monsterStats.accuracy)

  // Уклонение моба от удара игрока — та же формула: у моба нет ловкости (agi = 0),
  // в знаменателе его защита и точность игрока ×1.5.
  private def mobDodgeChance(heroAccuracy: Long, battle: ActiveBattle): Double =
    BattleState.dodgeChance(0L, battle.monsterStats.evasion, battle.monsterStats.defence, heroAccuracy)
}

object BattleState {

  /**
   * Шанс уклонения защищающегося юнита от удара атакующего, в процентах, зажат в [5, 95]:
   *   100 * (agi + evasion) / (agi + evasion + defence * 1 + attackerAccuracy * 1.5)
   * Единая логика «попадания по юниту» для обеих сторон: положительные параметры
   * (числитель) — ловкость и уклонение защищающегося; отрицательные (знаменатель) —
   * защита защищающегося (×1) и точность атакующего (×1.5). Атакующий попадает,
   * если бросок 1..100 больше уклонения.
   */
  def dodgeChance(agi: Long, evasion: Long, defence: Long, attackerAccuracy: Long): Double = {
    val positive = (agi + evasion).toDouble
    val denom    = positive + defence * 1.0 + attackerAccuracy * 1.5
    val raw      = if (denom <= 0.0) 0.0 else 100.0 * positive / denom
    raw.max(5.0).min(95.0)
  }
}
