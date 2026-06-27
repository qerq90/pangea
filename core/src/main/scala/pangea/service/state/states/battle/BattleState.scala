package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.LootGenerator
import pangea.model.battle.{ActiveBattle, SkillSlotState}
import pangea.model.hero.Hero
import pangea.model.skill.{MonsterSkill, Skill}
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
    fallback = Target.Run { (user, ua, renderer) =>
      // Кнопки способностей именуются Skill_<itemId> и роутятся динамически: id
      // указывает на конкретный предмет, поэтому два предмета с «одним» Skill
      // имеют независимые cd/uses.
      BattleState.parseSkillAction(ua) match {
        case Some(itemId) => useSkill(user, renderer, itemId)
        case None         => showScreen(user, renderer).as(StateType.Battle)
      }
    }
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
      result  <- playerBasicAttack(user, hero, battle, now, renderer, prefix = "", skipSlots = Set.empty)
    } yield result

  /** Базовая атака игрока. `prefix` приклеивается перед строкой об атаке —
   *  используется, когда базовая атака идёт после применения скилла (тогда
   *  prefix содержит описание самого скилла). `skipSlots` пробрасывается в
   *  `monsterAttacks → tickBuffs`, чтобы только что использованный слот не
   *  тикался в этом же ходу (см. ActiveBattle.tickBuffs). */
  private def playerBasicAttack(
    user:      User,
    hero:      Hero,
    battle:    ActiveBattle,
    nowMs:     Long,
    renderer:  Renderer,
    prefix:    String,
    skipSlots: Set[Long]
  ): Task[StateType] =
    for {
      eff       <- ZIO.succeed(hero.effectiveFightStats(nowMs))
      buffedEff  = battle.heroBattleState.applyTo(eff)
      monster    = battle.toMonster
      hitRoll   <- Random.nextIntBetween(1, 101)
      mobDodge   = mobDodgeChance(buffedEff.accuracy, battle)
      heroHitPct = (100.0 - mobDodge).toInt
      result <- if (hitRoll > mobDodge) {
        for {
          spread <- Random.nextLongBetween(80L, 121L)
          noWeapon = hero.equipment.weapon.itemType == pangea.model.item.ItemType.NoItem
          weaponMod: Double = if (noWeapon) 0.5 else 1.0
          damage    = (((buffedEff.atk + hero.baseStats.str) * spread / 100L) * weaponMod).toLong.max(1L)
          attackLine = content.format("battle.hit", "damage" -> damage.toString, "monster" -> monster.name)
          armorDmg  = math.min(battle.monsterCurrentArmor, damage)
          hpDmg     = damage - armorDmg
          newArmor  = battle.monsterCurrentArmor - armorDmg
          newHp     = (battle.monsterCurrentHp - hpDmg).max(0L)
          updated   = battle.copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor)
          r <- if (newHp <= 0)
                 heroDao.writeActiveBattle(user.userId, updated.asJson) *>
                   renderer.show(user, Screen(prefix + attackLine, Nil)) *>
                   victory(user, hero, updated, renderer)
               else
                 monsterAttacks(user, hero, updated, nowMs, renderer, prefix + attackLine, skipSlots)
        } yield r
      } else {
        val attackLine = content.format("battle.miss", "chance" -> heroHitPct.toString)
        monsterAttacks(user, hero, battle, nowMs, renderer, prefix + attackLine, skipSlots)
      }
    } yield result

  /** Применение активного навыка. Логика:
   *   - проверяем, что слот существует, готов (cd=0) и связан с экипированным предметом;
   *   - кидаем шанс попадания умением (см. `heroSkillHitChance`);
   *   - на попадании: применяем эффект (Damage/Heal) с ±20% разбросом, ставим cd
   *     и инкрементируем uses;
   *   - на промахе: cd не выставляется, uses не растёт; но ход завершается (как
   *     обычная атака — далее моб бьёт).
   */
  private def useSkill(user: User, renderer: Renderer, itemId: Long): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      result <- battle.slotByItem(itemId) match {
        case None =>
          renderer.show(user, Screen(content.text("battle.skillUnavailable"), Nil)) *>
            showScreen(user, renderer).as(StateType.Battle)
        case Some(slot) if slot.cooldown > 0 =>
          renderer.show(user, Screen(content.text("battle.skillOnCooldown"), Nil)) *>
            showScreen(user, renderer).as(StateType.Battle)
        case Some(slot) =>
          castSkill(user, hero, battle, slot, now, renderer)
      }
    } yield result

  private def castSkill(
    user:     User,
    hero:     Hero,
    battle:   ActiveBattle,
    slot:     SkillSlotState,
    nowMs:    Long,
    renderer: Renderer
  ): Task[StateType] =
    for {
      effHero  <- ZIO.succeed(hero.effectiveFightStats(nowMs))
      buffed    = battle.heroBattleState.applyTo(effHero)
      hitChance = BattleState.heroSkillHitChance(
                    heroConc     = buffed.concentration,
                    heroInt      = hero.baseStats.int,
                    monsterConc  = battle.monsterStats.concentration,
                    rarityFactor = battle.rarity.factor)
      roll     <- Random.nextIntBetween(1, 101)
      hitPct    = hitChance.toInt
      result   <- if (roll <= hitChance) skillHit(user, hero, battle, slot, nowMs, renderer)
                  else                   skillMiss(user, hero, battle, slot, nowMs, renderer, hitPct)
    } yield result

  // После применения скилла (hit/miss) — всегда идёт базовая атака игрока, как
  // и у моба сейчас. Кулдаун использованного слота не должен тикаться в этом же
  // ходу — передаём его в `skipSlots` цепочке вызовов (см. tickBuffs).
  private def skillHit(
    user:     User,
    hero:     Hero,
    battle:   ActiveBattle,
    slot:     SkillSlotState,
    nowMs:    Long,
    renderer: Renderer
  ): Task[StateType] =
    for {
      spread <- Random.nextLongBetween(80L, 121L)
      base    = slot.skill.baseValue(hero, nowMs)
      value   = (base * spread / 100.0).toLong.max(1L)
      bumpedBattle = battle.updateSlot(slot.itemId)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1))
      skip    = Set(slot.itemId)
      result <- slot.skill.kind match {
        case Skill.Kind.Damage =>
          val skillLine = slot.skill.hitTemplate.replace("{}", value.toString)
          val armorDmg  = math.min(battle.monsterCurrentArmor, value)
          val hpDmg     = value - armorDmg
          val newArmor  = battle.monsterCurrentArmor - armorDmg
          val newHp     = (battle.monsterCurrentHp - hpDmg).max(0L)
          val updated   = bumpedBattle.copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor)
          if (newHp <= 0)
            // Моб убит самим скиллом — базовая атака не нужна.
            heroDao.writeActiveBattle(user.userId, updated.asJson) *>
              renderer.show(user, Screen(skillLine, Nil)) *>
              victory(user, hero, updated, renderer)
          else
            playerBasicAttack(user, hero, updated, nowMs, renderer, prefix = skillLine + "\n", skipSlots = skip)

        case Skill.Kind.Heal =>
          val maxHp     = hero.effectiveMaxHp(nowMs)
          val newHp     = (hero.fightStats.hp + value).min(maxHp)
          val healed    = newHp - hero.fightStats.hp
          val newStats  = hero.fightStats.copy(hp = newHp)
          val skillLine = slot.skill.hitTemplate.replace("{}", healed.toString)
          heroDao.updateFightStats(user.userId, newStats) *>
            playerBasicAttack(user, hero.copy(fightStats = newStats), bumpedBattle, nowMs, renderer,
                              prefix = skillLine + "\n", skipSlots = skip)
      }
    } yield result

  private def skillMiss(
    user:     User,
    hero:     Hero,
    battle:   ActiveBattle,
    slot:     SkillSlotState,
    nowMs:    Long,
    renderer: Renderer,
    hitPct:   Int
  ): Task[StateType] = {
    // Промах: cd НЕ выставляется (uses не растёт); ход всё равно завершается —
    // делаем базовую атаку и ход моба.
    val skillLine = content.format("battle.skillMiss", "skill" -> slot.skill.label, "chance" -> hitPct.toString)
    playerBasicAttack(user, hero, battle, nowMs, renderer, prefix = skillLine + "\n", skipSlots = Set.empty)
  }

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
    playerLine: String,
    skipSlots:  Set[Long] = Set.empty
  ): Task[StateType] =
    for {
      ticked   <- ZIO.succeed(battle.tickBuffs(skipSlots))
      eff       = hero.effectiveFightStats(nowMs)
      buffedEff = ticked.heroBattleState.applyTo(eff)
      monster   = ticked.toMonster
      hitRoll  <- Random.nextIntBetween(1, 101)
      dodge     = playerDodgeChance(hero.baseStats.agi, buffedEff.evasion, buffedEff.defence, ticked)
      mobHitPct = (100.0 - dodge).toInt

      // 1) Обычная атака моба — обновляем hp/armor героя.
      atkResult <- if (hitRoll > dodge) {
        for {
          spread      <- Random.nextLongBetween(80L, 121L)
          rawDamage    = (monster.fightStats.atk * spread / 100L).max(1L)
          reduction    = BattleState.damageReduction(
                           protection      = buffedEff.defence,
                           defenderInt     = hero.baseStats.int,
                           attackerInt     = monster.fightStats.concentration
                         )
          reducedDamage = (rawDamage * (1.0 - reduction)).toLong.max(1L)
          (newHp, newArmor) = MonsterSkill.applyPhysicalDamage(ticked, hero, reducedDamage)
        } yield (newHp, newArmor, content.format("battle.mobHit",
                  "damage" -> reducedDamage.toString, "monster" -> monster.name))
      } else ZIO.succeed((hero.fightStats.hp, hero.fightStats.armor,
                          content.format("battle.mobMiss", "monster" -> monster.name, "chance" -> mobHitPct.toString)))
      (hpAfterAtk, armorAfterAtk, mobLine) = atkResult
      heroAfterAtk = hero.copy(fightStats = hero.fightStats.copy(hp = hpAfterAtk, armor = armorAfterAtk))

      // 2) Каст скилла моба — независимый бросок шанса применения умения. Если
      // прокнул — равновероятно выбираем applicable скилл из 4. Скилл применяется
      // ПОВЕРХ обычной атаки и может быть как уроном (CrushingStrike — мимо брони
      // и damageReduction), так и хилом/починкой моба.
      castResult <- if (heroAfterAtk.fightStats.hp <= 0) ZIO.succeed((ticked, heroAfterAtk, ""))
        else for {
          skillRoll  <- Random.nextIntBetween(1, 101)
          skillChance = BattleState.monsterSkillHitChance(
                          monsterConc  = monster.fightStats.concentration,
                          rarityFactor = ticked.rarity.factor,
                          heroConc     = buffedEff.concentration,
                          heroInt      = hero.baseStats.int)
          applicable  = MonsterSkill.values.filter(_.applicable(ticked)).toVector
          out <- if (skillRoll <= skillChance && applicable.nonEmpty)
                   for {
                     idx <- Random.nextIntBetween(0, applicable.size)
                     ms   = applicable(idx)
                     cast = ms.cast(ticked, heroAfterAtk, nowMs)
                   } yield (cast.battle, heroAfterAtk.copy(
                              fightStats = heroAfterAtk.fightStats.copy(hp = cast.heroHp, armor = cast.heroArmor)),
                            "\n" + cast.line)
                 else ZIO.succeed((ticked, heroAfterAtk, ""))
        } yield out
      (finalBattle, finalHero, skillLine) = castResult

      _ <- heroDao.updateFightStats(user.userId, finalHero.fightStats)
      _ <- heroDao.writeActiveBattle(user.userId, finalBattle.asJson)
      _ <- renderer.show(user, Screen(playerLine + "\n\n" + mobLine + skillLine, Nil))

      r <- if (finalHero.fightStats.hp <= 0) heroDeath(user, renderer)
           else showScreen(user, renderer).as(StateType.Battle)
    } yield r

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
          reduction    = BattleState.damageReduction(
                           protection      = buffedEff.defence,
                           defenderInt     = hero.baseStats.int,
                           attackerInt     = monster.fightStats.concentration
                         )
          reducedDamage = (rawDamage * (1.0 - reduction)).toLong.max(1L)
          buffReduct   = math.min(battle.heroBattleState.armorBonus, reducedDamage)
          afterBuff    = reducedDamage - buffReduct
          curArmor     = hero.fightStats.armor.max(0L)
          armorAbsorb  = math.min(curArmor, afterBuff)
          hpDmg        = afterBuff - armorAbsorb
          newArmor     = curArmor - armorAbsorb
          newHp        = (hero.fightStats.hp - hpDmg).max(0L)
          _           <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = newHp, armor = newArmor))
          _           <- renderer.show(user, Screen(
                           content.format("battle.mobHit", "damage" -> reducedDamage.toString, "monster" -> monster.name), Nil))
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
    val reductionPct    = (BattleState.damageReduction(
                            protection  = buffedEff.defence,
                            defenderInt = hero.baseStats.int,
                            attackerInt = battle.monsterStats.concentration
                          ) * 100.0).toInt
    val heroSkillHitPct = BattleState.heroSkillHitChance(
                            heroConc     = buffedEff.concentration,
                            heroInt      = hero.baseStats.int,
                            monsterConc  = battle.monsterStats.concentration,
                            rarityFactor = battle.rarity.factor).toInt
    val mobSkillHitPct  = BattleState.monsterSkillHitChance(
                            monsterConc  = battle.monsterStats.concentration,
                            rarityFactor = battle.rarity.factor,
                            heroConc     = buffedEff.concentration,
                            heroInt      = hero.baseStats.int).toInt
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
      "heroReduction" -> s"$reductionPct%",
      "heroSkillHit"  -> s"$heroSkillHitPct%",
      "mobSkillHit"   -> s"$mobSkillHitPct%",
      "flaskCharges" -> hero.equipment.flask.charges.getOrElse(0).toString
    )
    val skillButtons = battle.skillSlots.collect {
      case slot if slot.cooldown <= 0 =>
        pangea.engine.Choice(
          id    = s"Skill_${slot.itemId}",
          label = slot.skill.label,
          color = pangea.engine.ChoiceColor.Negative
        )
    }
    Screen(text, skillButtons ++ content.screen("battle.enter").choices)
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

  /** Шанс игрока попасть способностью по мобу, в процентах [5; 95]:
   *   100 · (heroConc · 0.75·heroInt) / (0.5·monsterConc · monster.rarity.factor). */
  def heroSkillHitChance(heroConc: Long, heroInt: Long, monsterConc: Long, rarityFactor: Double): Double = {
    val numer = heroConc.toDouble * (0.75 * heroInt.toDouble)
    val denom = (0.5 * monsterConc.toDouble) * rarityFactor.max(0.0001)
    val raw   = if (denom <= 0.0) 100.0 else 100.0 * numer / denom
    raw.max(5.0).min(95.0)
  }

  /** Шанс моба попасть способностью по игроку, в процентах [5; 95]:
   *   100 · (monsterConc · rarity.factor) / (0.25·heroConc · 0.75·heroInt). */
  def monsterSkillHitChance(monsterConc: Long, rarityFactor: Double, heroConc: Long, heroInt: Long): Double = {
    val numer = monsterConc.toDouble * rarityFactor
    val denom = (0.25 * heroConc.toDouble) * (0.75 * heroInt.toDouble)
    val raw   = if (denom <= 0.0) 100.0 else 100.0 * numer / denom
    raw.max(5.0).min(95.0)
  }

  /** Из payload UserAction достаём itemId, если action имеет вид `Skill_<id>`. */
  def parseSkillAction(ua: pangea.service.state.UserAction): Option[Long] = {
    val key = ua.payload
      .flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))
      .getOrElse("")
    key match {
      case s"Skill_$rest" => rest.toLongOption
      case _              => None
    }
  }

  /**
   * Шанс уклонения защищающегося юнита от удара атакующего, в процентах, зажат в [5, 95]:
   *   100 * (agi + evasion) / (agi + evasion + defence * 1 + attackerAccuracy * 1.5)
   * Единая логика «попадания по юниту» для обеих сторон: положительные параметры
   * (числитель) — ловкость и уклонение защищающегося; отрицательные (знаменатель) —
   * защита защищающегося (×1) и точность атакующего (×1.5). Атакующий попадает,
   * если бросок 1..100 больше уклонения.
   */
  /**
   * Процентное снижение урона, наносимого защищающемуся юниту:
   *   reduction = (P + Iₚ) / (P + Iₚ + Iₑ × 2),  зажато сверху 0.7.
   * Где `P` — защита защитника, `Iₚ` — его интеллект, `Iₑ` — интеллект
   * атакующего. Применяется к чистому урону до брони. Соотношение «×2 для
   * интеллекта атакующего» — из ТЗ. Возврат — доля [0; 0.7].
   */
  def damageReduction(protection: Long, defenderInt: Long, attackerInt: Long): Double = {
    val numer = (protection + defenderInt).toDouble
    val denom = numer + attackerInt * 2.0
    val raw   = if (denom <= 0.0) 0.0 else numer / denom
    raw.max(0.0).min(0.7)
  }

  def dodgeChance(agi: Long, evasion: Long, defence: Long, attackerAccuracy: Long): Double = {
    val positive = (agi + evasion).toDouble
    val denom    = positive + defence * 1.0 + attackerAccuracy * 1.5
    val raw      = if (denom <= 0.0) 0.0 else 100.0 * positive / denom
    raw.max(5.0).min(95.0)
  }
}
