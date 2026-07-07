package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.LootGenerator
import pangea.model.battle.{Buff, Regen, SoloPveBattle, SkillSlotState}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{FlaskEffect, ItemDetails, PotionKind}
import pangea.model.stats.FightStats
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
      "Attack" -> Target.Run((user, _, renderer) => attack(user, renderer)),
      "UseFlask" -> Target.Run { (user, _, renderer) =>
        useFlask(user, renderer)
      },
      "UseBelt" -> Target.Run { (user, _, renderer) =>
        useBelt(user, renderer)
      },
      "Flee" -> Target.Run((user, _, renderer) => flee(user, renderer)),
      "ConfirmFlee" -> Target.Run { (user, _, renderer) =>
        confirmFlee(user, renderer)
      },
      "CancelFlee" -> Target.Run { (user, _, renderer) =>
        showScreen(user, renderer).as(StateType.Battle)
      }
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

  override def action(
      user: User,
      ua: UserAction,
      renderer: Renderer
  ): Task[StateType] = branch.act(user, ua, renderer)

  private def attack(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      result <- playerBasicAttack(
        user,
        hero,
        battle,
        now,
        renderer,
        prefix = "",
        skipSlots = Set.empty
      )
    } yield result

  /** Базовая атака игрока. `prefix` приклеивается перед строкой об атаке —
    * используется, когда базовая атака идёт после применения скилла (тогда
    * prefix содержит описание самого скилла). `skipSlots` пробрасывается в
    * `monsterAttacks → tickBuffs`, чтобы только что использованный слот не
    * тикался в этом же ходу (см. SoloPveBattle.tickBuffs).
    */
  private def playerBasicAttack(
      user: User,
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      renderer: Renderer,
      prefix: String,
      skipSlots: Set[Long]
  ): Task[StateType] =
    for {
      eff <- ZIO.succeed(hero.effectiveFightStats(nowMs))
      buffedEff = battle.heroBattleState.applyTo(eff)
      monster   = battle.toMonster
      hitRoll <- Random.nextIntBetween(1, 101)
      mobDodge   = mobDodgeChance(buffedEff.accuracy, battle)
      // Попадание считается как hitRoll(1..100) > dodge, т.е. фактический шанс =
      // 100 - floor(dodge). Округляем ТАК ЖЕ, как экран боя (см. buildBattleScreen),
      // иначе при дробном dodge сообщение расходится с экраном на 1.
      heroHitPct = 100 - mobDodge.toInt
      result <-
        if (hitRoll > mobDodge) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            noWeapon =
              hero.equipment.weapon.itemType == pangea.model.item.ItemType.NoItem
            weaponMod: Double = if (noWeapon) 0.5 else 1.0
            damage =
              (((hero.effectiveBaseStats(nowMs).str * 3L + buffedEff.atk) * spread / 100L) * weaponMod).toLong
                .max(1L)
            attackLine = content.format(
              "battle.hit",
              "damage"  -> damage.toString,
              "monster" -> monster.name
            )
            armorDmg = math.min(battle.monsterCurrentArmor, damage)
            hpDmg    = damage - armorDmg
            newArmor = battle.monsterCurrentArmor - armorDmg
            newHp    = (battle.monsterCurrentHp - hpDmg).max(0L)
            // Баф «ядовитые атаки»: на любом попадании снимается. Моб травится, только
            // если удар прошёл в HP (hpDmg > 0) и моб жив: повторное попадание стакает
            // текущий яд (+OnHit), иначе накладывает свежий.
            poisonsNow = battle.effects.heroPoisonousAttacks && hpDmg > 0 && newHp > 0
            effects =
              if (!battle.effects.heroPoisonousAttacks) battle.effects
              else battle.effects.copy(
                heroPoisonousAttacks = false,
                monsterPoison =
                  if (poisonsNow) Some(battle.effects.monsterPoison.map(_.stacked).getOrElse(pangea.model.battle.Poison.onHit))
                  else battle.effects.monsterPoison
              )
            attackLineFull = attackLine + (if (poisonsNow) "\n" + content.format("battle.poisonApplied", "monster" -> monster.name) else "")
            updated = battle.copy(
              monsterCurrentHp = newHp,
              monsterCurrentArmor = newArmor,
              effects = effects
            )
            r <-
              if (newHp <= 0)
                heroDao.writeActiveBattle(user.userId, updated.asJson) *>
                  renderer.show(user, Screen(prefix + attackLineFull, Nil)) *>
                  victory(user, hero, updated, renderer)
              else
                monsterAttacks(
                  user,
                  hero,
                  updated,
                  nowMs,
                  renderer,
                  prefix + attackLineFull,
                  skipSlots
                )
          } yield r
        } else {
          val attackLine =
            content.format("battle.miss", "chance" -> heroHitPct.toString)
          monsterAttacks(
            user,
            hero,
            battle,
            nowMs,
            renderer,
            prefix + attackLine,
            skipSlots
          )
        }
    } yield result

  /** Применение активного навыка. Логика:
    *   - проверяем, что слот существует, готов (cd=0) и связан с экипированным
    *     предметом;
    *   - кидаем шанс попадания умением (см. `heroSkillHitChance`);
    *   - на попадании: применяем эффект (Damage/Heal) с ±20% разбросом, ставим
    *     cd и инкрементируем uses;
    *   - на промахе: cd не выставляется, uses не растёт; но ход завершается
    *     (как обычная атака — далее моб бьёт).
    */
  private def useSkill(
      user: User,
      renderer: Renderer,
      itemId: Long
  ): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      result <- battle.slotByItem(itemId) match {
        case None =>
          renderer.show(
            user,
            Screen(content.text("battle.skillUnavailable"), Nil)
          ) *>
            showScreen(user, renderer).as(StateType.Battle)
        case Some(slot) if slot.cooldown > 0 =>
          renderer.show(
            user,
            Screen(content.text("battle.skillOnCooldown"), Nil)
          ) *>
            showScreen(user, renderer).as(StateType.Battle)
        case Some(slot) =>
          castSkill(user, hero, battle, slot, now, renderer)
      }
    } yield result

  private def castSkill(
      user: User,
      hero: Hero,
      battle: SoloPveBattle,
      slot: SkillSlotState,
      nowMs: Long,
      renderer: Renderer
  ): Task[StateType] =
    for {
      hitChance <- ZIO.succeed(heroSkillHitChance(hero, battle, nowMs))
      roll <- Random.nextIntBetween(1, 101)
      hitPct = hitChance.toInt
      result <-
        if (roll <= hitChance)
          skillHit(user, hero, battle, slot, nowMs, renderer)
        else skillMiss(user, hero, battle, slot, nowMs, renderer, hitPct)
    } yield result

  // После применения скилла (hit/miss) — всегда идёт базовая атака игрока, как
  // и у моба сейчас. Кулдаун использованного слота не должен тикаться в этом же
  // ходу — передаём его в `skipSlots` цепочке вызовов (см. tickBuffs).
  private def skillHit(
      user: User,
      hero: Hero,
      battle: SoloPveBattle,
      slot: SkillSlotState,
      nowMs: Long,
      renderer: Renderer
  ): Task[StateType] =
    for {
      spread <- Random.nextLongBetween(80L, 121L)
      base = slot.skill.baseValue(hero, nowMs)
      raw  = (base * spread / 100.0).toLong.max(1L)
      // Часть скиллов (см. Skill.appliesDefenceReduction) бьются о защиту моба
      // так же, как обычная атака — иначе урон проходит мимо защиты.
      value =
        if (slot.skill.appliesDefenceReduction) {
          val red = BattleState.damageReduction(
            protection = battle.monsterStats.defence,
            defenderInt = battle.monsterStats.concentration,
            attackerInt = hero.effectiveBaseStats(nowMs).int
          )
          (raw * (1.0 - red)).toLong.max(1L)
        } else raw
      bumpedBattle = battle.updateSlot(slot.itemId)(s =>
        s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1)
      )
      skip = Set(slot.itemId)
      result <- slot.skill.kind match {
        case Skill.Kind.Damage =>
          val skillLine = slot.skill.hitTemplate.replace("{}", value.toString)
          val armorDmg  = math.min(battle.monsterCurrentArmor, value)
          val hpDmg     = value - armorDmg
          val newArmor  = battle.monsterCurrentArmor - armorDmg
          val newHp     = (battle.monsterCurrentHp - hpDmg).max(0L)
          val updated = bumpedBattle.copy(
            monsterCurrentHp = newHp,
            monsterCurrentArmor = newArmor
          )
          if (newHp <= 0)
            // Моб убит самим скиллом — базовая атака не нужна.
            heroDao.writeActiveBattle(user.userId, updated.asJson) *>
              renderer.show(user, Screen(skillLine, Nil)) *>
              victory(user, hero, updated, renderer)
          else
            playerBasicAttack(
              user,
              hero,
              updated,
              nowMs,
              renderer,
              prefix = skillLine + "\n",
              skipSlots = skip
            )

        case Skill.Kind.Heal =>
          val maxHp     = hero.effectiveMaxHp(nowMs)
          val newHp     = (hero.fightStats.hp + value).min(maxHp)
          val healed    = newHp - hero.fightStats.hp
          val newStats  = hero.fightStats.copy(hp = newHp)
          val skillLine = slot.skill.hitTemplate.replace("{}", healed.toString)
          heroDao.updateFightStats(user.userId, newStats) *>
            playerBasicAttack(
              user,
              hero.copy(fightStats = newStats),
              bumpedBattle,
              nowMs,
              renderer,
              prefix = skillLine + "\n",
              skipSlots = skip
            )
      }
    } yield result

  private def skillMiss(
      user: User,
      hero: Hero,
      battle: SoloPveBattle,
      slot: SkillSlotState,
      nowMs: Long,
      renderer: Renderer,
      hitPct: Int
  ): Task[StateType] = {
    // Промах: cd НЕ выставляется (uses не растёт); ход всё равно завершается —
    // делаем базовую атаку и ход моба.
    val skillLine = content.format(
      "battle.skillMiss",
      "skill"  -> slot.skill.label,
      "chance" -> hitPct.toString
    )
    playerBasicAttack(
      user,
      hero,
      battle,
      nowMs,
      renderer,
      prefix = skillLine + "\n",
      skipSlots = Set.empty
    )
  }

  /** Ход моба после атаки игрока. `playerLine` — заранее посчитанная строка о
    * результате удара игрока; mob-line склеивается с ней и выводится одним
    * сообщением, чтобы избежать спама.
    */
  private def monsterAttacks(
      user: User,
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      renderer: Renderer,
      playerLine: String,
      skipSlots: Set[Long]
  ): Task[StateType] =
    for {
      ticked <- ZIO.succeed(battle.tickBuffs(skipSlots))
      eff       = hero.effectiveFightStats(nowMs)
      buffedEff = ticked.heroBattleState.applyTo(eff)
      monster   = ticked.toMonster
      hitRoll <- Random.nextIntBetween(1, 101)
      dodge = playerDodgeChance(
        hero.effectiveBaseStats(nowMs).agi,
        buffedEff.evasion,
        buffedEff.defence,
        ticked
      )
      // Тот же порядок округления, что на экране боя: 100 - floor(dodge)
      // (совпадает с механикой hitRoll(1..100) > dodge). Раньше был (100.0 - dodge).toInt.
      mobHitPct = 100 - dodge.toInt

      // 1) Обычная атака моба — обновляем hp/armor героя.
      atkResult <-
        if (hitRoll > dodge) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monster.fightStats.atk * spread / 100L).max(1L)
            reduction = BattleState.damageReduction(
              protection = buffedEff.defence,
              defenderInt = hero.effectiveBaseStats(nowMs).int,
              attackerInt = monster.fightStats.concentration
            )
            reducedDamage = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            (newHp, newArmor) = MonsterSkill.applyPhysicalDamage(
              ticked,
              hero,
              reducedDamage
            )
          } yield (
            newHp,
            newArmor,
            content.format(
              "battle.mobHit",
              "damage"  -> reducedDamage.toString,
              "monster" -> monster.name
            )
          )
        } else
          ZIO.succeed(
            (
              hero.fightStats.hp,
              hero.fightStats.armor,
              content.format(
                "battle.mobMiss",
                "monster" -> monster.name,
                "chance"  -> mobHitPct.toString
              )
            )
          )
      (hpAfterAtk, armorAfterAtk, mobLine) = atkResult
      heroAfterAtk = hero.copy(fightStats =
        hero.fightStats.copy(hp = hpAfterAtk, armor = armorAfterAtk)
      )

      // 2) Каст скилла моба — независимый бросок шанса применения умения. Если
      // прокнул — равновероятно выбираем applicable скилл из 4. Скилл применяется
      // ПОВЕРХ обычной атаки и может быть как уроном (CrushingStrike — мимо брони
      // и damageReduction), так и хилом/починкой моба.
      castResult <-
        if (heroAfterAtk.fightStats.hp <= 0)
          ZIO.succeed((ticked, heroAfterAtk, ""))
        else
          for {
            skillRoll <- Random.nextIntBetween(1, 101)
            skillChance = BattleState.monsterSkillHitChance(
              monsterConc = monster.fightStats.concentration,
              rarityFactor = ticked.rarity.factor,
              heroConc = buffedEff.concentration,
              heroInt = hero.effectiveBaseStats(nowMs).int
            )
            applicable = MonsterSkill.values
              .filter(_.applicable(ticked))
              .toVector
            out <-
              if (skillRoll <= skillChance && applicable.nonEmpty)
                for {
                  idx <- Random.nextIntBetween(0, applicable.size)
                  ms   = applicable(idx)
                  cast = ms.cast(ticked, heroAfterAtk, nowMs)
                } yield (
                  cast.battle,
                  heroAfterAtk.copy(
                    fightStats = heroAfterAtk.fightStats
                      .copy(hp = cast.heroHp, armor = cast.heroArmor)
                  ),
                  "\n" + cast.line
                )
              else ZIO.succeed((ticked, heroAfterAtk, ""))
          } yield out
      (finalBattle, finalHero, skillLine) = castResult

      // 3) Конец раунда: тик статус-эффектов. Стадии героя и монстра — раздельные,
      // каждая со своей строкой (в будущем UI сможет показать статусы у своих статов).
      // Применяются только пока герой жив.
      heroAlive = finalHero.fightStats.hp > 0
      (tickedHero, battleAfterHero, heroEffectLine) =
        if (heroAlive) tickHeroEffects(finalHero, finalBattle, nowMs)
        else (finalHero, finalBattle, "")
      (tickedBattle, monsterEffectLine) =
        if (heroAlive) tickMonsterEffects(battleAfterHero, monster.name)
        else (battleAfterHero, "")

      _ <- heroDao.updateFightStats(user.userId, tickedHero.fightStats)
      _ <- heroDao.writeActiveBattle(user.userId, tickedBattle.asJson)
      _ <- renderer.show(
        user,
        Screen(playerLine + "\n\n" + mobLine + skillLine + monsterEffectLine + heroEffectLine, Nil)
      )

      r <-
        if (tickedHero.fightStats.hp <= 0) heroDeath(user, renderer)
        else if (tickedBattle.monsterCurrentHp <= 0)
          victory(user, tickedHero, tickedBattle, renderer)
        else showScreen(user, renderer).as(StateType.Battle)
    } yield r

  /** Тик эффектов ГЕРОЯ в конце раунда: реген лечит на `pct`% макс.HP и слабеет.
    * Возвращает обновлённых героя и бой (с ослабленным регеном) плюс строку эффекта. */
  private def tickHeroEffects(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): (Hero, SoloPveBattle, String) =
    battle.effects.heroRegen match {
      case Some(regen) =>
        val maxHp  = hero.effectiveMaxHp(nowMs)
        val heal   = regen.healOn(maxHp)
        val newHp  = (hero.fightStats.hp + heal).min(maxHp)
        val healed = newHp - hero.fightStats.hp
        (
          hero.copy(fightStats = hero.fightStats.copy(hp = newHp)),
          battle.copy(effects = battle.effects.copy(heroRegen = regen.decayed)),
          "\n" + content.format("battle.regenTick", "healed" -> healed.toString)
        )
      case None => (hero, battle, "")
    }

  /** Тик эффектов МОНСТРА в конце раунда: яд снимает `pct`% его макс.HP мимо брони
    * и слабеет. Возвращает обновлённый бой плюс строку эффекта. */
  private def tickMonsterEffects(
      battle: SoloPveBattle,
      monsterName: String
  ): (SoloPveBattle, String) =
    battle.effects.monsterPoison match {
      case Some(poison) =>
        val dmg   = poison.damageOn(battle.monsterStats.hp)
        val newHp = (battle.monsterCurrentHp - dmg).max(0L)
        (
          battle.copy(
            monsterCurrentHp = newHp,
            effects = battle.effects.copy(monsterPoison = poison.decayed)
          ),
          "\n" + content.format("battle.poisonTick", "damage" -> dmg.toString, "monster" -> monsterName)
        )
      case None => (battle, "")
    }

  private def victory(
      user: User,
      hero: Hero,
      battle: SoloPveBattle,
      renderer: Renderer
  ): Task[StateType] =
    for {
      expGained <- ZIO.succeed(
        (hero.dungeonLevel.toLong * battle.rarity.factor).toLong.max(1L)
      )
      leveled = hero.gainExp(expGained)
      // лут катаем чистым ядром; начисление (инвентарь/золото) — в LootState
      seed <- Random.nextLong
      monster = battle.toMonster
      (drops, _) = LootGenerator.roll(
        battle.rarity,
        monster.race,
        hero.dungeonLevel.toLong,
        Rng(seed)
      )
      // routing события (returnState/eventData) кладётся в scene_data ДО боя
      // (напр. цепочка «мобы с сокровищем»); переносим его в добычу, чтобы экран
      // добычи знал, куда вернуться. Для обычного боя scene_data пуст → None.
      prev <- heroDao
        .readSceneData(user.userId)
        .map(_.flatMap(_.as[LootState.LootData].toOption))
      lootData = LootState.LootData(
        items = drops.collect {
          case LootGenerator.LootDrop.Gear(i)    => i
          case LootGenerator.LootDrop.Trophy(i)  => i
          case LootGenerator.LootDrop.MapHalf(i) => i
        },
        golds = drops.collect { case LootGenerator.LootDrop.Gold(a, _) => a },
        returnState = prev.flatMap(_.returnState),
        eventData = prev.flatMap(_.eventData)
      )
      _ <- heroDao.clearActiveBattle(user.userId)
      _ <- heroDao.updateExpAndLevel(
        user.userId,
        leveled.exp,
        leveled.lvl,
        leveled.upgradePoints
      )
      _ <- heroDao.writeSceneData(user.userId, lootData.asJson)
      // победа над «Отмеченным тьмой» на текущем этаже открывает путь вглубь
      newMaxDungeon = math.min(150, hero.dungeonLevel + 1)
      unlocksDarkness =
        battle.monsterMarked && newMaxDungeon > hero.maxDungeonLevel
      _ <- ZIO.when(unlocksDarkness)(
        heroDao.updateMaxDungeonLevel(user.userId, newMaxDungeon)
      )
      _ <- renderer.show(
        user,
        Screen(
          content.format(
            "battle.victory",
            "monster" -> monster.name,
            "exp"     -> expGained.toString
          ),
          Nil
        )
      )
      _ <- ZIO.when(unlocksDarkness)(
        renderer.show(
          user,
          Screen(content.text("battle.darknessConquered"), Nil)
        )
      )
      _ <- ZIO.when(leveled.lvl > hero.lvl)(
        renderer.show(
          user,
          Screen(s"Вы получили новый уровень ${leveled.lvl}!", Nil)
        )
      )
    } yield StateType.Loot

  private def heroDeath(user: User, renderer: Renderer): Task[StateType] =
    renderer
      .show(user, Screen(content.text("battle.death"), Nil))
      .as(StateType.Death)

  private def flee(user: User, renderer: Renderer): Task[StateType] =
    renderer
      .show(user, content.screen("battle.fleeConfirm"))
      .as(StateType.Battle)

  private def confirmFlee(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      eff       = hero.effectiveFightStats(now)
      buffedEff = battle.heroBattleState.applyTo(eff)
      monster   = battle.toMonster
      hitRoll <- Random.nextIntBetween(1, 101)

      result <-
        if (
          hitRoll > playerDodgeChance(
            hero.effectiveBaseStats(now).agi,
            buffedEff.evasion,
            buffedEff.defence,
            battle
          )
        ) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monster.fightStats.atk * spread / 100L).max(1L)
            reduction = BattleState.damageReduction(
              protection = buffedEff.defence,
              defenderInt = hero.effectiveBaseStats(now).int,
              attackerInt = monster.fightStats.concentration
            )
            reducedDamage = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            buffReduct = math.min(
              battle.heroBattleState.armorBonus,
              reducedDamage
            )
            afterBuff   = reducedDamage - buffReduct
            curArmor    = hero.fightStats.armor.max(0L)
            armorAbsorb = math.min(curArmor, afterBuff)
            hpDmg       = afterBuff - armorAbsorb
            newArmor    = curArmor - armorAbsorb
            newHp       = (hero.fightStats.hp - hpDmg).max(0L)
            _ <- heroDao.updateFightStats(
              user.userId,
              hero.fightStats.copy(hp = newHp, armor = newArmor)
            )
            _ <- renderer.show(
              user,
              Screen(
                content.format(
                  "battle.mobHit",
                  "damage"  -> reducedDamage.toString,
                  "monster" -> monster.name
                ),
                Nil
              )
            )
            r <-
              if (newHp <= 0) heroDeath(user, renderer)
              else
                heroDao.clearActiveBattle(user.userId) *>
                  renderer
                    .show(user, Screen(content.text("battle.fled"), Nil))
                    .as(StateType.Dungeon)
          } yield r
        } else {
          heroDao.clearActiveBattle(user.userId) *>
            renderer
              .show(user, Screen(content.text("battle.fled"), Nil))
              .as(StateType.Dungeon)
        }
    } yield result

  private def useFlask(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      flask = hero.equipment.flask
      result <- flask.details match {
        case f: ItemDetails.Flask if f.charges <= 0 =>
          renderer
            .show(user, Screen(content.text("battle.flaskEmpty"), Nil))
            .as(StateType.Battle)
        case _: ItemDetails.Flask if battle.consumableUsedThisRound =>
          renderer
            .show(user, Screen(content.text("battle.consumableAlreadyUsed"), Nil))
            .as(StateType.Battle)
        case f: ItemDetails.Flask =>
          val newEquipment  = hero.equipment.copy(flask = flask.copy(details = f.spent))
          val updatedBattle = battle.copy(consumableUsedThisRound = true)
          f.effect match {
            case FlaskEffect.HealPercent(pct) =>
              val maxHp    = hero.effectiveMaxHp(now)
              val healAmt  = (maxHp * pct / 100L).max(1L)
              val newHp    = (hero.fightStats.hp + healAmt).min(maxHp)
              val healed   = newHp - hero.fightStats.hp
              val newStats = hero.fightStats.copy(hp = newHp)
              for {
                _ <- heroDao.updateEquipmentAndFightStats(
                  user.userId,
                  newEquipment,
                  newStats
                )
                _ <- heroDao.writeActiveBattle(
                  user.userId,
                  updatedBattle.asJson
                )
                _ <- renderer.show(
                  user,
                  Screen(
                    content.format(
                      "battle.flaskUsed",
                      "healed" -> healed.toString,
                      "hp"     -> newHp.toString,
                      "max"    -> maxHp.toString
                    ),
                    Nil
                  )
                )
                _ <- showScreen(user, renderer)
              } yield StateType.Battle
            case FlaskEffect.AddBuff(buff, rounds) =>
              val timedBuff = buff.copy(turnsLeft = Some(rounds))
              val newBattle = updatedBattle.copy(heroBattleState =
                updatedBattle.heroBattleState.add(timedBuff)
              )
              for {
                _ <- heroDao.updateEquipment(user.userId, newEquipment)
                _ <- heroDao.writeActiveBattle(user.userId, newBattle.asJson)
                _ <- renderer.show(
                  user,
                  Screen(content.text("battle.flaskBuff"), Nil)
                )
                _ <- showScreen(user, renderer)
              } yield StateType.Battle
          }
        case _ =>
          renderer
            .show(user, Screen(content.text("battle.noFlask"), Nil))
            .as(StateType.Battle)
      }
    } yield result

  /** Применение зелья пояса. Зеркалит [[useFlask]]: проверки пусто/уже-пили, трата
    * заряда, затем эффект по типу зелья ([[applyPotion]]). */
  private def useBelt(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      belt = hero.equipment.belt
      result <- belt.details match {
        case b: ItemDetails.Belt if b.charges <= 0 =>
          renderer
            .show(user, Screen(content.text("battle.beltEmpty"), Nil))
            .as(StateType.Battle)
        case _: ItemDetails.Belt if battle.consumableUsedThisRound =>
          renderer
            .show(user, Screen(content.text("battle.consumableAlreadyUsed"), Nil))
            .as(StateType.Battle)
        case b: ItemDetails.Belt =>
          val equipment = hero.equipment.copy(belt = belt.copy(details = b.spent))
          val battle1   = battle.copy(consumableUsedThisRound = true)
          applyPotion(user, hero, battle1, equipment, b.potion, now, renderer)
        case _ =>
          renderer
            .show(user, Screen(content.text("battle.noBelt"), Nil))
            .as(StateType.Battle)
      }
    } yield result

  /** Эффект конкретного зелья пояса. Мгновенные (лечение/металл) меняют статы
    * героя; временные (атака/защита/уворот/концентрация) кладут баф на 5 ходов;
    * яд/реген ставят соответствующий статус-эффект. `equipment` уже с потраченным
    * зарядом, `battle` — с выставленным `consumableUsedThisRound`. */
  private def applyPotion(
      user: User,
      hero: Hero,
      battle: SoloPveBattle,
      equipment: Equipment,
      potion: PotionKind,
      nowMs: Long,
      renderer: Renderer
  ): Task[StateType] = {
    val buffed = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    potion match {
      case PotionKind.Healing =>
        val maxHp  = hero.effectiveMaxHp(nowMs)
        val heal   = (maxHp * 25 / 100L).max(1L)
        val newHp  = (hero.fightStats.hp + heal).min(maxHp)
        val healed = newHp - hero.fightStats.hp
        finishBelt(user, equipment, Some(hero.fightStats.copy(hp = newHp)), battle, renderer,
          content.format("battle.beltHeal", "healed" -> healed.toString, "hp" -> newHp.toString, "max" -> maxHp.toString))

      case PotionKind.Metal =>
        val maxArmor = hero.effectiveMaxArmor(nowMs)
        val restore  = (maxArmor * 20 / 100L).max(1L)
        val newArmor = (hero.fightStats.armor + restore).min(maxArmor)
        val gained   = newArmor - hero.fightStats.armor
        finishBelt(user, equipment, Some(hero.fightStats.copy(armor = newArmor)), battle, renderer,
          content.format("battle.beltMetal", "armor" -> gained.toString, "cur" -> newArmor.toString, "max" -> maxArmor.toString))

      case PotionKind.Poison =>
        val newBattle = battle.copy(effects = battle.effects.copy(heroPoisonousAttacks = true))
        finishBelt(user, equipment, None, newBattle, renderer, content.text("battle.beltPoison"))

      case PotionKind.Regeneration =>
        val newBattle = battle.copy(effects = battle.effects.copy(heroRegen = Some(Regen.onDrink)))
        finishBelt(user, equipment, None, newBattle, renderer,
          content.format("battle.beltRegen", "pct" -> Regen.OnDrink.toString))

      case PotionKind.Evasion =>
        addBeltBuff(user, equipment, battle, renderer,
          Buff(0L, 0L, 0L, dodgePct = 10L, skillHitPct = 0L, Some(ItemDetails.Belt.BuffRounds)),
          content.format("battle.beltEvasion", "rounds" -> ItemDetails.Belt.BuffRounds.toString))

      case PotionKind.Concentration =>
        addBeltBuff(user, equipment, battle, renderer,
          Buff(0L, 0L, 0L, dodgePct = 0L, skillHitPct = 10L, Some(ItemDetails.Belt.BuffRounds)),
          content.format("battle.beltConcentration", "rounds" -> ItemDetails.Belt.BuffRounds.toString))

      case PotionKind.Attack =>
        val bonus = (buffed.atk * 5 / 100L).max(1L)
        addBeltBuff(user, equipment, battle, renderer,
          Buff(atk = bonus, 0L, 0L, 0L, 0L, Some(ItemDetails.Belt.BuffRounds)),
          content.format("battle.beltAttack", "atk" -> bonus.toString, "rounds" -> ItemDetails.Belt.BuffRounds.toString))

      case PotionKind.Defence =>
        val bonus = (buffed.defence * 10 / 100L).max(1L)
        addBeltBuff(user, equipment, battle, renderer,
          Buff(0L, 0L, defence = bonus, 0L, 0L, Some(ItemDetails.Belt.BuffRounds)),
          content.format("battle.beltDefence", "defence" -> bonus.toString, "rounds" -> ItemDetails.Belt.BuffRounds.toString))
    }
  }

  /** Кладёт временный баф на героя и сохраняет бой (без изменения статов). */
  private def addBeltBuff(
      user: User,
      equipment: Equipment,
      battle: SoloPveBattle,
      renderer: Renderer,
      buff: Buff,
      msg: String
  ): Task[StateType] =
    finishBelt(user, equipment, None,
      battle.copy(heroBattleState = battle.heroBattleState.add(buff)), renderer, msg)

  /** Общий хвост применения зелья: персист (снаряжение + опц. статы), запись боя,
    * сообщение об эффекте и перерисовка экрана боя. */
  private def finishBelt(
      user: User,
      equipment: Equipment,
      newStats: Option[FightStats],
      battle: SoloPveBattle,
      renderer: Renderer,
      msg: String
  ): Task[StateType] = {
    val persist = newStats match {
      case Some(stats) => heroDao.updateEquipmentAndFightStats(user.userId, equipment, stats)
      case None        => heroDao.updateEquipment(user.userId, equipment)
    }
    for {
      _ <- persist
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- renderer.show(user, Screen(msg, Nil))
      _ <- showScreen(user, renderer)
    } yield StateType.Battle
  }

  private def showScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      _ <- renderer.show(
        user,
        buildBattleScreen(hero, battle, hero.effectiveMaxHp(now), now)
      )
    } yield ()

  private def buildBattleScreen(
      hero: Hero,
      battle: SoloPveBattle,
      maxHp: Long,
      nowMs: Long
  ): Screen = {
    val eff       = hero.effectiveFightStats(nowMs)
    val buffedEff = battle.heroBattleState.applyTo(eff)
    val playerDodgePct = playerDodgeChance(
      hero.effectiveBaseStats(nowMs).agi,
      buffedEff.evasion,
      buffedEff.defence,
      battle
    ).toInt
    val monsterDodgePct = mobDodgeChance(buffedEff.accuracy, battle).toInt
    val mobHitPct       = 100 - playerDodgePct
    val heroHitPct      = 100 - monsterDodgePct
    val reductionPct = (BattleState.damageReduction(
      protection = buffedEff.defence,
      defenderInt = hero.effectiveBaseStats(nowMs).int,
      attackerInt = battle.monsterStats.concentration
    ) * 100.0).toInt
    val heroSkillHitPct = heroSkillHitChance(hero, battle, nowMs).toInt
    val mobSkillHitPct = BattleState
      .monsterSkillHitChance(
        monsterConc = battle.monsterStats.concentration,
        rarityFactor = battle.rarity.factor,
        heroConc = buffedEff.concentration,
        heroInt = hero.effectiveBaseStats(nowMs).int
      )
      .toInt
    // Статус-эффекты рядом со статами своей сущности: яд у HP моба (-урон/ход),
    // реген у HP героя (+лечение/ход).
    val monsterPoison = battle.effects.monsterPoison
      .map(p => s" (-${p.damageOn(battle.monsterStats.hp)})").getOrElse("")
    val heroRegen = battle.effects.heroRegen
      .map(r => s" (+${r.healOn(maxHp)})").getOrElse("")
    val text = content.format(
      "battle.enter.text",
      "monster"         -> battle.toMonster.name,
      "monsterRace"     -> battle.toMonster.race.toString,
      "monsterHp"       -> battle.monsterCurrentHp.toString,
      "monsterMax"      -> battle.monsterStats.hp.toString,
      "monsterPoison"   -> monsterPoison,
      "heroRegen"       -> heroRegen,
      "monsterArmor"    -> battle.monsterCurrentArmor.toString,
      "monsterMaxArmor" -> battle.monsterStats.armor.toString,
      "monsterAtk"      -> battle.monsterStats.atk.toString,
      "mobHit"          -> s"$mobHitPct%",
      "monsterDodge"    -> s"$monsterDodgePct%",
      "heroHit"         -> s"$heroHitPct%",
      "heroDodge"       -> s"$playerDodgePct%",
      "heroHp"          -> hero.fightStats.hp.toString,
      "heroMax"         -> maxHp.toString,
      "heroArmor" -> hero.fightStats.armor
        .min(hero.effectiveMaxArmor(nowMs))
        .toString,
      "heroMaxArmor"  -> hero.effectiveMaxArmor(nowMs).toString,
      "heroAtk"       -> buffedEff.atk.toString,
      "heroReduction" -> s"$reductionPct%",
      "heroSkillHit"  -> s"$heroSkillHitPct%",
      "mobSkillHit"   -> s"$mobSkillHitPct%",
      "flaskCharges"  -> BattleState.flaskCharges(hero).toString
    )
    val skillButtons = battle.skillSlots.collect {
      case slot if slot.cooldown <= 0 =>
        pangea.engine.Choice(
          id = s"Skill_${slot.itemId}",
          label = slot.skill.label,
          color = pangea.engine.ChoiceColor.Negative,
          row = Some(0)
        )
    }
    // Ряды кнопок: row 0 — активные навыки; row 1 — Атака; row 2 — Фляга и Пояс
    // (Пояс — только если несёт зелье); row 3 — Сбежать. Места под заглушки (доп.
    // слот, стиль атаки, сменить цель) пока скрыты.
    val flaskCharges = BattleState.flaskCharges(hero)
    val flaskButton = pangea.engine.Choice(
      "UseFlask",
      s"🧪 Фляга ($flaskCharges)",
      color =
        if (flaskCharges <= 0) pangea.engine.ChoiceColor.Negative
        else pangea.engine.ChoiceColor.Primary,
      row = Some(2)
    )
    val beltButton = BattleState.beltPotion(hero).map { belt =>
      pangea.engine.Choice(
        "UseBelt",
        s"👝 Пояс (${belt.charges})",
        color =
          if (belt.charges <= 0) pangea.engine.ChoiceColor.Negative
          else pangea.engine.ChoiceColor.Primary,
        row = Some(2)
      )
    }
    val mainButtons =
      List(pangea.engine.Choice("Attack", "Атака", row = Some(1)), flaskButton) ++
        beltButton.toList ++
        List(
          pangea.engine.Choice(
            "Flee",
            "Сбежать",
            color = pangea.engine.ChoiceColor.Negative,
            row = Some(3)
          )
        )
    Screen(text, skillButtons ++ mainButtons)
  }

  private def getHero(user: User): Task[Hero] =
    heroDao
      .getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getBattle(user: User): Task[SoloPveBattle] =
    heroDao
      .readActiveBattle(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No active battle for user ${user.userId}"))
      .flatMap(json => ZIO.fromEither(json.as[SoloPveBattle]))

  // Уклонение игрока от удара моба: защита игрока в знаменателе, точность моба ×1.5.
  // Поверх базового шанса добавляется бонус от боевых бафов (зелье уворота); потолок
  // 95% абсолютный — баф его не пробивает.
  private def playerDodgeChance(
      agi: Long,
      evasion: Long,
      defence: Long,
      battle: SoloPveBattle
  ): Double =
    (BattleState.dodgeChance(agi, evasion, defence, battle.monsterStats.accuracy)
      + battle.heroBattleState.dodgeBonus).min(95.0).max(5.0)

  // Шанс героя применить активный навык с учётом боевых бафов (зелье концентрации):
  // базовая формула плюс `skillHitBonus`; потолок 95% абсолютный — баф его не пробивает.
  private def heroSkillHitChance(hero: Hero, battle: SoloPveBattle, nowMs: Long): Double = {
    val buffed = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    val base = BattleState.heroSkillHitChance(
      heroConc = buffed.concentration,
      heroInt = hero.effectiveBaseStats(nowMs).int,
      monsterConc = battle.monsterStats.concentration,
      rarityFactor = battle.rarity.factor
    )
    (base + battle.heroBattleState.skillHitBonus).min(95.0).max(5.0)
  }

  // Уклонение моба от удара игрока — та же формула: у моба нет ловкости (agi = 0),
  // в знаменателе его защита и точность игрока ×1.5.
  private def mobDodgeChance(heroAccuracy: Long, battle: SoloPveBattle): Double =
    BattleState.dodgeChance(
      0L,
      battle.monsterStats.evasion,
      battle.monsterStats.defence,
      heroAccuracy
    )
}

object BattleState {

  /** Текущее число зарядов надетой фляги (0, если фляга не надета). */
  def flaskCharges(hero: Hero): Int = hero.equipment.flask.details match {
    case f: ItemDetails.Flask => f.charges
    case _                    => 0
  }

  /** Зелье надетого пояса (если пояс несёт зелье) — для кнопки «Пояс» и её зарядов. */
  def beltPotion(hero: Hero): Option[ItemDetails.Belt] = hero.equipment.belt.details match {
    case b: ItemDetails.Belt => Some(b)
    case _                   => None
  }

  /** Шанс игрока попасть способностью по мобу, в процентах [5; 95]: 100 ·
    * (heroConc · 0.75·heroInt) / (0.5·monsterConc · monster.rarity.factor).
    */
  def heroSkillHitChance(
      heroConc: Long,
      heroInt: Long,
      monsterConc: Long,
      rarityFactor: Double
  ): Double = {
    val numer = heroConc.toDouble * (0.75 * heroInt.toDouble)
    val denom = (0.5 * monsterConc.toDouble) * rarityFactor.max(0.0001)
    val raw   = if (denom <= 0.0) 100.0 else 100.0 * numer / denom
    raw.max(5.0).min(95.0)
  }

  /** Шанс моба попасть способностью по игроку, в процентах [5; 95]: 100 ·
    * (monsterConc · rarity.factor) / (0.25·heroConc · 0.75·heroInt).
    */
  def monsterSkillHitChance(
      monsterConc: Long,
      rarityFactor: Double,
      heroConc: Long,
      heroInt: Long
  ): Double = {
    val numer = monsterConc.toDouble * rarityFactor
    val denom = (0.25 * heroConc.toDouble) * (0.75 * heroInt.toDouble)
    val raw   = if (denom <= 0.0) 100.0 else 100.0 * numer / denom
    raw.max(5.0).min(95.0)
  }

  /** Из payload UserAction достаём itemId, если action имеет вид `Skill_<id>`.
    */
  def parseSkillAction(ua: pangea.service.state.UserAction): Option[Long] = {
    val key = ua.payload
      .flatMap(p =>
        io.circe.jawn
          .decode[Map[String, String]](p)
          .toOption
          .flatMap(_.get("action"))
      )
      .getOrElse("")
    key match {
      case s"Skill_$rest" => rest.toLongOption
      case _              => None
    }
  }

  /** Шанс уклонения защищающегося юнита от удара атакующего, в процентах, зажат
    * в [5, 95]: 100 * (agi + evasion) / (agi + evasion + defence * 1 +
    * attackerAccuracy * 1.5) Единая логика «попадания по юниту» для обеих
    * сторон: положительные параметры (числитель) — ловкость и уклонение
    * защищающегося; отрицательные (знаменатель) — защита защищающегося (×1) и
    * точность атакующего (×1.5). Атакующий попадает, если бросок 1..100 больше
    * уклонения.
    */
  /** Процентное снижение урона, наносимого защищающемуся юниту: reduction = (P
    * + Iₚ) / (P + Iₚ + Iₑ × 2), зажато сверху 0.7. Где `P` — защита защитника,
    * `Iₚ` — его интеллект, `Iₑ` — интеллект атакующего. Применяется к чистому
    * урону до брони. Соотношение «×2 для интеллекта атакующего» — из ТЗ.
    * Возврат — доля [0; 0.7].
    */
  def damageReduction(
      protection: Long,
      defenderInt: Long,
      attackerInt: Long
  ): Double = {
    val numer = (protection + defenderInt).toDouble
    val denom = numer + attackerInt * 2.0
    val raw   = if (denom <= 0.0) 0.0 else numer / denom
    raw.max(0.0).min(0.7)
  }

  def dodgeChance(
      agi: Long,
      evasion: Long,
      defence: Long,
      attackerAccuracy: Long
  ): Double = {
    val positive = (agi + evasion).toDouble
    val denom    = positive + defence * 1.0 + attackerAccuracy * 1.5
    val raw      = if (denom <= 0.0) 0.0 else 100.0 * positive / denom
    raw.max(5.0).min(95.0)
  }
}
