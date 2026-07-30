package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.LootGenerator
import pangea.model.battle.{Bleed, Buff, Burn, Element, Poison, Regen, SoloPveBattle, SkillSlotState, TimedDefenceDebuff}
import pangea.model.hero.{AzatState, CubeStatus, Hero}
import pangea.model.item.{FlaskEffect, ItemDetails, PassiveKind, PotionKind}
import pangea.model.stats.FightStats
import pangea.model.skill.{MonsterSkill, Skill}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

/** Бой строится как «прочитать всё в начале хода → чисто посчитать ход →
  * записать всё один раз в конце». Боевые функции ([[attackTurn]]/[[skillTurn]]/
  * [[flaskTurn]]/[[beltTurn]]/[[fleeTurn]] и внутренние [[playerStrike]]/
  * [[monsterPhase]]/[[dealSkillDamage]]) НЕ трогают `heroDao`/`renderer` — они
  * только считают и накапливают лог сообщений ([[BattleState.TurnResult]]).
  * Единственная точка I/O — [[resolve]]/[[commit]]: она персистит итог хода
  * ровно один раз и показывает склеенный лог. Так исключён целый класс багов
  * «одна из веток забыла сохранить стат» (см. Кровавую жатву). */
case class BattleState(heroDao: HeroDao, content: SceneContent) extends State {

  import BattleState.{Outcome, TurnResult, VictoryOutcome}

  /** Чистое вычисление одного хода: снимок состояния → результат хода. */
  private type Turn = (Hero, SoloPveBattle, Long) => Task[TurnResult]

  private val branch = new Branch(
    routes = Map(
      "Attack"      -> Target.Run((u, _, r) => resolve(u, r)(attackTurn)),
      "UseFlask"    -> Target.Run((u, _, r) => resolve(u, r)(flaskTurn)),
      "UseBelt"     -> Target.Run((u, _, r) => resolve(u, r)(beltTurn)),
      "Flee"        -> Target.Run((u, _, r) => flee(u, r)),
      "ConfirmFlee" -> Target.Run((u, _, r) => resolve(u, r)(fleeTurn)),
      "CancelFlee"  -> Target.Run((u, _, r) => showScreen(u, r).as(StateType.Battle))
    ),
    fallback = Target.Run { (user, ua, renderer) =>
      // Кнопки способностей именуются Skill_<itemId> и роутятся динамически: id
      // указывает на конкретный предмет, поэтому два предмета с «одним» Skill
      // имеют независимые cd/uses.
      BattleState.parseSkillAction(ua) match {
        case Some(itemId) => resolve(user, renderer)(skillTurn(itemId))
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

  // ── Оболочка I/O: читаем всё → считаем ход → пишем всё ──────────────────────

  /** Прочитать снимок (герой + бой + время) один раз, посчитать ход чистой
    * функцией `turn`, затем один раз закоммитить результат. */
  private def resolve(user: User, renderer: Renderer)(turn: Turn): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      result <- turn(hero, battle, now)
      state  <- commit(user, result, now, renderer)
    } yield state

  /** Единственная точка записи и показа за ход. Персист итоговых
    * снаряжения+статов героя выполняется ВСЕГДА и вне ветвления по исходу —
    * поэтому ни одна ветка не может «забыть» сохранить изменённый стат.
    *
    * ЖЁСТКИЙ ИНВАРИАНТ: сначала ВСЕ записи хода (неразрывным блоком), только
    * потом показы. Отправка сообщений — сетевой вызов к ВК: он может упасть,
    * а фибру обработки может прервать разрыв HTTP-соединения (ВК не дождался
    * ответа). Если бы запись шла после показа, ход «отображался, но не
    * случался»: игрок видел урон, а в БД оставалось прежнее состояние боя —
    * так терялось добивание навыком (моб оживал с прежним HP). */
  private def commit(
      user: User,
      res: TurnResult,
      nowMs: Long,
      renderer: Renderer
  ): Task[StateType] = {
    val persistHero =
      heroDao.updateEquipmentAndFightStats(user.userId, res.hero.equipment, res.hero.fightStats)
    val msg     = res.log.mkString("\n")
    val showLog = ZIO.when(msg.nonEmpty)(renderer.show(user, Screen(msg, Nil)))
    res.outcome match {
      case Outcome.Continue =>
        (persistHero *> heroDao.writeActiveBattle(user.userId, res.battle.asJson)).uninterruptible *>
          showLog *>
          renderer
            .show(user, buildBattleScreen(res.hero, res.battle, res.hero.effectiveMaxHp(nowMs), nowMs))
            .as(StateType.Battle)
      case Outcome.Victory =>
        for {
          outcome <- (persistHero *> applyVictory(user, res.hero, res.battle)).uninterruptible
          _       <- showLog
          _       <- showVictory(user, outcome, renderer)
        } yield StateType.Loot
      case Outcome.Death =>
        persistHero.uninterruptible *> showLog *>
          renderer.show(user, Screen(content.text("battle.death"), Nil)).as(StateType.Death)
      case Outcome.Fled =>
        (persistHero *> heroDao.clearActiveBattle(user.userId)).uninterruptible *>
          showLog *>
          renderer.show(user, Screen(content.text("battle.fled"), Nil)).as(StateType.Dungeon)
    }
  }

  /** Continue без изменения состояния — только сообщение (неготовый скилл, пустая
    * фляга и т.п.): ход не тратится, экран перерисовывается. */
  private def cont(hero: Hero, battle: SoloPveBattle, msg: String): Task[TurnResult] =
    ZIO.succeed(TurnResult(hero, battle, Vector(msg), Outcome.Continue))

  // ── Ход игрока: базовая атака ───────────────────────────────────────────────

  private def attackTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    playerStrike(hero, battle, nowMs, Vector.empty, Set.empty)

  /** Базовая атака игрока. `log` — уже накопленные строки хода (например, строка
    * применённого скилла, после которого идёт базовая атака). `skipSlots`
    * пробрасывается в `monsterPhase → tickBuffs`, чтобы только что использованный
    * слот не тикался в этом же ходу (см. SoloPveBattle.tickBuffs).
    */
  private def playerStrike(
      hero0: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long]
  ): Task[TurnResult] = {
    // Реген пассивок «Целебный»/«Самовосстанавливающийся» учитывается перед атакой.
    val hero = hero0.withCombatRegen(nowMs)
    for {
      buffedEff <- ZIO.succeed(effWithAir(hero, battle, nowMs))
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
            // «Разбойника» (+5%) множит итоговый урон; грейды стихийных камней в
            // оружии добавляют +2%/грейд стихийного урона (elementalDamageMult).
            damage =
              (((hero.effectiveBaseStats(nowMs).str * 3L + buffedEff.atk) * spread / 100L) * weaponMod * hero.passives.finalDamageMult * hero.gems.elementalDamageMult).toLong
                .max(1L)
            attackLine = content.format(
              "battle.hit",
              "damage"  -> damage.toString,
              "monster" -> monster.name
            )
            // Стихии оружия модифицируют раздельно урон по броне и по HP
            // (см. splitElementalDamage). Без стихий поведение прежнее.
            (armorDmg, hpDmg) = splitElementalDamage(battle.monsterCurrentArmor, damage, hero.gems)
            newArmor = battle.monsterCurrentArmor - armorDmg
            newHp    = (battle.monsterCurrentHp - hpDmg).max(0L)
            // Баф «ядовитые атаки»: на любом попадании снимается. Моб травится, только
            // если удар прошёл в HP (hpDmg > 0) и моб жив: повторное попадание стакает
            // текущий яд (+OnHit), иначе накладывает свежий.
            poisonsNow = battle.effects.heroPoisonousAttacks && hpDmg > 0 && newHp > 0
            effectsAfterPotion =
              if (!battle.effects.heroPoisonousAttacks) battle.effects
              else battle.effects.copy(
                heroPoisonousAttacks = false,
                monsterPoison =
                  if (poisonsNow) Some(battle.effects.monsterPoison.map(_.stacked).getOrElse(Poison.onHit))
                  else battle.effects.monsterPoison
              )
            // Изумруд в оружии: при уроне по HP пассивно накладывает/стакает яд.
            gemPoisonPct = hero.gems.weaponPoisonPct
            gemPoisons   = gemPoisonPct > 0 && hpDmg > 0 && newHp > 0
            effects =
              if (!gemPoisons) effectsAfterPotion
              else effectsAfterPotion.copy(
                monsterPoison = Some(effectsAfterPotion.monsterPoison
                  .map(p => Poison(p.pct + gemPoisonPct))
                  .getOrElse(Poison(gemPoisonPct))))
            // Череп в оружии: вампиризм — % нанесённого по HP урона в лечение героя.
            maxHp      = hero.effectiveMaxHp(nowMs)
            vamp       = if (hero.gems.vampirismPct > 0 && hpDmg > 0) (hpDmg * hero.gems.vampirismPct / 100L).max(0L) else 0L
            healedHero = if (vamp > 0) hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp + vamp).min(maxHp))) else hero
            vampGained = healedHero.fightStats.hp - hero.fightStats.hp
            hitBattle = battle.copy(
              monsterCurrentHp = newHp,
              monsterCurrentArmor = newArmor,
              effects = effects
            )
            // Проки стихий оружия (30% каждый) — только если моб жив после удара.
            elemResult <- if (newHp > 0) resolveElementProcs(hero, hitBattle)
                          else ZIO.succeed((hitBattle, Vector.empty[String]))
            (updated, elemLog) = elemResult
            log1 = log :+ (attackLine + dotIndicators(updated))
            log2 =
              if (poisonsNow || gemPoisons) log1 :+ content.format("battle.poisonApplied", "monster" -> monster.name)
              else log1
            log3 =
              if (vampGained > 0) log2 :+ content.format("battle.vampirism", "healed" -> vampGained.toString)
              else log2
            log4 = log3 ++ elemLog
            r <-
              if (updated.monsterCurrentHp <= 0) ZIO.succeed(TurnResult(healedHero, updated, log4, Outcome.Victory))
              else monsterPhase(healedHero, updated, nowMs, log4, skip)
          } yield r
        } else {
          val attackLine =
            content.format("battle.miss", "chance" -> heroHitPct.toString)
          monsterPhase(hero, battle, nowMs, log :+ attackLine, skip)
        }
    } yield result
  }

  // ── Стихии оружия ───────────────────────────────────────────────────────────

  /** Боевые статы героя с учётом бафов боя и временного буста Воздуха (прок
    * Воздуха: +10% уклонения/точности на `airBoostTurns` ходов). Постоянный +5%
    * от воздуха в оружии уже учтён в `hero.effectiveFightStats`. */
  private def effWithAir(hero: Hero, battle: SoloPveBattle, nowMs: Long): FightStats = {
    val fs = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    if (battle.effects.airBoostTurns <= 0) fs
    else {
      val p = Element.Air.ProcBonusPct
      fs.copy(
        evasion  = fs.evasion + fs.evasion * p / 100L,
        accuracy = fs.accuracy + fs.accuracy * p / 100L
      )
    }
  }

  /** Разбивает `damage` на урон по броне и по HP с учётом стихийных модификаторов
    * оружия. Без стихий (мультипликаторы = 1, доля молнии = 0) поведение прежнее:
    * броня поглощает `min(armor, damage)`, остальное — в HP. */
  private def splitElementalDamage(curArmor: Long, damage: Long, g: pangea.model.hero.HeroGems): (Long, Long) = {
    val rawArmorPart = math.min(curArmor, damage)
    val rawHpPart    = damage - rawArmorPart
    val armorDmg     = math.min(curArmor, (rawArmorPart * g.armorDamageMult).toLong).max(0L)
    val hpFromHp     = (rawHpPart * g.hpDamageMult).toLong
    // Молния: часть урона по броне дополнительно бьёт по HP.
    val hpFromArmor  = (armorDmg * g.lightningArmorToHpFrac).toLong
    (armorDmg, (hpFromHp + hpFromArmor).max(0L))
  }

  /** Роллит проки стихий оружия (по 30% на каждую стихию в оружии) и применяет их:
    * сперва комбо (Огонь+Воздух, Молния+Холод), затем оставшиеся одиночные эффекты.
    * Возвращает обновлённый бой и строки лога. Урон комбо снимается сразу (может
    * добить моба — исход перепроверяется вызывающим по `monsterCurrentHp`). */
  private def resolveElementProcs(hero: Hero, battle: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] = {
    val elems = hero.gems.weaponElements
    if (elems.isEmpty) ZIO.succeed((battle, Vector.empty))
    else
      // Роллим в фиксированном порядке Element.values для детерминизма тестов.
      ZIO.foreach(Element.values.filter(elems.contains).toList) { e =>
        Random.nextIntBetween(1, 101).map(roll => e -> (roll <= Element.ProcChancePct))
      }.map { rolls =>
        val fired      = rolls.collect { case (e, true) => e }.toSet
        val maxHp      = battle.monsterStats.hp
        val maxArmor   = MonsterSkill.monsterMaxArmor(battle)
        val fireAir    = fired(Element.Fire) && fired(Element.Air)
        val lightCold  = fired(Element.Lightning) && fired(Element.Cold)

        var b   = battle
        var log = Vector.empty[String]

        // Комбо Огонь+Воздух: мгновенный урон 30% макс.HP + текущий % горения, горение → 2%.
        if (fireAir) {
          val burnPct = b.effects.monsterBurn.map(_.pct).getOrElse(0)
          val dmg     = (maxHp.toDouble * (30 + burnPct) / 100.0).toLong.max(0L)
          b = b.copy(
            monsterCurrentHp = (b.monsterCurrentHp - dmg).max(0L),
            effects = b.effects.copy(monsterBurn = Some(Burn(Burn.Initial)))
          )
          log = log :+ content.format("battle.comboFireAir", "damage" -> dmg.toString)
        }

        // Комбо Молния+Холод: -40% макс.брони (не переходит в HP), -20% защиты (3 хода), выжиг энергии (флейвор).
        if (lightCold) {
          val armorCut = (maxArmor.toDouble * 0.40).toLong.max(0L)
          b = b.copy(
            monsterCurrentArmor = (b.monsterCurrentArmor - armorCut).max(0L),
            effects = b.effects.copy(monsterDefenceDebuff = Some(TimedDefenceDebuff(20, 3)))
          )
          log = log :+ content.text("battle.comboLightningCold")
        }

        // Одиночные проки для стихий, не поглощённых комбо.
        if (fired(Element.Cold) && !lightCold) {
          b = b.copy(effects = b.effects.copy(
            monsterColdDefenceCut = b.effects.monsterColdDefenceCut + Element.Cold.DefenceReductionCut))
          log = log :+ Element.Cold.procText
        }
        if (fired(Element.Fire) && !fireAir) {
          val burn = b.effects.monsterBurn.map(_.reignited).getOrElse(Burn.onIgnite)
          b = b.copy(effects = b.effects.copy(monsterBurn = Some(burn)))
          log = log :+ Element.Fire.procText
        }
        if (fired(Element.Air) && !fireAir) {
          b = b.copy(effects = b.effects.copy(airBoostTurns = Element.Air.ProcTurns))
          log = log :+ Element.Air.procText
        }
        if (fired(Element.Lightning) && !lightCold) {
          // Выжиг энергии моба — флейвор (у мобов нет рабочего пула энергии).
          log = log :+ Element.Lightning.procText
        }
        (b, log)
      }
  }

  /** Итоговая защита моба с учётом временного %-дебафа (комбо Молния+Холод). */
  private def effectiveMonsterDefence(battle: SoloPveBattle): Long =
    battle.effects.monsterDefenceDebuff match {
      case Some(d) => (battle.monsterStats.defence * (100 - d.pct) / 100).max(0L)
      case None    => battle.monsterStats.defence
    }

  /** Компактная сводка активных DoT на мобе для приписки к строкам атаки:
    * ` (🟢 -N ❤)(🔴 -N ❤)(🔥 -N ❤)`. Пусто, если эффектов нет. */
  private def dotIndicators(battle: SoloPveBattle): String = {
    val maxHp = battle.monsterStats.hp
    val parts = List(
      battle.effects.monsterPoison.map(p => s"🟢 -${p.damageOn(maxHp)} ❤"),
      battle.effects.monsterBleed.map(b => s"🔴 -${b.damageOn(maxHp)} ❤"),
      battle.effects.monsterBurn.map(bn => s"🔥 -${bn.damageOn(maxHp)} ❤")
    ).flatten
    if (parts.isEmpty) "" else parts.mkString(" (", ")(", ")")
  }

  // ── Ход монстра ─────────────────────────────────────────────────────────────

  /** Ход моба после атаки игрока. `log` — строки, накопленные в фазе игрока;
    * между сегментами игрока и монстра вставляется пустая строка-разделитель
    * (в склейке `mkString("\n")` даёт двойной перенос).
    */
  private def monsterPhase(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long]
  ): Task[TurnResult] =
    for {
      ticked <- ZIO.succeed(battle.tickBuffs(skip))
      eff       = hero.effectiveFightStats(nowMs)
      buffedEff = ticked.heroBattleState.applyTo(eff)
      monster   = ticked.toMonster
      hitRoll <- Random.nextIntBetween(1, 101)
      dodge = playerDodgeChance(hero, ticked, nowMs)
      // Тот же порядок округления, что на экране боя: 100 - floor(dodge).
      mobHitPct = 100 - dodge.toInt

      // 1) Обычная атака моба — обновляем hp/armor героя и (для Шипастого/Крепкости) бой.
      atkResult <-
        if (hitRoll > dodge) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monster.fightStats.atk * spread / 100L).max(1L)
            reduction = BattleState.damageReduction(
              protection = buffedEff.defence,
              defenderInt = hero.effectiveBaseStats(nowMs).int,
              // «Интеллект» моба в атаке = его атака (у мобов нет отдельного стата интеллекта).
              attackerInt = monster.fightStats.atk,
              bonusPct = ticked.heroBattleState.reductionBonusPct
            )
            baseReduced = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            // «Непробиваемый»: 20% шанс срезать полученный урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            reducedDamage =
              if (impenTriggered) (baseReduced * (100L - PassiveKind.Impenetrable.ReductionPct) / 100L).max(1L)
              else baseReduced
            (newHp, newArmor) = MonsterSkill.applyPhysicalDamage(ticked, hero, reducedDamage)
            // «Крепкость»: при обнулении брони 25% шанс одноразово восстановить 10% макс.брони.
            armorEmptied = hero.fightStats.armor > 0 && newArmor <= 0
            toughTriggered <- chanceRoll(
              armorEmptied && !ticked.toughnessUsed && hero.passives.hasToughness,
              PassiveKind.Toughness.TriggerPct
            )
            restoredArmor =
              if (toughTriggered) (hero.effectiveMaxArmor(nowMs) * PassiveKind.Toughness.RestorePct / 100L).max(1L) else 0L
            // «Шипастый»: вернуть 5% полученного урона врагу (по HP моба, мимо брони).
            thorns = if (hero.passives.hasSpiky) (reducedDamage * PassiveKind.Spiky.ThornsPct / 100L).max(1L) else 0L
            battleAfterAtk = ticked.copy(
              monsterCurrentHp = (ticked.monsterCurrentHp - thorns).max(0L),
              toughnessUsed    = ticked.toughnessUsed || toughTriggered
            )
            lines = List(
              Some(content.format("battle.mobHit", "damage" -> reducedDamage.toString, "monster" -> monster.name)),
              Option.when(impenTriggered)(content.text("battle.impenetrable")),
              Option.when(toughTriggered)(content.format("battle.toughness", "armor" -> restoredArmor.toString)),
              Option.when(thorns > 0)(content.format("battle.thorns", "damage" -> thorns.toString, "monster" -> monster.name))
            ).flatten.mkString("\n")
          } yield (newHp, newArmor + restoredArmor, lines, battleAfterAtk)
        } else
          ZIO.succeed(
            (
              hero.fightStats.hp,
              hero.fightStats.armor,
              content.format(
                "battle.mobMiss",
                "monster" -> monster.name,
                "chance"  -> mobHitPct.toString
              ),
              ticked
            )
          )
      (hpAfterAtk, armorAfterAtk, mobLine, battleAfterAtk) = atkResult
      heroAfterAtk = hero.copy(fightStats =
        hero.fightStats.copy(hp = hpAfterAtk, armor = armorAfterAtk)
      )

      // 2) Каст скилла моба — независимый бросок шанса применения умения. Шанс
      // зависит ТОЛЬКО от редкости моба (см. monsterSkillHitChance). Если прокнул —
      // равновероятно выбираем applicable скилл из 4. Скилл применяется ПОВЕРХ
      // обычной атаки и может быть как уроном (CrushingStrike — мимо брони и
      // damageReduction), так и хилом/починкой моба.
      castResult <-
        // Моб не кастует, если герой мёртв или моб добит Шипастым (battleAfterAtk).
        if (heroAfterAtk.fightStats.hp <= 0 || battleAfterAtk.monsterCurrentHp <= 0)
          ZIO.succeed((battleAfterAtk, heroAfterAtk, ""))
        else
          for {
            skillRoll <- Random.nextIntBetween(1, 101)
            skillChance = BattleState.monsterSkillHitChance(battleAfterAtk.rarity.factor)
            applicable = MonsterSkill.values
              .filter(_.applicable(battleAfterAtk))
              .toVector
            out <-
              if (skillRoll <= skillChance && applicable.nonEmpty)
                for {
                  idx <- Random.nextIntBetween(0, applicable.size)
                  ms   = applicable(idx)
                  cast = ms.cast(battleAfterAtk, heroAfterAtk, nowMs)
                } yield (
                  cast.battle,
                  heroAfterAtk.copy(
                    fightStats = heroAfterAtk.fightStats
                      .copy(hp = cast.heroHp, armor = cast.heroArmor)
                  ),
                  cast.line
                )
              else ZIO.succeed((battleAfterAtk, heroAfterAtk, ""))
          } yield out
      (finalBattle, finalHero, castLine) = castResult

      // 3) Конец раунда: тик статус-эффектов. Стадии героя и монстра — раздельные,
      // каждая со своей строкой. Применяются только пока герой жив.
      heroAlive = finalHero.fightStats.hp > 0
      (tickedHero, battleAfterHero, heroEffectLine) =
        if (heroAlive) tickHeroEffects(finalHero, finalBattle, nowMs)
        else (finalHero, finalBattle, "")
      (tickedBattle, monsterEffectLine) =
        if (heroAlive) tickMonsterEffects(battleAfterHero, monster.name)
        else (battleAfterHero, "")

      // Реген энергии в конце хода: +(Интеллект + 0.5·Ловкость), не меньше 1 и не
      // выше максимума. «Сосредоточенный» множит реген на 1.1. Пока герой жив.
      finalHeroWithEnergy =
        if (heroAlive) {
          val b        = tickedHero.effectiveBaseStats(nowMs)
          // «Сосредоточенный» (пассивка) и черепа в оружии множат реген энергии.
          val regen    = ((b.int + 0.5 * b.agi) * tickedHero.passives.energyRegenMult * tickedHero.gems.energyRegenMult).toLong.max(1L)
          val maxEn    = tickedHero.maxEnergy(nowMs)
          val newEn    = (tickedHero.fightStats.energy + regen).min(maxEn)
          tickedHero.copy(fightStats = tickedHero.fightStats.copy(energy = newEn))
        } else tickedHero

      // Сегмент монстра: пустой разделитель, строка атаки, затем (в исходном
      // порядке) каст моба, тик яда моба, тик регена героя — только непустые.
      monsterLog = {
        // К строке атаки моба приписываем сводку активных DoT (яд/кровь/огонь) —
        // берём состояние ДО тика конца раунда (finalBattle), как на стороне игрока.
        val base       = (log :+ "") :+ (mobLine + dotIndicators(finalBattle))
        val withCast   = if (castLine.nonEmpty) base :+ castLine else base
        val withPoison = if (monsterEffectLine.nonEmpty) withCast :+ monsterEffectLine else withCast
        if (heroEffectLine.nonEmpty) withPoison :+ heroEffectLine else withPoison
      }

      outcome =
        if (finalHeroWithEnergy.fightStats.hp <= 0) Outcome.Death
        else if (tickedBattle.monsterCurrentHp <= 0) Outcome.Victory
        else Outcome.Continue
    } yield TurnResult(finalHeroWithEnergy, tickedBattle, monsterLog, outcome)

  /** Тик эффектов ГЕРОЯ в конце раунда: реген лечит на `pct`% макс.HP и слабеет.
    * Возвращает обновлённых героя и бой (с ослабленным регеном) плюс строку
    * эффекта (пустая, если регена нет). */
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
          content.format("battle.regenTick", "healed" -> healed.toString)
        )
      case None => (hero, battle, "")
    }

  /** Тик DoT-эффектов МОНСТРА в конце раунда (каждый снимает `pct`% макс.HP мимо
    * брони). Порядок: яд → кровотечение → горение. Яд слабеет ([[Poison.decayed]]),
    * кровотечение НЕ затухает, горение усиливается ([[Burn.grown]]). Возвращает
    * обновлённый бой и склеенную строку эффектов (пустую, если DoT нет). */
  private def tickMonsterEffects(
      battle: SoloPveBattle,
      monsterName: String
  ): (SoloPveBattle, String) = {
    val maxHp = battle.monsterStats.hp
    var eff   = battle.effects
    var hp    = battle.monsterCurrentHp
    var lines = Vector.empty[String]

    eff.monsterPoison.foreach { p =>
      val dmg = p.damageOn(maxHp)
      hp = (hp - dmg).max(0L)
      eff = eff.copy(monsterPoison = p.decayed)
      lines = lines :+ content.format("battle.poisonTick", "damage" -> dmg.toString, "monster" -> monsterName)
    }
    eff.monsterBleed.foreach { b =>
      val dmg = b.damageOn(maxHp)
      hp = (hp - dmg).max(0L)
      // Кровотечение не затухает — сила остаётся прежней.
      lines = lines :+ content.format("battle.bleedTick", "damage" -> dmg.toString, "monster" -> monsterName)
    }
    eff.monsterBurn.foreach { bn =>
      val dmg = bn.damageOn(maxHp)
      hp = (hp - dmg).max(0L)
      eff = eff.copy(monsterBurn = Some(bn.grown))
      lines = lines :+ content.format("battle.burnTick", "damage" -> dmg.toString, "monster" -> monsterName)
    }
    (battle.copy(monsterCurrentHp = hp, effects = eff), lines.mkString("\n"))
  }

  // ── Ход игрока: активный навык ──────────────────────────────────────────────

  /** Применение активного навыка. Проверки (слот существует / готов / хватает
    * энергии) дают Continue-сообщение без траты хода. Умение ВСЕГДА срабатывает:
    * списываем энергию (в героя, персист — в commit), применяем эффект с ±20%
    * разбросом, ставим cd и инкрементируем uses. */
  private def skillTurn(itemId: Long)(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    battle.slotByItem(itemId) match {
      case None =>
        cont(hero, battle, content.text("battle.skillUnavailable"))
      case Some(slot) if slot.cooldown > 0 =>
        cont(hero, battle, content.text("battle.skillOnCooldown"))
      case Some(slot) if hero.fightStats.energy < slot.skill.energyCost(hero) =>
        cont(
          hero,
          battle,
          content.format(
            "battle.notEnoughEnergy",
            "cost"   -> slot.skill.energyCost(hero).toString,
            "energy" -> hero.fightStats.energy.toString
          )
        )
      case Some(slot) =>
        val cost      = slot.skill.energyCost(hero)
        val heroAfter = hero.copy(fightStats =
          hero.fightStats.copy(energy = (hero.fightStats.energy - cost).max(0L)))
        skillHit(heroAfter, battle, slot, nowMs)
    }

  /** Применяет эффект навыка (матч по Skill.Effect). После урона/лечения идёт
    * базовая атака игрока (кроме случая, когда моб убит самим скиллом). Кулдаун
    * использованного слота не тикается в этом же ходу (`skip`). Изменения статов
    * героя (энергия/HP Кровавой жатвы/лечение) едут в `hero` результата и
    * персистятся в commit — здесь никаких записей в БД. */
  private def skillHit(
      hero: Hero,
      battle: SoloPveBattle,
      slot: SkillSlotState,
      nowMs: Long
  ): Task[TurnResult] =
    for {
      spread <- Random.nextLongBetween(80L, 121L)
      raw = (slot.skill.baseValue(hero, nowMs) * spread / 100.0).toLong.max(1L)
      bumped = battle.updateSlot(slot.itemId)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1))
      skip = Set(slot.itemId)
      tmpl = slot.skill.hitTemplate
      // Урон, срезанный защитой моба (для эффектов, которые «упираются» в защиту).
      // Дебаф Холода снижает итоговое %-снижение цели на monsterColdDefenceCut п.п.;
      // временный дебаф защиты (комбо) режет саму защиту (effectiveMonsterDefence).
      reduced = {
        val def0 = effectiveMonsterDefence(battle)
        val red0 = BattleState.damageReduction(
          protection  = def0,
          defenderInt = def0,
          attackerInt = hero.effectiveBaseStats(nowMs).int,
          bonusPct    = 0L)
        val red = (red0 - battle.effects.monsterColdDefenceCut / 100.0).max(0.0)
        (raw * (1.0 - red)).toLong.max(1L)
      }
      result <- slot.skill.effect match {
        case Skill.Effect.Damage(reducedByDefence) =>
          val value = if (reducedByDefence) reduced else raw
          dealSkillDamage(hero, bumped, value, tmpl.replace("{}", value.toString), nowMs, skip)

        case Skill.Effect.BleedDamage(pct) =>
          // Урон сразу + наложение (стак) КРОВОТЕЧЕНИЯ на моба (отдельно от яда).
          val stacked  = bumped.effects.monsterBleed.map(_.stackedWith(pct)).getOrElse(Bleed(pct))
          val bled     = bumped.copy(effects = bumped.effects.copy(monsterBleed = Some(stacked)))
          val bleedMsg = tmpl.replace("{}", raw.toString) + "\n" +
            content.format("battle.bleedApplied", "monster" -> battle.toMonster.name)
          dealSkillDamage(hero, bled, raw, bleedMsg, nowMs, skip)

        case Skill.Effect.WeakSpotStrike =>
          for {
            roll <- Random.nextIntBetween(1, 101)
            chance = (2L * hero.effectiveBaseStats(nowMs).int - battle.monsterLvl).max(0L).min(95L)
            doubled = roll <= chance
            value   = if (doubled) raw * 2L else raw
            line    = tmpl.replace("{}", value.toString) +
                      (if (doubled) "\n" + content.text("battle.weakSpotDouble") else "")
            r <- dealSkillDamage(hero, bumped, value, line, nowMs, skip)
          } yield r

        case Skill.Effect.BloodHarvest =>
          // Урон (уже включает +8% тек.HP в baseValue) ценой 10% текущего HP.
          // Раненый герой едет дальше в цепочке и персистится в commit ЕДИНООБРАЗНО
          // во всех ветках победы — HP-цена не теряется при добивании базовой атакой.
          val hpLost      = (hero.fightStats.hp * Skill.BloodHarvestHpCostPct / 100L).max(0L)
          val woundedHero = hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp - hpLost).max(1L)))
          val line        = tmpl.replaceFirst("\\{\\}", raw.toString).replaceFirst("\\{\\}", hpLost.toString)
          dealSkillDamage(woundedHero, bumped, raw, line, nowMs, skip)

        case Skill.Effect.Heal =>
          // «Целителя» множит активное лечение на 1.1.
          val amount   = (raw * hero.passives.healMult).toLong.max(1L)
          val maxHp    = hero.effectiveMaxHp(nowMs)
          val newHp    = (hero.fightStats.hp + amount).min(maxHp)
          val healed     = newHp - hero.fightStats.hp
          val healedHero = hero.copy(fightStats = hero.fightStats.copy(hp = newHp))
          playerStrike(healedHero, bumped, nowMs, Vector(tmpl.replace("{}", healed.toString)), skip)

        case Skill.Effect.RepairArmor =>
          val (newStats, gained) = repairArmor(hero, raw, nowMs)
          playerStrike(hero.copy(fightStats = newStats), bumped, nowMs, Vector(tmpl.replace("{}", gained.toString)), skip)

        case Skill.Effect.GuardRepair(defencePct, turns) =>
          // Восстановление брони + временный %-баф итоговой защиты.
          val (newStats, gained) = repairArmor(hero, raw, nowMs)
          val guarded = bumped.copy(heroBattleState = bumped.heroBattleState.add(
            Buff(0L, 0L, 0L, dodgePct = 0L, defencePct = defencePct, turnsLeft = Some(turns))))
          playerStrike(hero.copy(fightStats = newStats), guarded, nowMs, Vector(tmpl.replace("{}", gained.toString)), skip)
      }
    } yield result

  /** Наносит `value` урона мобу (сначала броня, затем HP). Если моб убит — победа
    * без базовой атаки; иначе — базовая атака игрока тем же ходом. Никаких записей
    * в БД: итог (в т.ч. изменённые статы героя) уезжает в TurnResult и
    * персистится в commit. */
  private def dealSkillDamage(
      hero: Hero,
      battle: SoloPveBattle,
      value: Long,
      skillLine: String,
      nowMs: Long,
      skip: Set[Long]
  ): Task[TurnResult] = {
    // Урон навыка тоже несёт стихию оружия: грейд-множитель + раздельный урон по
    // броне/HP. Проки стихий роллятся отдельным броском (как и на обычной атаке).
    val scaled = (value * hero.gems.elementalDamageMult).toLong.max(1L)
    val (armorDmg, hpDmg) = splitElementalDamage(battle.monsterCurrentArmor, scaled, hero.gems)
    val newArmor = battle.monsterCurrentArmor - armorDmg
    val newHp    = (battle.monsterCurrentHp - hpDmg).max(0L)
    val hit      = battle.copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor)
    if (newHp <= 0) ZIO.succeed(TurnResult(hero, hit, Vector(skillLine + dotIndicators(hit)), Outcome.Victory))
    else
      resolveElementProcs(hero, hit).flatMap { case (afterProcs, elemLog) =>
        val lines = Vector(skillLine + dotIndicators(afterProcs)) ++ elemLog
        if (afterProcs.monsterCurrentHp <= 0) ZIO.succeed(TurnResult(hero, afterProcs, lines, Outcome.Victory))
        else playerStrike(hero, afterProcs, nowMs, lines, skip)
      }
  }

  /** Восстановление брони героя на `value` (кап — эффективный максимум). Возвращает
    * новые статы и фактически восстановленную величину. */
  private def repairArmor(hero: Hero, value: Long, nowMs: Long): (FightStats, Long) = {
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    val newArmor = (hero.fightStats.armor + value).min(maxArmor)
    (hero.fightStats.copy(armor = newArmor), newArmor - hero.fightStats.armor)
  }

  // ── Фляга и пояс (расходники, ход монстра не провоцируют) ────────────────────

  /** Бросок «сработала ли пассивка» с шансом `pct`% — но ТОЛЬКО если она активна
    * (`active`). Если пассивки нет, случайное число не тратится (важно для
    * детерминированных боевых тестов): сразу `false`. */
  private def chanceRoll(active: Boolean, pct: Long): Task[Boolean] =
    if (!active) ZIO.succeed(false)
    else Random.nextIntBetween(1, 101).map(_ <= pct)

  /** Пассивка «Быстрые руки»: с 25% шансом расходник не тратит раунд (можно применить
    * ещё раз). Возвращает флаг «раунд потрачен» (в `consumableUsedThisRound`) и
    * приписку в лог, если прокнуло. */
  private def quickHandsRoll(hero: Hero): Task[(Boolean, Vector[String])] =
    chanceRoll(hero.passives.hasQuickHands, PassiveKind.QuickHands.RepeatChancePct).map { repeated =>
      if (repeated) (false, Vector(content.text("battle.quickHands")))
      else (true, Vector.empty)
    }

  private def flaskTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val flask = hero.equipment.flask
    flask.details match {
      case f: ItemDetails.Flask if f.charges <= 0 =>
        cont(hero, battle, content.text("battle.flaskEmpty"))
      case _: ItemDetails.Flask if battle.consumableUsedThisRound =>
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      case f: ItemDetails.Flask =>
        quickHandsRoll(hero).map { case (consumed, qhLog) =>
          val newEquipment  = hero.equipment.copy(flask = flask.copy(details = f.spent))
          val updatedBattle = battle.copy(consumableUsedThisRound = consumed)
          f.effect match {
            case FlaskEffect.HealPercent(pct) =>
              val maxHp    = hero.effectiveMaxHp(nowMs)
              // «Целителя» множит лечение фляги на 1.1.
              val healAmt  = (maxHp * pct / 100L * hero.passives.healMult).toLong.max(1L)
              val newHp    = (hero.fightStats.hp + healAmt).min(maxHp)
              val healed   = newHp - hero.fightStats.hp
              // Глоток фляги дополнительно восстанавливает 5% максимума Энергии.
              val maxEn      = hero.maxEnergy(nowMs)
              val newEnergy  = (hero.fightStats.energy + (maxEn * 5 / 100L).max(1L)).min(maxEn)
              val energyBack = newEnergy - hero.fightStats.energy
              val newStats   = hero.fightStats.copy(hp = newHp, energy = newEnergy)
              val msg = content.format(
                "battle.flaskUsed",
                "healed" -> healed.toString,
                "hp"     -> newHp.toString,
                "max"    -> maxHp.toString,
                "energy" -> energyBack.toString
              )
              TurnResult(hero.copy(equipment = newEquipment, fightStats = newStats), updatedBattle, Vector(msg) ++ qhLog, Outcome.Continue)
            case FlaskEffect.AddBuff(buff, rounds) =>
              val timedBuff = buff.copy(turnsLeft = Some(rounds))
              val newBattle = updatedBattle.copy(heroBattleState = updatedBattle.heroBattleState.add(timedBuff))
              TurnResult(hero.copy(equipment = newEquipment), newBattle, Vector(content.text("battle.flaskBuff")) ++ qhLog, Outcome.Continue)
          }
        }
      case _ =>
        cont(hero, battle, content.text("battle.noFlask"))
    }
  }

  /** Применение зелья пояса. Зеркалит [[flaskTurn]]: проверки пусто/уже-пили, трата
    * заряда, затем эффект по типу зелья ([[applyPotion]]). */
  private def beltTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val belt = hero.equipment.belt
    belt.details match {
      case b: ItemDetails.Belt if b.charges <= 0 =>
        cont(hero, battle, content.text("battle.beltEmpty"))
      case _: ItemDetails.Belt if battle.consumableUsedThisRound =>
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      case b: ItemDetails.Belt =>
        quickHandsRoll(hero).map { case (consumed, qhLog) =>
          val equipment = hero.equipment.copy(belt = belt.copy(details = b.spent))
          val battle1   = battle.copy(consumableUsedThisRound = consumed)
          val (newStats, newBattle, msg) = applyPotion(hero, battle1, b.potion, nowMs)
          TurnResult(hero.copy(equipment = equipment, fightStats = newStats), newBattle, Vector(msg) ++ qhLog, Outcome.Continue)
        }
      case _ =>
        cont(hero, battle, content.text("battle.noBelt"))
    }
  }

  /** Эффект конкретного зелья пояса. Мгновенные (лечение/металл/энергия) меняют
    * статы героя; временные (атака/защита/уворот) кладут баф на 5 ходов;
    * яд/реген ставят соответствующий статус-эффект. Возвращает новые статы героя,
    * обновлённый бой и строку эффекта — запись выполняет вызывающий (commit). */
  private def applyPotion(
      hero: Hero,
      battle: SoloPveBattle,
      potion: PotionKind,
      nowMs: Long
  ): (FightStats, SoloPveBattle, String) = {
    val buffed = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    potion match {
      case PotionKind.Healing =>
        val maxHp  = hero.effectiveMaxHp(nowMs)
        // «Целителя» множит лечение зелья на 1.1.
        val heal   = (maxHp * 25 / 100L * hero.passives.healMult).toLong.max(1L)
        val newHp  = (hero.fightStats.hp + heal).min(maxHp)
        val healed = newHp - hero.fightStats.hp
        (hero.fightStats.copy(hp = newHp), battle,
          content.format("battle.beltHeal", "healed" -> healed.toString, "hp" -> newHp.toString, "max" -> maxHp.toString))

      case PotionKind.Metal =>
        val maxArmor = hero.effectiveMaxArmor(nowMs)
        val restore  = (maxArmor * 20 / 100L).max(1L)
        val newArmor = (hero.fightStats.armor + restore).min(maxArmor)
        val gained   = newArmor - hero.fightStats.armor
        (hero.fightStats.copy(armor = newArmor), battle,
          content.format("battle.beltMetal", "armor" -> gained.toString, "cur" -> newArmor.toString, "max" -> maxArmor.toString))

      case PotionKind.Poison =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroPoisonousAttacks = true)),
          content.text("battle.beltPoison"))

      case PotionKind.Regeneration =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroRegen = Some(Regen.onDrink))),
          content.format("battle.beltRegen", "pct" -> Regen.OnDrink.toString))

      case PotionKind.Evasion =>
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, 0L, dodgePct = 10L, defencePct = 0L, turnsLeft = Some(ItemDetails.Belt.BuffRounds)))),
          content.format("battle.beltEvasion", "rounds" -> ItemDetails.Belt.BuffRounds.toString))

      case PotionKind.Energy =>
        val maxEn     = hero.maxEnergy(nowMs)
        val restore   = (maxEn * 25 / 100L).max(1L)
        val newEnergy = (hero.fightStats.energy + restore).min(maxEn)
        val gained    = newEnergy - hero.fightStats.energy
        (hero.fightStats.copy(energy = newEnergy), battle,
          content.format("battle.beltEnergy", "energy" -> gained.toString, "cur" -> newEnergy.toString, "max" -> maxEn.toString))

      case PotionKind.Attack =>
        val bonus = (buffed.atk * 5 / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(atk = bonus, 0L, 0L, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(ItemDetails.Belt.BuffRounds)))),
          content.format("battle.beltAttack", "atk" -> bonus.toString, "rounds" -> ItemDetails.Belt.BuffRounds.toString))

      case PotionKind.Defence =>
        val bonus = (buffed.defence * 10 / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, defence = bonus, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(ItemDetails.Belt.BuffRounds)))),
          content.format("battle.beltDefence", "defence" -> bonus.toString, "rounds" -> ItemDetails.Belt.BuffRounds.toString))
    }
  }

  // ── Бегство ─────────────────────────────────────────────────────────────────

  /** Экран подтверждения бегства (чистая навигация, состояние не меняется). */
  private def flee(user: User, renderer: Renderer): Task[StateType] =
    renderer
      .show(user, content.screen("battle.fleeConfirm"))
      .as(StateType.Battle)

  /** Подтверждённое бегство: моб делает один удар (без каста/тиков/регена, как и
    * прежде). Умер герой — Death; иначе — Fled (бой очистит commit). */
  private def fleeTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    for {
      eff       <- ZIO.succeed(hero.effectiveFightStats(nowMs))
      buffedEff = battle.heroBattleState.applyTo(eff)
      monster   = battle.toMonster
      hitRoll <- Random.nextIntBetween(1, 101)
      // «Сверкающие» дают +25% к уклонению именно при бегстве.
      result <-
        if (hitRoll > playerDodgeChance(hero, battle, nowMs, fleeing = true)) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monster.fightStats.atk * spread / 100L).max(1L)
            reduction = BattleState.damageReduction(
              protection = buffedEff.defence,
              defenderInt = hero.effectiveBaseStats(nowMs).int,
              attackerInt = monster.fightStats.atk,
              bonusPct = battle.heroBattleState.reductionBonusPct
            )
            baseReduced = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            // «Непробиваемый»: 20% шанс срезать урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            reducedDamage =
              if (impenTriggered) (baseReduced * (100L - PassiveKind.Impenetrable.ReductionPct) / 100L).max(1L)
              else baseReduced
            buffReduct  = math.min(battle.heroBattleState.armorBonus, reducedDamage)
            afterBuff   = reducedDamage - buffReduct
            curArmor    = hero.fightStats.armor.max(0L)
            armorAbsorb = math.min(curArmor, afterBuff)
            hpDmg       = afterBuff - armorAbsorb
            newArmor    = curArmor - armorAbsorb
            newHp       = (hero.fightStats.hp - hpDmg).max(0L)
            hero2       = hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor))
            mobLine     = content.format("battle.mobHit", "damage" -> reducedDamage.toString, "monster" -> monster.name)
          } yield
            if (newHp <= 0) TurnResult(hero2, battle, Vector(mobLine), Outcome.Death)
            else TurnResult(hero2, battle, Vector(mobLine), Outcome.Fled)
        } else
          ZIO.succeed(TurnResult(hero, battle, Vector.empty, Outcome.Fled))
    } yield result

  // ── Победа ──────────────────────────────────────────────────────────────────

  /** Награды за победу: РОЛЛ + ВСЕ ЗАПИСИ в БД (опыт, лут в scene_data, очистка
    * боя, куб Азата, путь вглубь). Ничего не показывает — сообщения собирает
    * [[showVictory]] по возвращённому [[VictoryOutcome]]. Разделение сделано,
    * чтобы победа была ЗАФИКСИРОВАНА до первого сетевого вызова (см. commit). */
  private def applyVictory(
      user: User,
      hero: Hero,
      battle: SoloPveBattle
  ): Task[VictoryOutcome] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      azat <- loadAzat(user)
      // Недельное благословение Азата: +10% опыта/золота, +10% редкости, +5% доп. дроп.
      blessed = azat.blessingActive(now)
      baseExp = (hero.dungeonLevel.toLong * battle.rarity.factor).toLong.max(1L)
      expGained = if (blessed) (baseExp * (100L + BattleState.BlessingBonusPct) / 100L).max(1L) else baseExp
      leveled = hero.gainExp(expGained)
      // лут катаем чистым ядром; начисление (инвентарь/золото) — в LootState
      seed <- Random.nextLong
      monster = battle.toMonster
      (baseDrops, rngAfter) = LootGenerator.roll(
        battle.rarity,
        monster.race,
        hero.dungeonLevel.toLong,
        Rng(seed),
        gearChanceBonusPct = hero.gems.gearDropBonusPct,
        rarityBumpPct = if (blessed) BattleState.BlessingBonusPct else 0L
      )
      // «Таксидермиста»/«Ювелира» дают отдельные доп. дропы поверх основного лута.
      (extraDrops, rngAfter2) = LootGenerator.rollPassiveDrops(
        hero.passives.hasTaxidermist,
        hero.passives.hasJeweler,
        battle.rarity,
        monster.race,
        hero.dungeonLevel.toLong,
        rngAfter
      )
      // Благословение: 5% шанс дополнительной экипировки после боя.
      (blessingGear, _) =
        if (blessed) LootGenerator.rollBlessingExtraGear(BattleState.BlessingExtraDropPct, battle.rarity, hero.dungeonLevel.toLong, rngAfter2)
        else (Option.empty[pangea.model.item.Item], rngAfter2)
      drops = baseDrops ++ extraDrops
      // routing события (returnState/eventData) кладётся в scene_data ДО боя
      // (напр. цепочка «мобы с сокровищем»); переносим его в добычу, чтобы экран
      // добычи знал, куда вернуться. Для обычного боя scene_data пуст → None.
      prev <- heroDao
        .readSceneData(user.userId)
        .map(_.flatMap(_.as[LootState.LootData].toOption))
      goldScalePct = if (blessed) 100L + BattleState.BlessingBonusPct else 100L
      lootData = LootState.LootData(
        items = drops.collect {
          case LootGenerator.LootDrop.Gear(i)    => i
          case LootGenerator.LootDrop.Trophy(i)  => i
          case LootGenerator.LootDrop.MapHalf(i) => i
        } ++ blessingGear.toList,
        golds = drops.collect { case LootGenerator.LootDrop.Gold(a, _) => a * goldScalePct / 100L },
        returnState = prev.flatMap(_.returnState),
        eventData = prev.flatMap(_.eventData)
      )
      // Первый поверженный легендарный моб роняет квестовый «Неактивный куб Азата»
      // (если куба ещё нет и он не был куплен). Хранится флагом в azat_data.
      cubeDropped = battle.rarity == pangea.model.monster.Rarity.Legendary && azat.cubeAbsent
      _ <- ZIO.when(cubeDropped)(saveAzat(user, azat.copy(cube = CubeStatus.FoundInactive)))
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
    } yield VictoryOutcome(
      monsterName     = monster.name,
      expGained       = expGained,
      newLvl          = Option.when(leveled.lvl > hero.lvl)(leveled.lvl),
      unlocksDarkness = unlocksDarkness,
      cubeDropped     = cubeDropped
    )

  /** Показ итогов победы — вызывается ПОСЛЕ того, как [[applyVictory]] всё
    * записал. Падение любого сообщения уже не откатывает бой. */
  private def showVictory(
      user: User,
      outcome: VictoryOutcome,
      renderer: Renderer
  ): Task[Unit] =
    for {
      _ <- renderer.show(
        user,
        Screen(
          content.format(
            "battle.victory",
            "monster" -> outcome.monsterName,
            "exp"     -> outcome.expGained.toString
          ),
          Nil
        )
      )
      _ <- ZIO.when(outcome.unlocksDarkness)(
        renderer.show(
          user,
          Screen(content.text("battle.darknessConquered"), Nil)
        )
      )
      _ <- ZIO.foreachDiscard(outcome.newLvl)(lvl =>
        renderer.show(user, Screen(s"Вы получили новый уровень $lvl!", Nil))
      )
      _ <- ZIO.when(outcome.cubeDropped)(
        renderer.show(user, Screen(content.text("battle.cubeFound"), Nil))
      )
    } yield ()

  private def loadAzat(user: User): Task[AzatState] =
    heroDao.readAzatData(user.userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  // ── Экран боя ───────────────────────────────────────────────────────────────

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
    val buffedEff = effWithAir(hero, battle, nowMs)
    val playerDodgePct  = playerDodgeChance(hero, battle, nowMs).toInt
    val monsterDodgePct = mobDodgeChance(buffedEff.accuracy, battle).toInt
    val mobHitPct       = 100 - playerDodgePct
    val heroHitPct      = 100 - monsterDodgePct
    val reductionPct = (BattleState.damageReduction(
      protection = buffedEff.defence,
      defenderInt = hero.effectiveBaseStats(nowMs).int,
      attackerInt = battle.monsterStats.atk,
      bonusPct = battle.heroBattleState.reductionBonusPct
    ) * 100.0).toInt
    val mobSkillHitPct = BattleState.monsterSkillHitChance(battle.rarity.factor).toInt
    val maxEnergy     = hero.maxEnergy(nowMs)
    // Статус-эффекты рядом со статами своей сущности: яд у HP моба (-урон/ход),
    // реген у HP/брони героя (+восстановление/ход). Реген HP = зелье регенерации
    // (затухающее) + пассивка «Целебный» (4% макс.HP); реген брони =
    // «Самовосстанавливающийся» (4% макс.брони). Показываем суммарной припиской.
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    // Сводка DoT у HP моба: яд 🟢 / кровотечение 🔴 / горение 🔥 (урон/ход).
    val monsterMaxHp = battle.monsterStats.hp
    val monsterPoison = {
      val parts = List(
        battle.effects.monsterPoison.map(p => s"🟢-${p.damageOn(monsterMaxHp)}"),
        battle.effects.monsterBleed.map(b => s"🔴-${b.damageOn(monsterMaxHp)}"),
        battle.effects.monsterBurn.map(bn => s"🔥-${bn.damageOn(monsterMaxHp)}")
      ).flatten
      if (parts.isEmpty) "" else " (" + parts.mkString(" ") + ")"
    }
    val beltHpRegen    = battle.effects.heroRegen.map(_.healOn(maxHp)).getOrElse(0L)
    // Реген HP/ход = зелье + «Целебный» (% макс.HP) + черепа в снаряжении (промилле макс.HP).
    val hpRegenTotal   = beltHpRegen + maxHp * hero.passives.hpRegenPct / 100L + maxHp * hero.gems.hpRegenPerMille / 1000L
    val heroRegen      = if (hpRegenTotal > 0) s" (+$hpRegenTotal)" else ""
    val armorRegen     = maxArmor * hero.passives.armorRegenPct / 100L
    val heroArmorRegen = if (armorRegen > 0) s" (+$armorRegen)" else ""
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
      "heroArmorRegen"  -> heroArmorRegen,
      "heroArmor"       -> hero.fightStats.armor.min(maxArmor).toString,
      "heroMaxArmor"    -> maxArmor.toString,
      "heroAtk"        -> buffedEff.atk.toString,
      "heroReduction"  -> s"$reductionPct%",
      "heroEnergy"     -> hero.fightStats.energy.min(maxEnergy).toString,
      "heroMaxEnergy"  -> maxEnergy.toString,
      "mobSkillHit"    -> s"$mobSkillHitPct%",
      "flaskCharges"   -> BattleState.flaskCharges(hero).toString
    )
    // Готовый навык синий, если хватает энергии на каст, и красный — если нет.
    val skillButtons = battle.skillSlots.collect {
      case slot if slot.cooldown <= 0 =>
        val enoughEnergy = hero.fightStats.energy >= slot.skill.energyCost(hero)
        pangea.engine.Choice(
          id = s"Skill_${slot.itemId}",
          label = slot.skill.label,
          color =
            if (enoughEnergy) pangea.engine.ChoiceColor.Primary
            else pangea.engine.ChoiceColor.Negative,
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

  /** Уклонение игрока от удара моба, в процентах [5;95]: защита игрока в знаменателе,
    * точность моба ×1.5. Поверх базового шанса добавляются бонус боевых бафов (зелье
    * уворота) и плоские бонусы пассивок — «Быстрые ноги»/«Сливающиеся» всегда, а
    * «Сверкающие» — только при бегстве (`fleeing`). Потолок 95% абсолютный. */
  private def playerDodgeChance(hero: Hero, battle: SoloPveBattle, nowMs: Long, fleeing: Boolean = false): Double = {
    // Буст Воздуха (+10% уклонения на airBoostTurns ходов) входит в effWithAir.
    val buffed       = effWithAir(hero, battle, nowMs)
    val agi          = hero.effectiveBaseStats(nowMs).agi
    val passiveBonus = hero.passives.dodgeBonusPct + (if (fleeing) hero.passives.fleeDodgeBonusPct else 0L)
    (BattleState.dodgeChance(agi, buffed.evasion, buffed.defence, battle.monsterStats.accuracy)
      + battle.heroBattleState.dodgeBonus + passiveBonus).min(95.0).max(5.0)
  }

  // Уклонение моба от удара игрока — та же формула: у моба нет ловкости (agi = 0),
  // в знаменателе его защита (с учётом %-дебафа комбо) и точность игрока ×1.5.
  private def mobDodgeChance(heroAccuracy: Long, battle: SoloPveBattle): Double =
    BattleState.dodgeChance(
      0L,
      battle.monsterStats.evasion,
      effectiveMonsterDefence(battle),
      heroAccuracy
    )
}

object BattleState {

  /** Исход хода — определяет переход и терминальные действия в [[BattleState.commit]]. */
  sealed trait Outcome
  object Outcome {
    case object Continue extends Outcome
    case object Victory  extends Outcome
    case object Death    extends Outcome
    case object Fled     extends Outcome
  }

  /** Уже ЗАПИСАННЫЙ итог победы — всё, что осталось показать игроку.
    * `newLvl` непуст, если ход дал новый уровень. */
  final case class VictoryOutcome(
      monsterName: String,
      expGained: Long,
      newLvl: Option[Long],
      unlocksDarkness: Boolean,
      cubeDropped: Boolean
  )

  /** Результат чистого вычисления хода: итоговый герой и бой (для персиста),
    * накопленный лог сообщений (склеивается и показывается один раз) и исход. */
  final case class TurnResult(
      hero: Hero,
      battle: SoloPveBattle,
      log: Vector[String],
      outcome: Outcome
  )

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

  /** Бонус благословения Азата (в %): к опыту, золоту и редкости добычи. */
  val BlessingBonusPct: Long = AzatState.BlessingBonusPct
  /** Шанс (в %) дополнительной экипировки после боя при благословении. */
  val BlessingExtraDropPct: Long = AzatState.BlessingExtraDropPct

  /** ЗАГЛУШКА множителя шанса каста моба от редкости. Позже может стать формулой. */
  val MonsterSkillChancePerRarity: Double = 20.0

  /** Шанс моба применить активный навык — зависит ТОЛЬКО от редкости моба:
    * `rarity.factor · MonsterSkillChancePerRarity`, в процентах [5; 95].
    * Обычный моб (factor 0.8) ≈ 16%, легендарный (factor 3) = 60%.
    */
  def monsterSkillHitChance(rarityFactor: Double): Double =
    (rarityFactor * MonsterSkillChancePerRarity).max(5.0).min(95.0)

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
    * + Iₚ) / (P + Iₚ + Iₑ × 2), затем к итогу ПРИБАВЛЯЕТСЯ bonusPct п.п. —
    * процентный баф «Заслона» бьёт по ИТОГОВОМУ снижению, а не по защите. Всё
    * вместе зажато сверху 0.7. Где `P` — защита защитника, `Iₚ` — его интеллект,
    * `Iₑ` — интеллект атакующего, `bonusPct` — прибавка в п.п. к итогу. Применяется
    * к чистому урону до брони. Соотношение «×2 для интеллекта атакующего» — из ТЗ.
    * Возврат — доля [0; 0.7].
    */
  def damageReduction(
      protection: Long,
      defenderInt: Long,
      attackerInt: Long,
      bonusPct: Long
  ): Double = {
    val numer   = (protection + defenderInt).toDouble
    val denom   = numer + attackerInt * 2.0
    val raw     = if (denom <= 0.0) 0.0 else numer / denom
    val boosted = raw + bonusPct / 100.0
    boosted.max(0.0).min(0.7)
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
