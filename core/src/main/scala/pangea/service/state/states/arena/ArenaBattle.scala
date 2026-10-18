package pangea.service.state.states.arena

import pangea.model.arena.{ArenaAlly, ArenaFight, ArenaSide}
import pangea.model.battle.{ArenaRef, BattleAlly, BattleEffects, Formation, HeroBattleState, MonsterSlot, SoloPveBattle}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.monster.{Monster, Rarity}
import pangea.model.squad.AllyKind
import pangea.model.user.UserId
import pangea.service.state.states.battle.BattleState

/** Сборка боя на арене. Правда о бойцах живёт врозь: HP, броня и энергия героев
  * — в самих героях (их пишет боевой движок), союзники на песке и всё, что на
  * ком висит и какие умения на перезарядке, — в строке боя.
  *
  * Каждой стороне бой собирается заново и со своей колокольни: она герой, её
  * союзники — союзники, а соперник и его отряд — «мобы» со своими статами и
  * именами. Бить в ответ эти мобы не умеют (см. `ArenaRef`) — соперник и его
  * отряд ответят своим ходом из своего зеркала. Кто есть кто, видно по номеру
  * бойца на песке (`MonsterSlot.arenaUnit`): по нему урон и путы возвращаются
  * той стороне, чей это боец.
  */
object ArenaBattle {

  /** Номер самого соперника среди «мобов»; его союзники — от единицы. */
  val HeroUnit: Int = SoloPveBattle.ArenaHeroUnit

  /** Кто начинает: у кого ловкость с интеллектом больше. Поровну — решает
    * брошенная монета (её кидает вызывающий: здесь чистая функция). */
  def firstTurn(a: (UserId, Hero), b: (UserId, Hero), nowMs: Long, coin: Boolean): UserId = {
    def quick(h: Hero): Long = {
      val s = h.effectiveBaseStats(nowMs)
      s.agi + s.int
    }
    val (aUser, aHero) = a
    val (bUser, bHero) = b
    val diff = quick(aHero) - quick(bHero)
    if (diff > 0) aUser else if (diff < 0) bUser else if (coin) aUser else bUser
  }

  /** Бой глазами стороны `me`. Первый её ход собирается с нуля (умения со
    * снаряжения, зеркала настоя), дальше подхватывается то, что она нажила. */
  def assemble(
    fight:   ArenaFight,
    me:      ArenaSide,
    foe:     ArenaSide,
    meHero:  Hero,
    foeHero: Hero,
    nowMs:   Long
  ): SoloPveBattle = {
    val foeStats = foeHero.effectiveFightStats(nowMs).copy(
      hp    = foeHero.effectiveMaxHp(nowMs),
      armor = foeHero.effectiveMaxArmor(nowMs))
    val asMonster = Monster(0L, foeHero.lvl, foeHero.race, Rarity.Common, foeStats)
    // Отряд героя из «Отряда» сам на песок не выходит: на песке те, кто
    // записан на стороне боя (см. `ArenaSide.allies`).
    val fresh = SoloPveBattle.from(asMonster, meHero, squad = false)
    val carried =
      if (!me.ready) fresh
      else fresh.copy(skillSlots = me.slots, heroBattleState = me.buffs, effects = me.effects)
    // Сбитому с мысли умения не даются: слоты стоят на перезарядке, пока он не
    // придёт в себя. Тратить умение в пустоту было бы обиднее и непонятнее.
    val gathered =
      if (me.blocked <= 0) carried
      else carried.copy(skillSlots = carried.skillSlots.map(s => s.copy(cooldown = s.cooldown.max(me.blocked))))
    // Отряд соперника — «мобы» на своих местах; что сторона на них наложила —
    // её же, она это и тикает.
    val foeUnits = foe.allies.filter(_.ally.alive).map { a =>
      a.ally.position -> MonsterSlot(
        lvl = a.ally.lvl, race = a.ally.kind.race.entryName, rarity = Rarity.Common.entryName,
        stats = a.ally.stats, currentHp = a.ally.hp, currentArmor = a.ally.armor, marked = false,
        currentEnergy = a.ally.energy, effects = me.marks.getOrElse(a.id, BattleEffects.empty),
        customName = Some(a.ally.name), undead = a.ally.isUndead, arenaUnit = Some(a.id))
    }
    val sand = gathered.copy(
      monsterCurrentHp    = foeHero.fightStats.hp.max(0L),
      monsterCurrentArmor = foeHero.fightStats.armor.max(0L),
      customName          = Some(foe.name),
      monsterArenaUnit    = Some(HeroUnit),
      arena               = Some(ArenaRef(fight.id, foe.userId.value)),
      // Соперник поднял щит своим ходом — в этом ходе он режет удары героя.
      monsterShieldUp     = foe.shieldUp,
      // …или держит парирующий кинжал наготове: промах героя — и он ответит.
      monsterCounterPower = if (foe.counterUp) counterPower(foeHero, foe.buffs, nowMs) else 0L,
      // Каждый на своём месте: от расстояния зависит, кто кого достаёт. Сами
      // «мобы» здесь не ходят — только своим ходом, в своём зеркале. Свои
      // союзники — как в лабиринте; дом каждого — его номер на песке.
      group = gathered.group.copy(
        heroPos  = me.pos,
        activePos = foe.pos,
        allies   = me.allies.filter(_.ally.alive).map(a => a.ally.copy(home = a.id)),
        others   = foeUnits.map(_._2),
        places   = foeUnits.map(_._1),
        // Сам лежит, а отряд бьётся — сторона падает только целиком.
        heroDown = meHero.fightStats.hp <= 0L),
      // Пригвоздил соперник — правда о том на этой стороне боя.
      effects             = gathered.effects.copy(heroRootedTurns = me.rooted),
      // Сородичи на песок не прибегают: здесь дерутся те, кто вышел.
      noKin               = true)
    // Соперник лежит, а его отряд ещё на ногах — в полях ближайший из отряда.
    if (foeHero.fightStats.hp > 0L) sand
    else sand.promoteNext.getOrElse(sand)
  }

  /** Групповой бой: сторона выходит на песок со своим отрядом — герой и
    * союзники встают так же, как в лабиринте (по местам «Отряда», без пустот).
    * На песке — копии: что бы с ними ни случилось, отряд в «Отряде» прежний. */
  def withSquad(side: ArenaSide, hero: Hero): ArenaSide = {
    val formation = hero.squad.compact
    val allies = formation.inOrder.zipWithIndex.map { case (a, i) =>
      val id = side.nextAlly + i
      ArenaAlly(id, BattleAlly.of(a, hero.lvl, hero.sets.undeadAllyBoostPct, hero.sets.undeadEntersFresh).copy(home = id))
    }
    side.copy(pos = formation.heroPos, allies = side.allies ++ allies, nextAlly = side.nextAlly + allies.size)
  }

  /** Сколько бойцов выйдет с героем в групповом бою — для списка ждущих. */
  def squadSize(hero: Hero): Int = hero.squad.compact.allies.size

  /** Крыса на старте (набор «Крыса», порог 12): выходит на песок вместе с
    * героем — на ближайшее к нему свободное место. Одна такая уже есть — вторая
    * не нужна. */
  def withStartingRat(side: ArenaSide, hero: Hero): ArenaSide =
    if (!hero.sets.startsWithRat || side.allies.exists(_.ally.kind == AllyKind.Rat)) side
    else {
      val taken = side.pos :: side.allies.map(_.ally.position)
      (1 to Formation.HeroPlaces).filterNot(taken.contains)
        .sortBy(p => (math.abs(p - side.pos), -p)).headOption
        .fold(side)(p => side.copy(
          allies   = side.allies :+ ArenaAlly(side.nextAlly, BattleState.ratAlly(hero, p).copy(home = side.nextAlly)),
          nextAlly = side.nextAlly + 1))
    }

  /** Сила удара стороны для контрудара в чужом зеркале — та же формула, что у
    * обычного удара героя (см. BattleState.playerStrike), только без разброса,
    * проков и стихий: (Сила·3 + атака) с бафами боя, вполсилы без оружия, с
    * множителями пассивок, пыли и достижений. */
  def counterPower(hero: Hero, buffs: HeroBattleState, nowMs: Long): Long = {
    val f         = buffs.applyTo(hero.effectiveFightStats(nowMs))
    val weaponMod = if (hero.equipment.unarmed) 0.5 else 1.0
    ((hero.effectiveBaseStats(nowMs).str * 3L + f.atk) * weaponMod * hero.passives.finalDamageMult *
      hero.weaponDust.damageMult * Achievement.damagePct(hero) / 100.0).toLong.max(1L)
  }

  /** Что из боя переживает ход: эффекты, кулдауны и бафы своей стороны, её
    * место и отряд. Свой запрет на умения к этому ходу уже отработал — снимаем.
    * Щит, поднятый этим ходом, держится весь ход соперника. */
  def harvest(side: ArenaSide, battle: SoloPveBattle): ArenaSide = {
    // Свои союзники: кто вышел на песок только что (крыса из-под ног), тот
    // получает номер; павшие с песка уходят.
    val (allies, next) = battle.group.allies.filter(_.alive).foldLeft((Vector.empty[ArenaAlly], side.nextAlly)) {
      case ((acc, n), a) if a.home > 0 => (acc :+ ArenaAlly(a.home, a), n)
      case ((acc, n), a)               => (acc :+ ArenaAlly(n, a.copy(home = n)), n + 1)
    }
    // Что сторона наложила на соперника и его отряд: на самого соперника — в
    // effects, на его союзников — в marks. Путы — и свои, и наложенные — живут
    // на сторонах боя, а не в эффектах зеркала: иначе их считали бы дважды.
    val units  = battle.arenaUnits
    val onHero = units.collectFirst { case (_, s) if s.arenaUnit.contains(HeroUnit) => s.effects }
                   .getOrElse(BattleEffects.empty)
    val marks  = units.flatMap { case (_, s) =>
                   s.arenaUnit.filter(_ != HeroUnit).map(_ -> s.effects.copy(monsterRootedTurns = 0))
                 }.toMap
    side.copy(
      effects = battle.effects.withMonsterPart(onHero).copy(heroRootedTurns = 0, monsterRootedTurns = 0),
      slots = battle.skillSlots, buffs = battle.heroBattleState, ready = true,
      blocked = (side.blocked - 1).max(0),
      shieldUp = battle.heroShieldUp, counterUp = battle.heroCounterReady,
      pos = battle.group.heroPos, rooted = (side.rooted - 1).max(0),
      allies = allies.toList, nextAlly = next, marks = marks)
  }

  /** Что ход сделал с соперником, кроме ран героя: сбил с мысли, отбросил на
    * другое место, пригвоздил, ранил или положил его союзников. Путы после хода
    * — уже в его ходах: в этом ходе они оттикали свой запас (см. Skill.Effect.Pin). */
  def foeAfterTurn(foe: ArenaSide, battle: SoloPveBattle): ArenaSide = {
    val hero = battle.arenaUnit(HeroUnit)
    foe.copy(
      blocked = hero.fold(0)(_._2.effects.monsterSkillBlockedTurns.max(0)),
      pos     = hero.fold(foe.pos)(_._1),
      rooted  = foe.rooted.max(hero.fold(0)(_._2.effects.monsterRootedTurns)),
      // Его отряд — по номерам: раны, место после отброса; павшего больше нет.
      allies  = foe.allies.flatMap(a => battle.arenaUnit(a.id).map { case (place, s) =>
                  a.copy(ally = a.ally.copy(hp = s.currentHp, armor = s.currentArmor, position = place))
                }))
  }

  /** Что стало с соперником: его HP и броня после чужого хода. Павшего в
    * зеркале уже нет — у него ноль. */
  def foeAfter(foeHero: Hero, battle: SoloPveBattle): Hero = {
    val (hp, armor) = battle.arenaUnit(HeroUnit)
      .fold((0L, 0L)) { case (_, s) => (s.currentHp.max(0L), s.currentArmor.max(0L)) }
    foeHero.copy(fightStats = foeHero.fightStats.copy(hp = hp, armor = armor))
  }

  /** Сколько ран (HP и брони) союзники стороны получили за чужой ход: для
    * сводки, которую она прочтёт. */
  def alliesHurt(before: List[ArenaAlly], battle: SoloPveBattle): Long =
    before.map { a =>
      val (hp, armor) = battle.arenaUnit(a.id).fold((0L, 0L)) { case (_, s) => (s.currentHp, s.currentArmor) }
      (a.ally.hp - hp).max(0L) + (a.ally.armor - armor).max(0L)
    }.sum

  /** Проигравший уходит с песка на ногах: единица HP и ни клочка брони.
    * Травм, потерь и прочего арена не знает. */
  def beaten(hero: Hero): Hero =
    hero.copy(fightStats = hero.fightStats.copy(hp = 1L, armor = 0L))
}
