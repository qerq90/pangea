package pangea.service.state.states.arena

import pangea.model.arena.{ArenaFight, ArenaSide}
import pangea.model.battle.{ArenaRef, HeroBattleState, SoloPveBattle}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.monster.{Monster, Rarity}
import pangea.model.user.UserId

/** Сборка боя на арене. Правда о бойцах живёт врозь: HP, броня и энергия — в
  * самих героях (их пишет боевой движок), а что на ком висит и какие умения
  * на перезарядке — в строке боя.
  *
  * Каждой стороне бой собирается заново и со своей колокольни: она герой,
  * соперник — «моб» с его же статами и именем. Бить в ответ этот моб не умеет
  * (см. `ArenaRef`) — соперник ответит своим ходом из своего зеркала.
  */
object ArenaBattle {

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
    // Отряд на арену не выходит: squad = false, строй остаётся пустым.
    val fresh = SoloPveBattle.from(asMonster, meHero, squad = false)
    val carried =
      if (!me.ready) fresh
      else fresh.copy(skillSlots = me.slots, heroBattleState = me.buffs, effects = me.effects)
    // Сбитому с мысли умения не даются: слоты стоят на перезарядке, пока он не
    // придёт в себя. Тратить умение в пустоту было бы обиднее и непонятнее.
    val gathered =
      if (me.blocked <= 0) carried
      else carried.copy(skillSlots = carried.skillSlots.map(s => s.copy(cooldown = s.cooldown.max(me.blocked))))
    gathered.copy(
      monsterCurrentHp    = foeHero.fightStats.hp.max(0L),
      monsterCurrentArmor = foeHero.fightStats.armor.max(0L),
      customName          = Some(foe.name),
      arena               = Some(ArenaRef(fight.id, foe.userId.value)),
      // Соперник поднял щит своим ходом — в этом ходе он режет удары героя.
      monsterShieldUp     = foe.shieldUp,
      // …или держит парирующий кинжал наготове: промах героя — и он ответит.
      monsterCounterPower = if (foe.counterUp) counterPower(foeHero, foe.buffs, nowMs) else 0L,
      // Каждый на своём месте: от расстояния зависит, кто кого достаёт. Сам
      // соперник здесь не ходит — только своим ходом, в своём зеркале.
      group               = gathered.group.copy(heroPos = me.pos, activePos = foe.pos),
      // Пригвоздил соперник — правда о том на этой стороне боя.
      effects             = gathered.effects.copy(heroRootedTurns = me.rooted),
      // Сородичи на песок не прибегают: здесь дерутся двое.
      noKin               = true)
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

  /** Что из боя переживает ход: эффекты, кулдауны и бафы своей стороны.
    * Свой запрет на умения к этому ходу уже отработал — снимаем. Щит, поднятый
    * этим ходом, держится весь ход соперника. */
  def harvest(side: ArenaSide, battle: SoloPveBattle): ArenaSide =
    side.copy(
      // Путы — и свои, и наложенные на соперника — живут на сторонах боя, а не
      // в эффектах зеркала: иначе их считали бы дважды.
      effects = battle.effects.copy(heroRootedTurns = 0, monsterRootedTurns = 0), slots = battle.skillSlots,
      buffs = battle.heroBattleState, ready = true, blocked = (side.blocked - 1).max(0),
      shieldUp = battle.heroShieldUp, counterUp = battle.heroCounterReady,
      pos = battle.group.heroPos, rooted = (side.rooted - 1).max(0))

  /** Что ход сделал с соперником, кроме ран: сбил с мысли, отбросил на другое
    * место, пригвоздил. В зеркале он — тот, кто в полях. Путы после хода — уже
    * в его ходах: в этом ходе они оттикали свой запас (см. Skill.Effect.Pin). */
  def foeAfterTurn(foe: ArenaSide, battle: SoloPveBattle): ArenaSide =
    foe.copy(blocked = blockFor(battle), pos = battle.group.activePos,
      rooted = foe.rooted.max(battle.effects.monsterRootedTurns))

  /** Сколько ходов соперник не сможет колдовать после этого хода: Боевой клич
    * и комбо вешают запрет на «моба», а на арене этот моб — живой игрок. */
  def blockFor(battle: SoloPveBattle): Int = battle.effects.monsterSkillBlockedTurns.max(0)

  /** Что стало с соперником: его HP и броня после чужого хода. */
  def foeAfter(foeHero: Hero, battle: SoloPveBattle): Hero =
    foeHero.copy(fightStats = foeHero.fightStats.copy(
      hp    = battle.monsterCurrentHp.max(0L),
      armor = battle.monsterCurrentArmor.max(0L)))

  /** Проигравший уходит с песка на ногах: единица HP и ни клочка брони.
    * Травм, потерь и прочего арена не знает. */
  def beaten(hero: Hero): Hero =
    hero.copy(fightStats = hero.fightStats.copy(hp = 1L, armor = 0L))
}
