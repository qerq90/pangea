package pangea.service.state.states.arena

import pangea.model.arena.{ArenaFight, ArenaSide}
import pangea.model.battle.{ArenaRef, SoloPveBattle}
import pangea.model.hero.Hero
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
      arena               = Some(ArenaRef(fight.id, foe.userId.value)))
  }

  /** Что из боя переживает ход: эффекты, кулдауны и бафы своей стороны.
    * Свой блок к этому ходу уже отработал — снимаем. */
  def harvest(side: ArenaSide, battle: SoloPveBattle): ArenaSide =
    side.copy(effects = battle.effects, slots = battle.skillSlots,
      buffs = battle.heroBattleState, ready = true, blocked = (side.blocked - 1).max(0))

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
