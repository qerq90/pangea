package pangea.model.battle

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.{MonsterEnergy, Skill}
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

case class SoloPveBattle(
  monsterLvl:          Long,
  monsterRace:         String,
  monsterRarity:       String,
  monsterStats:        FightStats,
  monsterCurrentHp:    Long,
  monsterCurrentArmor: Long,
  heroBattleState:     HeroBattleState = HeroBattleState.empty,
  consumableUsedThisRound: Boolean = false, // за раунд можно выпить либо флягу, либо зелье пояса
  monsterMarked:       Boolean = false,
  skillSlots:          List[SkillSlotState] = Nil,
  effects:             BattleEffects = BattleEffects.empty, // тикающие статус-эффекты (яд/реген)
  toughnessUsed:       Boolean = false, // пассивка «Крепкость» срабатывает один раз за бой
  // Вид элементаля, если это бой с минибоссом (имя варианта Elemental). У
  // обычных мобов пусто — по нему бой и отличает босса от рядового врага.
  bossKind:       Option[String] = None,
  // Какую способность элементаль применит следующей (он ходит строго по кругу),
  // и сколько зарядов он уже собрал рядом с собой: у огненного это сферы огня,
  // у каменного — валуны. Механика у них одна, поэтому счётчик общий.
  bossTurn:       Int = 0,
  bossCharges:    Int = 0,
  // Сколько раз минибосс уже поднимался после обнуления HP (Гнилой Джо).
  bossRevives:    Int = 0,
  // Текущая энергия моба. У рядовых мобов не расходуется (их скиллы бесплатны),
  // а элементаль тратит её на способности и восстанавливает по столько-то за раунд.
  monsterCurrentEnergy: Long = 0L,
  // Групповая часть боя: мобы под номерами 2+, павшие, счётчик раундов, Таран.
  // Обычный бой 1 на 1 — группа из одного, здесь пусто (см. GroupState).
  group: GroupState = GroupState.empty,
  // Сюжетный бой (ключ сценария): добыча и опыт за него решаются сюжетом, а не
  // таблицами; смерть в нём — тоже (см. DeathState).
  story: Option[String] = None,
  // Имя моба от сюжета («Коллектор») вместо имени по расе и редкости.
  customName: Option[String] = None
) {

  // ── Группа ────────────────────────────────────────────────────────────────

  def isGroup: Boolean = group.isGroup

  /** Активный моб (тот, что в паре с героем) как слот — для перекладывания. */
  def activeSlot: MonsterSlot = MonsterSlot(
    lvl = monsterLvl, race = monsterRace, rarity = monsterRarity, stats = monsterStats,
    currentHp = monsterCurrentHp, currentArmor = monsterCurrentArmor, marked = monsterMarked,
    currentEnergy = monsterCurrentEnergy, effects = effects.monsterPart)

  /** Поставить слот в пару: его состояние и эффекты — в поля активного моба,
    * геройская половина эффектов (и разовые флаги героя вроде «Крепкости»)
    * остаётся как была. Минибоссы в группе не бывают, поэтому их поля не трогаем. */
  def withActive(slot: MonsterSlot): SoloPveBattle = copy(
    monsterLvl = slot.lvl, monsterRace = slot.race, monsterRarity = slot.rarity, monsterStats = slot.stats,
    monsterCurrentHp = slot.currentHp, monsterCurrentArmor = slot.currentArmor, monsterMarked = slot.marked,
    monsterCurrentEnergy = slot.currentEnergy, effects = effects.withMonsterPart(slot.effects))

  /** Герой шагает на место `pos`: в пару встаёт моб, стоящий там, прежний
    * активный остаётся на своём месте. Мобы не двигаются. Пустое, чужое или
    * своё место — бой не меняется. */
  def moveHeroTo(pos: Int): SoloPveBattle = {
    val idx = group.idxOf(pos)
    if (idx < 0 || pos == group.heroPos) this
    else withActive(group.others(idx)).copy(group = group.copy(
      others  = group.others.updated(idx, activeSlot),
      places  = group.places.updated(idx, group.heroPos),
      heroPos = pos))
  }

  /** Активный моб пал, а в группе есть ещё: записать его в павшие, его место
    * пустеет, и герой шагает к ближайшему живому (при равном расстоянии —
    * правее). Если шагать не к кому — None, это победа. Отложенный Таран сгорает. */
  def promoteNext: Option[SoloPveBattle] =
    if (group.others.isEmpty) None
    else {
      val idx  = group.places.indices.minBy { i =>
        val d = group.places(i) - group.heroPos
        (math.abs(d), if (d > 0) 0 else 1)
      }
      Some(withActive(group.others(idx)).copy(group = group.copy(
        others      = group.others.patch(idx, Nil, 1),
        places      = group.places.patch(idx, Nil, 1),
        slain       = group.slain :+ slainActive,
        pendingMove = None,
        heroPos     = group.places(idx))))
    }

  /** Моб вне пары `others(idx)` пал — в павшие, строй смыкается. */
  def sideFallen(idx: Int): SoloPveBattle = copy(group = group.withoutSlot(idx))

  /** Активный моб как запись о павшем — для добычи после боя. */
  def slainActive: SlainMonster =
    SlainMonster(monsterLvl, monsterRace, monsterRarity, monsterMarked, monsterName)

  /** Подкрепление встаёт на первое свободное место за строем. */
  def withReinforcement(slot: MonsterSlot): SoloPveBattle =
    copy(group = group.copy(others = group.others :+ slot, places = group.places :+ (group.size + 1)))

  /** Перемешать всех живых мобов по занятым местам: любой может оказаться
    * напротив героя, сам герой с места не сходит, пустые места пустыми и
    * остаются. `order` — новый порядок индексов по списку «активный :: others». */
  def reorderMonsters(order: List[Int]): SoloPveBattle = {
    val all = activeSlot :: group.others
    if (order.sorted != all.indices.toList) this
    else {
      val shuffled = order.map(all)
      withActive(shuffled.head).copy(group = group.copy(others = shuffled.tail))
    }
  }

  /** Все мобы по возрастанию мест (пустые места пропущены); активный — на месте героя. */
  def monstersInOrder: List[MonsterSlot] =
    ((group.heroPos, activeSlot) :: group.entries).sortBy(_._1).map(_._2)

  /** Весь строй по местам, с пустотами: место → кто на нём. */
  def placesInOrder: List[(Int, Option[MonsterSlot])] = {
    val byPos = ((group.heroPos, activeSlot) :: group.entries).toMap
    (1 to group.size).toList.map(pos => pos -> byPos.get(pos))
  }

  /** Моб на месте `pos`, если там кто-то стоит. */
  def monsterAt(pos: Int): Option[MonsterSlot] =
    if (pos == group.heroPos) Some(activeSlot) else group.entries.find(_._1 == pos).map(_._2)

  /** Раса, которой приходит подкрепление: первого моба этого боя. */
  def reinforcementRace: String = group.originRace.getOrElse(monsterRace)

  /** Элементаль этого боя, если сражаемся с минибоссом. */
  def boss: Option[pangea.model.monster.MiniBoss] =
    bossKind.flatMap(pangea.model.monster.MiniBoss.byName)

  /** Обновляет эффекты боя, уважая иммунитеты расы моба: элементалю нельзя
   *  навесить яд или кровотечение — ему нечему течь и нечего травить. Все точки,
   *  накладывающие эффекты на моба, идут через этот метод, а не через `copy`,
   *  чтобы иммунитет нельзя было обойти, забыв про него в новом источнике. */
  def withEffects(e: BattleEffects): SoloPveBattle = {
    val noDots  = if (Race.immuneToDots(Race.withName(monsterRace))) e.copy(monsterPoison = None, monsterBleed = None) else e
    // Огненного элементаля вдобавок нельзя поджечь — он и так пламя.
    val noBurn  = if (boss.exists(_.immuneToBurn)) noDots.copy(monsterBurn = None) else noDots
    copy(effects = noBurn)
  }
  def toMonster: Monster =
    Monster(0L, monsterLvl, Race.withName(monsterRace), Rarity.withName(monsterRarity), monsterStats, monsterMarked)

  /** Имя моба для лога и экрана боя. У минибосса оно именное и приходит от вида
   *  («Огненный Элементаль»); у обычных мобов — из таблицы раса × редкость.
   *  Держим его ЗДЕСЬ, а не в [[Monster]]: тот читается из таблицы `monsters`
   *  целиком (`select *`), и лишнее поле сломало бы чтение. */
  def monsterName: String = customName.orElse(boss.map(_.monsterName)).getOrElse(toMonster.name)

  def rarity: Rarity = Rarity.withName(monsterRarity)

  /** Тик в конце хода игрока. `skipSlots` — itemId слотов, кулдауны которых не
   *  должны декрементиться (например, слот, скил из которого только что
   *  использован — его cd должен начать отсчитываться со СЛЕДУЮЩЕГО хода). */
  def tickBuffs(skipSlots: Set[Long] = Set.empty): SoloPveBattle = copy(
    heroBattleState         = heroBattleState.tick,
    consumableUsedThisRound = false,
    skillSlots              = skillSlots.map(s =>
      if (skipSlots.contains(s.itemId)) s
      else s.copy(cooldown = (s.cooldown - 1).max(0))
    ),
    // Временные стихийные эффекты тикают вместе с бафами: буст Воздуха и %-дебаф
    // защиты цели (комбо Молния+Холод) живут ограниченное число ходов.
    effects = effects.copy(
      airBoostTurns        = (effects.airBoostTurns - 1).max(0),
      // Буст Воздуха на самом мобе тикает вместе с остальным временным.
      mobAirBoostTurns     = (effects.mobAirBoostTurns - 1).max(0),
      monsterSkillBlockedTurns = (effects.monsterSkillBlockedTurns - 1).max(0),
      // Оцепенение элементаля от холода тикает вместе с прочими временными эффектами.
      chilledTurns         = (effects.chilledTurns - 1).max(0),
      // Дебафы каменного элементаля живут теми же ходами.
      heroStunnedTurns     = (effects.heroStunnedTurns - 1).max(0),
      heroGroundedTurns    = (effects.heroGroundedTurns - 1).max(0),
      monsterWeakenedTurns = (effects.monsterWeakenedTurns - 1).max(0),
      monsterDefenceDebuff = effects.monsterDefenceDebuff.flatMap(_.ticked)
    )
  )

  def slotByItem(itemId: Long): Option[SkillSlotState] = skillSlots.find(_.itemId == itemId)

  def updateSlot(itemId: Long)(f: SkillSlotState => SkillSlotState): SoloPveBattle =
    copy(skillSlots = skillSlots.map(s => if (s.itemId == itemId) f(s) else s))

  /** Стартовый запас энергии обычного моба: `pctRoll`% (5–30) от его потолка,
   *  умноженные на редкость. Копить с нуля мобу некогда — короткий бой кончится
   *  раньше, чем он покажет хоть одно умение. Минибоссов это не касается: они
   *  входят в бой с полным запасом. */
  def withStartEnergy(pctRoll: Long): SoloPveBattle =
    if (bossKind.isDefined) this
    else copy(monsterCurrentEnergy = MonsterEnergy.startEnergy(monsterLvl, rarity, pctRoll))
}

object SoloPveBattle {
  /** Сборка боя из моба и героя: статы/hp/броня берутся с моба, слоты активных
   *  навыков отдаёт сам герой (`hero.activeSkillSlots`). Единая точка входа в
   *  бой для всех событий. */
  def from(monster: Monster, hero: Hero): SoloPveBattle = SoloPveBattle(
    monsterLvl          = monster.lvl,
    monsterRace         = monster.race.entryName,
    monsterRarity       = monster.rarity.entryName,
    monsterStats        = monster.fightStats,
    monsterCurrentHp    = monster.fightStats.hp,
    monsterCurrentArmor = monster.fightStats.armor,
    monsterMarked       = monster.marked,
    skillSlots          = hero.activeSkillSlots,
    monsterCurrentEnergy = monster.fightStats.energy
  )

  /** Бой против группы: первый моб в паре, остальные — слотами под номерами 2+.
    * Раса первого запоминается — подкрепление приходит такой же. */
  def fromGroup(monsters: List[Monster], hero: Hero, startEnergies: List[Long]): SoloPveBattle = {
    val energies = startEnergies.padTo(monsters.size, 0L)
    val slots = monsters.zip(energies).map { case (m, e) =>
      MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
        m.fightStats.armor, m.marked, e, BattleEffects.empty)
    }
    val head = from(monsters.head, hero).copy(monsterCurrentEnergy = energies.head)
    head.copy(group = GroupState(others = slots.tail, originRace = Some(monsters.head.race.entryName),
      places = (2 to monsters.size).toList))
  }

  implicit val encoder: Encoder[SoloPveBattle] = deriveEncoder

  implicit val decoder: Decoder[SoloPveBattle] = (c: HCursor) =>
    for {
      monsterLvl          <- c.get[Long]("monsterLvl")
      monsterRace         <- c.get[String]("monsterRace")
      monsterRarity       <- c.get[String]("monsterRarity")
      monsterStats        <- c.get[FightStats]("monsterStats")
      monsterCurrentHp    <- c.get[Long]("monsterCurrentHp")
      monsterCurrentArmor <- c.get[Long]("monsterCurrentArmor")
      heroBattleState     <- c.getOrElse[HeroBattleState]("heroBattleState")(HeroBattleState.empty)
      consumableUsed      <- c.getOrElse[Boolean]("consumableUsedThisRound")(false)
      monsterMarked       <- c.getOrElse[Boolean]("monsterMarked")(false)
      skillSlots          <- c.getOrElse[List[SkillSlotState]]("skillSlots")(Nil)
      effects             <- c.getOrElse[BattleEffects]("effects")(BattleEffects.empty)
      toughnessUsed       <- c.getOrElse[Boolean]("toughnessUsed")(false)
      bossKind       <- c.getOrElse[Option[String]]("bossKind")(None)
      bossTurn       <- c.getOrElse[Int]("bossTurn")(0)
      charges             <- c.getOrElse[Int]("bossCharges")(0)
      revives             <- c.getOrElse[Int]("bossRevives")(0)
      monsterEnergy       <- c.getOrElse[Long]("monsterCurrentEnergy")(0L)
      group               <- c.getOrElse[GroupState]("group")(GroupState.empty)
      story               <- c.getOrElse[Option[String]]("story")(None)
      customName          <- c.getOrElse[Option[String]]("customName")(None)
    } yield SoloPveBattle(monsterLvl, monsterRace, monsterRarity, monsterStats,
                         monsterCurrentHp, monsterCurrentArmor, heroBattleState, consumableUsed, monsterMarked,
                         skillSlots, effects, toughnessUsed, bossKind, bossTurn, charges, revives, monsterEnergy, group,
                         story, customName)
}
