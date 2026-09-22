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
  // Применил ли минибосс своё первое бьющее умение: у Белого волка оно бьёт
  // вдвое, дальше двойной урон — только по шансу.
  bossFirstSkillSpent: Boolean = false,
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
  customName: Option[String] = None,
  // Реликвия доп. слота бьёт раз в раунд — и своего счётчика ни с кем не делит.
  relicUsedThisRound: Boolean = false
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

  /** Пара пуста: активный моб стоит не напротив героя. */
  def unpaired: Boolean = !group.paired

  /** Развернуть в поля моба с места `pos` (для удара по нему): прежний активный
    * сворачивается в слот на своё место. Мобы не двигаются, герой тоже — это
    * подмена «кто в полях», и повторный `engage` на прежнее место всё
    * возвращает. Пустое или уже активное место — бой не меняется. */
  def engage(pos: Int): SoloPveBattle = {
    val idx = group.idxOf(pos)
    if (idx < 0 || pos == group.activePos) this
    else withActive(group.others(idx)).copy(group = group.copy(
      others    = group.others.updated(idx, activeSlot),
      places    = group.places.updated(idx, group.activePos),
      activePos = pos))
  }

  /** Герой шагает на место `pos` (Таран): моб оттуда встаёт с ним в пару,
    * прежний активный остаётся на своём месте. Пустое, чужое или своё место —
    * бой не меняется. */
  def moveHeroTo(pos: Int): SoloPveBattle =
    if (pos == group.heroPos || !group.hasMonster(pos)) this
    else {
      val e = engage(pos)
      e.copy(group = e.group.copy(heroPos = pos))
    }

  /** Запомнить, по кому герой бил: когда напротив пусто, экран боя считает
    * его шансы против этой цели. */
  def rememberTarget(pos: Int): SoloPveBattle =
    copy(group = group.copy(lastTarget = monsterAt(pos).orElse(group.lastTarget)))

  /** Ближайший к герою из индексов `others` (при равном расстоянии — правее). */
  private def nearestOf(idxs: List[Int]): Int =
    idxs.minBy { i =>
      val d = group.places(i) - group.heroPos
      (math.abs(d), if (d > 0) 0 else 1)
    }

  /** Активный моб пал, а в группе есть ещё: записать его в павшие. К герою
    * шагает ближайший свободный — тот, напротив которого нет союзника (при
    * равном расстоянии — правее); его прежнее место пустеет. Свободных нет —
    * никто не шагает: в полях остаётся ближайший занятый, на своём месте, и
    * напротив героя пусто. Герой с места не сходит — у отряда позиции свои.
    * Если мобов больше нет — None, это победа. Отложенный Таран сгорает. */
  def promoteNext: Option[SoloPveBattle] =
    if (group.others.isEmpty && group.queue.isEmpty) None
    else if (group.others.isEmpty) admitQueued._1.promoteNext   // строй пуст, но за ним ждут — входят и шагают
    else {
      val all  = group.places.indices.toList
      val free = all.filter(i => group.freeAt(group.places(i)))
      val idx  = if (free.nonEmpty) nearestOf(free) else nearestOf(all)
      val pos  = if (free.nonEmpty) group.heroPos else group.places(idx)
      Some(withActive(group.others(idx)).copy(group = group.copy(
        others      = group.others.patch(idx, Nil, 1),
        places      = group.places.patch(idx, Nil, 1),
        slain       = group.slain :+ slainActive,
        activePos   = pos,
        pendingMove = None)))
    }

  /** Напротив героя пусто, а свободный моб есть — он шагает к герою: сам
    * активный, если его никто не держит, иначе ближайший свободный из строя
    * (активный возвращается на своё место). Пара цела или шагать некому — None. */
  def pullFree: Option[SoloPveBattle] =
    if (group.paired) None
    else if (group.freeAt(group.activePos)) Some(copy(group = group.copy(activePos = group.heroPos)))
    else {
      val free = group.places.indices.toList.filter(i => group.freeAt(group.places(i)))
      if (free.isEmpty) None
      else {
        val idx = nearestOf(free)
        Some(withActive(group.others(idx)).copy(group = group.copy(
          others    = group.others.updated(idx, activeSlot),
          places    = group.places.updated(idx, group.activePos),
          activePos = group.heroPos)))
      }
    }

  /** Собрать пару в начале боя: на месте героя стоит моб — он и активный;
    * иначе к герою шагает свободный, если есть. */
  def settle: SoloPveBattle =
    if (group.paired) this
    else if (group.occupied(group.heroPos)) engage(group.heroPos)
    else pullFree.getOrElse(this)

  /** Моб вне пары `others(idx)` пал — в павшие, строй смыкается. */
  def sideFallen(idx: Int): SoloPveBattle = copy(group = group.withoutSlot(idx))

  /** Активный моб как запись о павшем — для добычи после боя. */
  def slainActive: SlainMonster =
    SlainMonster(monsterLvl, monsterRace, monsterRarity, monsterMarked, monsterName)

  /** Подкрепление встаёт на первое свободное место за строем. */
  def withReinforcement(slot: MonsterSlot): SoloPveBattle =
    copy(group = group.copy(others = group.others :+ slot, places = group.places :+ (group.size + 1)))

  /** Есть ли место в строю ещё для одного. */
  def hasRoom: Boolean = group.aliveCount < GroupState.MaxMonsters

  /** Пришедший встаёт в строй, если есть место, иначе — в очередь за ним. */
  def admit(slot: MonsterSlot): SoloPveBattle =
    if (hasRoom) withReinforcement(slot) else copy(group = group.copy(queue = group.queue :+ slot))

  /** Конец раунда: мобы вне досягаемости героя подтягиваются к нему на одно
    * место, если оно свободно, — ближние первыми, чтобы следом двинулись и
    * дальние. Не двигаются: те, кто уже достаёт героя, кто занят союзником
    * напротив, и моб в полях (за него — `pullFree`). На место героя никто не
    * шагает: к пустой паре ведёт `pullFree`. В дыму герой не виден — все стоят.
    * Возвращает бой и кто куда шагнул. */
  def closeIn: (SoloPveBattle, List[(MonsterSlot, Int)]) =
    if (effects.heroInSmoke || group.heroDown) (this, Nil)
    else {
      val order = group.places.indices.toList.sortBy(i => math.abs(group.places(i) - group.heroPos))
      order.foldLeft((this, List.empty[(MonsterSlot, Int)])) { case ((b, moved), idx) =>
        val pos  = b.group.places(idx)
        val dist = pos - b.group.heroPos
        val next = if (dist > 0) pos - 1 else pos + 1
        val far  = math.abs(dist) > GroupState.Reach
        val free = next >= 1 && next != b.group.heroPos && !b.group.hasMonster(next)
        val busy = b.group.allyAt(pos).exists(_.alive)
        if (!far || !free || busy) (b, moved)
        else (b.copy(group = b.group.copy(places = b.group.places.updated(idx, next))), moved :+ (b.group.others(idx) -> next))
      }
    }

  /** Из очереди в строй — сколько влезет. Возвращает бой и вошедших. */
  def admitQueued: (SoloPveBattle, List[MonsterSlot]) =
    group.queue.foldLeft((copy(group = group.copy(queue = Nil)), List.empty[MonsterSlot])) { case ((b, in), s) =>
      if (b.hasRoom) (b.withReinforcement(s), in :+ s) else (b.copy(group = b.group.copy(queue = b.group.queue :+ s)), in)
    }

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

  /** Все мобы по возрастанию мест (пустые места пропущены); активный — на своём месте. */
  def monstersInOrder: List[MonsterSlot] =
    ((group.activePos, activeSlot) :: group.entries).sortBy(_._1).map(_._2)

  /** Весь строй по местам, с пустотами: место → кто на нём. */
  def placesInOrder: List[(Int, Option[MonsterSlot])] = {
    val byPos = ((group.activePos, activeSlot) :: group.entries).toMap
    (1 to group.size).toList.map(pos => pos -> byPos.get(pos))
  }

  /** Моб на месте `pos`, если там кто-то стоит. */
  def monsterAt(pos: Int): Option[MonsterSlot] =
    if (pos == group.activePos) Some(activeSlot) else group.entries.find(_._1 == pos).map(_._2)

  /** Моб напротив героя, если пара не пуста. */
  def pairedMonster: Option[MonsterSlot] = if (group.paired) Some(activeSlot) else None

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
    // Реликвия доп. слота живёт своим счётчиком: «быстрые руки» её не ускоряют,
    // а расходник в руках её не блокирует.
    relicUsedThisRound      = false,
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
      // Инстинкт волка живёт своими четырьмя ходами.
      mobInstinctTurns     = (effects.mobInstinctTurns - 1).max(0),
      // Смазка на оружии героя и дым сходят за свои раунды.
      heroPoisonCoatTurns  = (effects.heroPoisonCoatTurns - 1).max(0),
      heroBleedCoatTurns   = (effects.heroBleedCoatTurns - 1).max(0),
      heroSmokeTurns       = (effects.heroSmokeTurns - 1).max(0),
      heroTrueStrikeTurns  = (effects.heroTrueStrikeTurns - 1).max(0),
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
   *  бой для всех событий. `squad = false` — сюжетный бой, в который отряд не
   *  берут: герой один, на месте 1. */
  def from(monster: Monster, hero: Hero, squad: Boolean = true): SoloPveBattle = SoloPveBattle(
    monsterLvl          = monster.lvl,
    monsterRace         = monster.race.entryName,
    monsterRarity       = monster.rarity.entryName,
    monsterStats        = monster.fightStats,
    monsterCurrentHp    = monster.fightStats.hp,
    monsterCurrentArmor = monster.fightStats.armor,
    monsterMarked       = monster.marked,
    skillSlots          = hero.activeSkillSlots,
    monsterCurrentEnergy = monster.fightStats.energy,
    // Отряд встаёт по своим позициям без пустот (герой один — на месте 1); моб
    // обычной встречи всегда появляется на месте 1, где бы ни стоял герой.
    group =
      if (squad) {
        val formation = hero.squad.compact
        GroupState(heroPos = formation.heroPos, activePos = 1,
          allies = formation.inOrder.map(BattleAlly.of(_, hero.lvl)))
      }
      else GroupState()
  )

  /** Бой против группы: мобы встают по местам 1, 2, … подряд; в паре с героем
    * — тот, что на его месте, а если там пусто — свободный (см. `settle`).
    * Раса первого запоминается — подкрепление приходит такой же. */
  def fromGroup(monsters: List[Monster], hero: Hero, startEnergies: List[Long], squad: Boolean = true): SoloPveBattle = {
    val energies = startEnergies.padTo(monsters.size, 0L)
    val slots = monsters.zip(energies).map { case (m, e) =>
      MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
        m.fightStats.armor, m.marked, e, BattleEffects.empty)
    }
    val head = from(monsters.head, hero, squad).copy(monsterCurrentEnergy = energies.head)
    head.copy(group = head.group.copy(others = slots.tail, originRace = Some(monsters.head.race.entryName),
      places = (2 to monsters.size).toList)).settle
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
      firstSkill          <- c.getOrElse[Boolean]("bossFirstSkillSpent")(false)
      monsterEnergy       <- c.getOrElse[Long]("monsterCurrentEnergy")(0L)
      group               <- c.getOrElse[GroupState]("group")(GroupState.empty)
      story               <- c.getOrElse[Option[String]]("story")(None)
      customName          <- c.getOrElse[Option[String]]("customName")(None)
      relicUsed           <- c.getOrElse[Boolean]("relicUsedThisRound")(false)
    } yield SoloPveBattle(monsterLvl, monsterRace, monsterRarity, monsterStats,
                         monsterCurrentHp, monsterCurrentArmor, heroBattleState, consumableUsed, monsterMarked,
                         skillSlots, effects, toughnessUsed, bossKind, bossTurn, charges, revives, firstSkill, monsterEnergy, group,
                         story, customName, relicUsed)
}
