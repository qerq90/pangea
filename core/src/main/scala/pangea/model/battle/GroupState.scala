package pangea.model.battle

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind}
import pangea.model.stats.FightStats

/** Союзник в бою: позиция в строю (напротив места врага с тем же номером),
  * текущее состояние и потолки на этот бой. Эффектов на союзниках нет. */
final case class BattleAlly(kind: AllyKind, position: Int, hp: Long, armor: Long, energy: Long, stats: FightStats, lvl: Long) {
  def name: String  = kind.name
  def alive: Boolean = hp > 0L
  def hpPct: Long    = if (stats.hp <= 0L) 0L else hp * 100L / stats.hp
  def armorPct: Long = if (stats.armor <= 0L) 0L else armor * 100L / stats.armor

  /** Обратно в отряд — с тем, что осталось. */
  def toAlly: Ally = Ally(kind, position, hp, armor, energy)
}

object BattleAlly {
  def of(a: Ally, lvl: Long): BattleAlly = {
    val c = a.clamped(lvl)
    BattleAlly(c.kind, c.position, c.hp, c.armor, c.energy, a.kind.stats(lvl), lvl)
  }

  implicit val encoder: Encoder[BattleAlly] = deriveEncoder
  implicit val decoder: Decoder[BattleAlly] = (c: HCursor) =>
    for {
      kind     <- c.get[AllyKind]("kind")
      position <- c.get[Int]("position")
      hp       <- c.get[Long]("hp")
      armor    <- c.get[Long]("armor")
      energy   <- c.get[Long]("energy")
      stats    <- c.get[FightStats]("stats")
      lvl      <- c.getOrElse[Long]("lvl")(1L)
    } yield BattleAlly(kind, position, hp, armor, energy, stats, lvl)
}

/** Моб группы, стоящий НЕ в паре с героем: всё, что описывает его и его текущее
  * состояние, включая эффекты на нём (яд, кровь, огонь, порошок, дебафы). Когда
  * такой моб встаёт в пару, слот разворачивается в поля [[SoloPveBattle]], а
  * прежний активный сворачивается в слот — см. `SoloPveBattle.swapWith`. */
final case class MonsterSlot(
  lvl:           Long,
  race:          String,
  rarity:        String,
  stats:         FightStats,
  currentHp:     Long,
  currentArmor:  Long,
  marked:        Boolean,
  currentEnergy: Long,
  effects:       BattleEffects
) {
  def toMonster: Monster =
    Monster(0L, lvl, Race.withName(race), Rarity.withName(rarity), stats, marked)

  def name: String = toMonster.name

  /** Имя для кнопки — см. `Monster.shortName`. */
  def shortName: String = toMonster.shortName

  def alive: Boolean = currentHp > 0L

  /** Слот как запись о павшем — для добычи после боя. */
  def slain: SlainMonster = SlainMonster(lvl, race, rarity, marked, name)

  /** Проценты для строки группового экрана. */
  def hpPct: Long    = if (stats.hp <= 0L) 0L else currentHp * 100L / stats.hp
  def armorPct: Long = if (stats.armor <= 0L) 0L else currentArmor * 100L / stats.armor
}

object MonsterSlot {
  implicit val encoder: Encoder[MonsterSlot] = deriveEncoder

  implicit val decoder: Decoder[MonsterSlot] = (c: HCursor) =>
    for {
      lvl           <- c.get[Long]("lvl")
      race          <- c.get[String]("race")
      rarity        <- c.get[String]("rarity")
      stats         <- c.get[FightStats]("stats")
      currentHp     <- c.get[Long]("currentHp")
      currentArmor  <- c.get[Long]("currentArmor")
      marked        <- c.getOrElse[Boolean]("marked")(false)
      currentEnergy <- c.getOrElse[Long]("currentEnergy")(0L)
      effects       <- c.getOrElse[BattleEffects]("effects")(BattleEffects.empty)
    } yield MonsterSlot(lvl, race, rarity, stats, currentHp, currentArmor, marked, currentEnergy, effects)
}

/** Убитый моб — ровно то, что нужно, чтобы после боя накатать за него добычу. */
final case class SlainMonster(lvl: Long, race: String, rarity: String, marked: Boolean, name: String)

object SlainMonster {
  implicit val encoder: Encoder[SlainMonster] = deriveEncoder
  implicit val decoder: Decoder[SlainMonster] = (c: HCursor) =>
    for {
      lvl    <- c.get[Long]("lvl")
      race   <- c.get[String]("race")
      rarity <- c.get[String]("rarity")
      marked <- c.getOrElse[Boolean]("marked")(false)
      name   <- c.getOrElse[String]("name")("")
    } yield SlainMonster(lvl, race, rarity, marked, name)
}

/** Групповая часть боя. Мобы стоят в строю по местам 1, 2, …; герой стоит на
  * месте `heroPos`. Активный моб — тот, что живёт в полях [[SoloPveBattle]], —
  * стоит на месте `activePos`. Совпало с местом героя — они в паре: герой бьёт
  * его по умолчанию, он отвечает герою. Не совпало (у героя напротив пусто:
  * своего он убил, а свободных мобов нет, или обычная встреча началась не на
  * его месте) — активный просто хранится в полях и ходит как моб вне пары.
  * Павший освобождает место, и оно остаётся пустым — строй не смыкается
  * (пустоты — задел под тактику). Здесь — всё остальное:
  *
  *  - `others`  — мобы на прочих местах (без активного), `places` — их места,
  *    список в список;
  *  - `heroPos` — место героя, с единицы; `activePos` — место активного моба;
  *  - `lastTarget` — по кому герой бил последним: когда напротив пусто, экран
  *    боя считает его шансы против этой цели;
  *  - `heroDown` — герой обнулён, но отряд ещё на ногах: бой идёт без него,
  *    раунд за раундом по таймеру, а его смерть отложена до исхода;
  *  - `queue` — кому не хватило места в строю (больше [[GroupState.MaxMonsters]]):
  *    ждут за спинами и входят, как только место освободится;
  *  - `slain`   — павшие, в порядке гибели, для выдачи добычи после победы;
  *  - `round`   — сколько раундов прошло (каждый четвёртый — перемешивание);
  *  - `pendingMove` — Таран: место, на которое герой шагнёт в конце раунда;
  *  - `originRace` — раса первого моба: подкрепление приходит той же расы;
  *  - `allies`  — союзники героя по своим позициям (см. [[BattleAlly]]);
  *  - `alliesGone` — кто ушёл по свитку за этот бой (виды): после боя они
  *    выбывают из отряда на сутки.
  *
  * Обычный бой 1 на 1 — это группа из одного: `others` пуст. */
final case class GroupState(
  others:      List[MonsterSlot]  = Nil,
  slain:       List[SlainMonster] = Nil,
  round:       Int                = 0,
  pendingMove: Option[Int]        = None,
  originRace:  Option[String]     = None,
  heroPos:     Int                = 1,
  places:      List[Int]          = Nil,
  allies:      List[BattleAlly]   = Nil,
  alliesGone:  List[String]       = Nil,
  activePos:   Int                = 1,
  lastTarget:  Option[MonsterSlot] = None,
  heroDown:    Boolean            = false,
  queue:       List[MonsterSlot]  = Nil
) {
  def isGroup: Boolean = others.nonEmpty

  /** Активный моб стоит напротив героя. */
  def paired: Boolean = activePos == heroPos

  /** Есть ли строй, который стоит показать: мобы вне пары, союзники или
    * пустота напротив героя. */
  def hasFormation: Boolean = isGroup || allies.nonEmpty || !paired

  /** Стоит ли на месте `pos` какой-нибудь моб — активный или вне пары. */
  def hasMonster(pos: Int): Boolean = pos == activePos || occupied(pos)

  /** Моб на месте `pos` никем из союзников не занят. */
  def freeAt(pos: Int): Boolean = allyAt(pos).forall(!_.alive)

  /** По кому герой может ударить: моб в паре и соседи, слева направо. */
  def attackTargets: List[Int] =
    ((if (paired) List(heroPos) else Nil) ++ neighbourPositions).sorted

  /** Союзник на позиции `pos`. */
  def allyAt(pos: Int): Option[BattleAlly] = allies.find(_.position == pos)

  /** Строй для показа — до самого дальнего занятого места по обеим сторонам. */
  def rows: Int = (size :: allies.map(_.position)).max

  def updateAlly(kind: AllyKind)(f: BattleAlly => BattleAlly): GroupState =
    copy(allies = allies.map(a => if (a.kind == kind) f(a) else a))

  /** Союзник ушёл по свитку. */
  def withoutAlly(kind: AllyKind): GroupState =
    copy(allies = allies.filterNot(_.kind == kind), alliesGone = alliesGone :+ kind.entryName)

  /** Сколько мобов ещё на ногах, включая активного. */
  def aliveCount: Int = 1 + others.count(_.alive)

  /** Сколько мест в строю — до самого дальнего занятого (пустые между — тоже места). */
  def size: Int = (heroPos :: activePos :: places).max

  /** Место моба `others(idx)`. */
  def posOf(idx: Int): Int = places(idx)

  /** Индекс в `others` моба на месте `pos`; −1, если там пусто или герой. */
  def idxOf(pos: Int): Int = places.indexOf(pos)

  /** Место есть в строю (пусть и пустое). */
  def hasPos(pos: Int): Boolean = pos >= 1 && pos <= size

  /** Стоит ли на месте `pos` моб вне пары. */
  def occupied(pos: Int): Boolean = idxOf(pos) >= 0

  /** Достаёт ли герой до моба на месте `pos`: соседнее место, и там кто-то
    * есть — моб вне пары или активный, стоящий не напротив героя. */
  def inReach(pos: Int): Boolean = pos != heroPos && math.abs(pos - heroPos) <= GroupState.Reach && hasMonster(pos)

  /** Занятые мобами места по соседству с героем, слева направо. */
  def neighbourPositions: List[Int] = List(heroPos - 1, heroPos + 1).filter(hasMonster)

  /** Мобы с их местами, список в список. */
  def entries: List[(Int, MonsterSlot)] = places.zip(others)

  /** Моб `others(idx)` пал: из строя — в павшие, его место пустеет. Таран в него
    * сгорает. Чужой индекс — ничего. */
  def withoutSlot(idx: Int): GroupState =
    others.lift(idx) match {
      case None       => this
      case Some(slot) =>
        val pos = posOf(idx)
        copy(others = others.patch(idx, Nil, 1), places = places.patch(idx, Nil, 1),
             slain = slain :+ slot.slain, pendingMove = pendingMove.filter(_ != pos))
    }
}

object GroupState {
  val empty: GroupState = GroupState()

  /** Сколько мест в строю: столько мобов может стоять против героя разом. */
  val MaxMonsters: Int = 10

  /** Каждый такой раунд пары рвутся и собираются заново. */
  val ShufflePeriod: Int = 4

  /** Радиус: с места n достают до мест n−1, n, n+1. Герой бьёт соседей своего
    * места, и только они бьют его сбоку. */
  val Reach: Int = 1

  /** Шанс (в %), что в начале раунда к мобу прибежит сородич. */
  val ReinforcementChancePct: Long = 2L

  /** Призыв в конце первого раунда: легендарный зовёт 2–3 сородичей 3–4 ранга
    * (редкий/мифический), мифический — 1–3 сородичей 1–3 ранга. */
  val LegendarySummonMin: Int = 2
  val LegendarySummonMax: Int = 3
  val MythicalSummonMin: Int  = 1
  val MythicalSummonMax: Int  = 3

  /** Каждый свободный моб добавляет столько процентов к шансу, что он не даст сбежать. */
  val SurroundPctPerFreeMob: Long = 5L

  implicit val encoder: Encoder[GroupState] = deriveEncoder
  implicit val decoder: Decoder[GroupState] = (c: HCursor) =>
    for {
      others      <- c.getOrElse[List[MonsterSlot]]("others")(Nil)
      slain       <- c.getOrElse[List[SlainMonster]]("slain")(Nil)
      round       <- c.getOrElse[Int]("round")(0)
      pendingMove <- c.getOrElse[Option[Int]]("pendingMove")(None)
      originRace  <- c.getOrElse[Option[String]]("originRace")(None)
      heroPos     <- c.getOrElse[Int]("heroPos")(1)
      // Без мест (старая запись) — строй сплошной: слева от героя, потом справа.
      places      <- c.getOrElse[List[Int]]("places")(others.indices.map(i => if (i < heroPos - 1) i + 1 else i + 2).toList)
      allies      <- c.getOrElse[List[BattleAlly]]("allies")(Nil)
      gone        <- c.getOrElse[List[String]]("alliesGone")(Nil)
      activePos   <- c.getOrElse[Int]("activePos")(heroPos)
      lastTarget  <- c.getOrElse[Option[MonsterSlot]]("lastTarget")(None)
      heroDown    <- c.getOrElse[Boolean]("heroDown")(false)
      queue       <- c.getOrElse[List[MonsterSlot]]("queue")(Nil)
    } yield GroupState(others, slain, round, pendingMove, originRace, heroPos, places, allies, gone, activePos, lastTarget, heroDown, queue)
}
