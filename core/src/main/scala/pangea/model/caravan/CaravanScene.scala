package pangea.model.caravan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.domain.Rng
import pangea.model.item.{Item, Rarity}
import pangea.model.monster.Rarity.{Legendary, Mythical, Rare, Uncommon}
import pangea.model.monster.{Race, Rarity => MobRarity}

/** Караван, на который вышел герой: кто его ведёт, сколько охраны и башен, что
  * везут и что герой успел с этим сделать.
  *
  * Живёт в `scene_data`, поэтому декодер рукописный: уход в «Персонаж» за
  * снаряжением и возврат должны показать тот же караван, а не новый.
  *
  * @param race     раса каравана — она же у всей охраны
  * @param guards   сколько охранников осталось (отпугнутые уже вычтены)
  * @param towers   сколько при караване башен со стрелками
  * @param stage    какая сцена сейчас: [[CaravanRates.StageSpotted]] и дальше
  * @param weakened охрана одурманена: дым или сонный дурман
  * @param smoke    разбита именно дымная фляга — без неё в караван не прокрасться
  * @param scared   сколько раз охрану проредил страх (божественное оружие, «Ужасающий»)
  * @param goods    что караван везёт: три вещи, они же добыча
  * @param prices   почём он их отдаёт
  * @param page     страница списка вещей героя
  * @param wave     какая волна боя идёт, когда караван разделён дурманом
  * @param spoils   охрана перебита, поклажа ещё не разобрана
  */
final case class CaravanScene(
  race:     String,
  guards:   Int,
  towers:   Int,
  stage:    Int         = CaravanRates.StageSpotted,
  weakened: Boolean     = false,
  smoke:    Boolean     = false,
  scared:   Int         = 0,
  goods:    List[Item]  = Nil,
  prices:   List[Long]  = Nil,
  page:     Int         = 0,
  wave:     Int         = 0,
  spoils:   Boolean     = false
) {

  /** Сколько охраны выйдет драться: каждый испуг уводит свою долю. */
  def fighting: Int = {
    val left = guards - guards * CaravanRates.ScarePct.toInt * scared / 100
    left.max(CaravanRates.MinFighting)
  }

  /** Одурманенный караван дерётся двумя волнами, и башни в этом не участвуют. */
  def waves: Int = if (weakened) 2 else 1

  def towersInFight: Int = if (weakened) 0 else towers

  /** Сколько охраны в этой волне (нумерация с единицы). */
  def waveSize(n: Int): Int =
    if (!weakened) fighting
    else if (n <= 1) (fighting + 1) / 2
    else fighting / 2
}

object CaravanScene {
  implicit val encoder: Encoder[CaravanScene] = (s: CaravanScene) =>
    Json.obj(
      "race" -> s.race.asJson, "guards" -> s.guards.asJson, "towers" -> s.towers.asJson,
      "stage" -> s.stage.asJson, "weakened" -> s.weakened.asJson, "smoke" -> s.smoke.asJson,
      "scared" -> s.scared.asJson, "goods" -> s.goods.asJson, "prices" -> s.prices.asJson,
      "page" -> s.page.asJson, "wave" -> s.wave.asJson, "spoils" -> s.spoils.asJson)

  implicit val decoder: Decoder[CaravanScene] = (c: HCursor) =>
    for {
      race     <- c.get[String]("race")
      guards   <- c.get[Int]("guards")
      towers   <- c.getOrElse[Int]("towers")(0)
      stage    <- c.getOrElse[Int]("stage")(CaravanRates.StageSpotted)
      weakened <- c.getOrElse[Boolean]("weakened")(false)
      smoke    <- c.getOrElse[Boolean]("smoke")(false)
      scared   <- c.getOrElse[Int]("scared")(0)
      goods    <- c.getOrElse[List[Item]]("goods")(Nil)
      prices   <- c.getOrElse[List[Long]]("prices")(Nil)
      page     <- c.getOrElse[Int]("page")(0)
      wave     <- c.getOrElse[Int]("wave")(0)
      spoils   <- c.getOrElse[Boolean]("spoils")(false)
    } yield CaravanScene(race, guards, towers, stage, weakened, smoke, scared, goods, prices, page, wave, spoils)
}

/** Числа каравана. Вынесены отдельно: их читают и генератор, и сцена, и тесты. */
object CaravanRates {

  /** Сцены: заметил издали → подошёл вплотную → выждал момент. */
  val StageSpotted: Int = 1
  val StageClose:   Int = 2
  val StageMoment:  Int = 3

  /** Сколько существ в охране. */
  val MinGuards: Int = 10
  val MaxGuards: Int = 20

  /** Сколько башен при караване. */
  val MinTowers: Int = 1
  val MaxTowers: Int = 2

  /** Башни стоят поодаль от свалки и не двигаются: им отдан хвост строя —
    * последние места единой схемы (см. [[pangea.model.battle.Formation]]). До
    * стрелков ещё надо добежать, а они бьют с любого расстояния. */
  val TowerPlace: Int = pangea.model.battle.Formation.MonsterPlaces - MaxTowers + 1

  /** На сколько процентов испуг уводит охрану и сколько её остаётся всегда. */
  val ScarePct: Long   = 25L
  val MinFighting: Int = 1

  /** Насколько дурман срезает атаку и энергию охраны. */
  val DopeCutPct: Long = 20L

  /** Шанс (в %) прокрасться в караван и шанс уйти после кражи. */
  val SneakPct: Int = 50
  val SlipPct: Int  = 50

  /** Сколько вещей караван везёт и почём: множители у него свои, щедрее
    * лавочных, — за легендарную вещь торговец в лабиринте просит всерьёз. */
  val Goods: Int = 3

  def priceFactor(rarity: Rarity): Double = rarity match {
    case Rarity.Blue   => 6.0
    case Rarity.Purple => 8.0
    case Rarity.Violet => 12.0
    case Rarity.Orange => 25.0
    case _             => 4.0
  }

  /** Редкости товара: синяя 35 · фиолетовая 32 · пурпурная 32 · легендарная 1. */
  val GoodsPool: List[Rarity] =
    List.fill(35)(Rarity.Blue) ++ List.fill(32)(Rarity.Purple) ++
      List.fill(32)(Rarity.Violet) ++ List.fill(1)(Rarity.Orange)

  /** Редкости охраны: второй тир 50 · третий 40 · четвёртый 9 · именной 1. */
  val GuardPool: List[MobRarity] =
    List.fill(50)(Uncommon) ++ List.fill(40)(Rare) ++ List.fill(9)(Mythical) ++ List.fill(1)(Legendary)
}

/** Чистая сборка каравана: раса, охрана, башни. Товары докатываются отдельно —
  * им нужен уровень героя, а не этажа. */
object CaravanGenerator {

  /** Бросок в [0, bound) по старшим битам — младшие у [[Rng]] ходят коротким
    * циклом (см. `CaveGenerator`). */
  private def roll(rng: Rng, bound: Int): (Int, Rng) = {
    val (l, next) = rng.nextLong
    (((l >>> 17) % bound.toLong).toInt, next)
  }

  def generate(rng: Rng): (CaravanScene, Rng) = {
    val (raceIdx, r1) = roll(rng, Race.mortals.size)
    val (extra, r2)   = roll(r1, CaravanRates.MaxGuards - CaravanRates.MinGuards + 1)
    val (towers, r3)  = roll(r2, CaravanRates.MaxTowers - CaravanRates.MinTowers + 1)
    (CaravanScene(
      race   = Race.mortals(raceIdx).entryName,
      guards = CaravanRates.MinGuards + extra,
      towers = CaravanRates.MinTowers + towers), r3)
  }

  /** Редкость очередного охранника. */
  def guardRarity(rng: Rng): (MobRarity, Rng) = {
    val (i, next) = roll(rng, CaravanRates.GuardPool.size)
    (CaravanRates.GuardPool(i), next)
  }

  /** Редкость товара. */
  def goodsRarity(rng: Rng): (Rarity, Rng) = {
    val (i, next) = roll(rng, CaravanRates.GoodsPool.size)
    (CaravanRates.GoodsPool(i), next)
  }

  /** Цена вещи у каравана: как у лавочника, но по своим множителям и с тем же
    * разбросом в пятую часть. */
  def price(heroLvl: Long, rarity: Rarity, rng: Rng): (Long, Rng) = {
    val base      = (heroLvl + 20) * 4 * CaravanRates.priceFactor(rarity)
    val (pct, nx) = rng.between(-20L, 21L)
    ((base + base * pct / 100.0).toLong.max(1L), nx)
  }
}
