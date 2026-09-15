package pangea.model.hero

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}

/** Что герой уже знает о мире — и что помнит о себе. Durable, лежит в
 *  `heroes.lore_data`.
 *
 *  `metElemental` — встречал ли элементаля хоть раз: по первой встрече
 *  показывается особая реплика, и после неё у трактирщика появляется кнопка
 *  «Что ты знаешь о элементалях?».
 *  `elementalLore` — куплена ли у трактирщика легенда об элементалях (тогда
 *  кнопка больше не предлагает платить).
 *  `metJoe`/`joeLore` — то же самое про Гнилого Джо.
 *  `prologueBranch` — какой смертью герой прошёл пролог («Gnome», «Orc»,
 *  «Elf», «Human»); пишется в финале, когда раса подтверждена, — сюжет потом
 *  сможет на это опереться.
 *  `knowledge` — что герой умеет ([[Knowledge]], ключи); `selfTaught` — из них
 *  те, до чего дошёл сам, а не по книге; `bookCooldowns` — когда книгу можно
 *  читать снова (ключ книги → момент); `bookFailures` — сколько раз книга не
 *  далась (каждая неудача прибавляет к следующему броску). */
final case class LoreData(
  metElemental:   Boolean           = false,
  elementalLore:  Boolean           = false,
  metJoe:         Boolean           = false,
  joeLore:        Boolean           = false,
  prologueBranch: Option[String]    = None,
  knowledge:      List[String]      = Nil,
  selfTaught:     List[String]      = Nil,
  bookCooldowns:  Map[String, Long] = Map.empty,
  bookFailures:   Map[String, Int]  = Map.empty
) {
  def failuresOf(book: String): Int = bookFailures.getOrElse(book, 0)

  /** Книга не далась: час на переварить и +1 к счёту неудач. */
  def bookFailed(book: String, until: Long): LoreData =
    copy(bookCooldowns = bookCooldowns.updated(book, until), bookFailures = bookFailures.updated(book, failuresOf(book) + 1))

  /** Книга осилена: следы попыток больше не нужны. */
  def bookMastered(book: String): LoreData =
    copy(bookCooldowns = bookCooldowns - book, bookFailures = bookFailures - book)

  def knows(k: Knowledge): Boolean = knowledge.contains(k.entryName)

  def learnedAlone(k: Knowledge): Boolean = selfTaught.contains(k.entryName)

  /** Выучить: второй раз одно и то же не записывается. */
  def learn(k: Knowledge, alone: Boolean): LoreData =
    if (knows(k)) this
    else copy(knowledge = knowledge :+ k.entryName, selfTaught = if (alone) selfTaught :+ k.entryName else selfTaught)
}

object LoreData {
  val empty: LoreData = LoreData()

  implicit val encoder: Encoder[LoreData] = deriveEncoder

  /** Декодер рукописный, и каждое поле читается с запасным значением. Иначе
    * добавление нового знания (так было с Гнилым Джо) роняет разбор старых
    * записей целиком: герой, уже купивший легенду об элементалях, снова видел
    * бы её в продаже, а трактирщик — брал бы за неё серебро повторно. */
  implicit val decoder: Decoder[LoreData] = (c: HCursor) =>
    for {
      metElemental  <- c.getOrElse[Boolean]("metElemental")(false)
      elementalLore <- c.getOrElse[Boolean]("elementalLore")(false)
      metJoe        <- c.getOrElse[Boolean]("metJoe")(false)
      joeLore       <- c.getOrElse[Boolean]("joeLore")(false)
      branch        <- c.getOrElse[Option[String]]("prologueBranch")(None)
      knowledge     <- c.getOrElse[List[String]]("knowledge")(Nil)
      selfTaught    <- c.getOrElse[List[String]]("selfTaught")(Nil)
      cooldowns     <- c.getOrElse[Map[String, Long]]("bookCooldowns")(Map.empty)
      failures      <- c.getOrElse[Map[String, Int]]("bookFailures")(Map.empty)
    } yield LoreData(metElemental, elementalLore, metJoe, joeLore, branch, knowledge, selfTaught, cooldowns, failures)
}
