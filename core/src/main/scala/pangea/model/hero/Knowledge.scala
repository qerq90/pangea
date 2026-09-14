package pangea.model.hero

import enumeratum._

/** Что герой умеет: знания открывают то, что без них не разглядеть, — например,
  * какие цветы на поляне чего-то стоят. Хранятся ключами в `lore_data`. */
sealed abstract class Knowledge(val title: String, val description: String) extends EnumEntry

object Knowledge extends Enum[Knowledge] {
  val values: IndexedSeq[Knowledge] = findValues

  /** Простые травы: без этого на поляне попадаются только «странные цветки». */
  case object FlowersRank1 extends Knowledge(
    "Знания о цветах 1 ранга",
    "Вы отличаете крапиву от календулы и знаете, за что травник заплатит."
  )

  /** Редкие травы: пузырьковая лилия, цветок-мираж и прочее, что без книги — тоже «странное». */
  case object FlowersRank2 extends Knowledge(
    "Знания о цветах 2 ранга",
    "Редкие растения, о которых пишут в трактатах, а не рассказывают у костра."
  )

  /** Знание, нужное, чтобы узнать траву этого ранга. */
  def forHerbRank(rank: Int): Option[Knowledge] = rank match {
    case 1 => Some(FlowersRank1)
    case 2 => Some(FlowersRank2)
    case _ => None
  }
}
