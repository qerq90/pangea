package pangea.model.battle

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Метка боя на арене внутри обычного боя. Пока она стоит, движок ведёт себя
  * иначе в трёх местах: соперник не отвечает на ход (он ответит своим),
  * победа не даёт ни добычи, ни опыта, а поражение не убивает героя.
  *
  * @param fightId строка боя в `arena_fights` — общая на обоих
  * @param foeUser кому передавать ход
  */
final case class ArenaRef(fightId: Long, foeUser: Long)

object ArenaRef {
  implicit val encoder: Encoder[ArenaRef] = deriveEncoder
  implicit val decoder: Decoder[ArenaRef] = deriveDecoder
}
