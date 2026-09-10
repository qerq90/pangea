package pangea.model.item

/** Ломка камней: камень в гнезде или лежащий в инвентаре крошится в пыль своего
  * вида. Чистое ядро без экранов и хранилищ — им пользуются и инвентарь, и
  * экипировка, чтобы правила были одни на все входы.
  *
  * Обратный ход — рецепт куба «три пыли → надколотый камень»
  * (см. `CubeCraft.DustAssembly`). */
object GemBreaking {

  /** Камни в гнёздах предмета вместе с номером гнезда — для экрана выбора,
    * когда камней больше одного. */
  def socketed(item: Item): List[(Int, Gem)] =
    item.sockets.zipWithIndex.collect { case (Some(g), idx) => idx -> g }

  /** Есть ли что ломать в этом предмете. */
  def hasGems(item: Item): Boolean = item.socketedGems.nonEmpty

  /** Выбивает камень из гнезда: предмет с опустевшим гнездом и сам камень.
    * None — если гнезда с таким номером нет или оно и так пустое. */
  def breakSocket(item: Item, idx: Int): Option[(Item, Gem)] =
    item.sockets.lift(idx).flatten.map { gem =>
      (item.copy(sockets = item.sockets.updated(idx, None)), gem)
    }
}
