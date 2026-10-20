package pangea.model.vk

case class Attachment(`type`: AttachmentType, ownerId: String, id: String) {
  override def toString: String = s"${`type`}${ownerId}_$id"
}

object Attachment {
  private val Ref = """(photo|video|audio|doc|wall)(-?\d+)_(\d+)""".r

  /** Вложение из ссылки на него: `photo-196380979_457239080` или полный адрес
    * вида `https://vk.ru/photo-196380979_457239080`. Не разобрали — None. */
  def parse(ref: String): Option[Attachment] =
    Ref.findFirstMatchIn(ref).flatMap { m =>
      AttachmentType.values.find(_.toString == m.group(1)).map(Attachment(_, m.group(2), m.group(3)))
    }
}
