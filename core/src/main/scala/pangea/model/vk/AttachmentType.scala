package pangea.model.vk

import enumeratum._

sealed trait AttachmentType extends EnumEntry

object AttachmentType extends Enum[AttachmentType] {

  val values = findValues

  case object Photo extends AttachmentType {
    override def toString: String = "photo"
  }

  case object Video extends AttachmentType {
    override def toString: String = "video"
  }

  case object Audio extends AttachmentType {
    override def toString: String = "audio"
  }

  case object Doc extends AttachmentType {
    override def toString: String = "doc"
  }

  case object Wall extends AttachmentType {
    override def toString: String = "wall"
  }

  case object Market extends AttachmentType {
    override def toString: String = "market"
  }

  case object Poll extends AttachmentType {
    override def toString: String = "poll"
  }
}
