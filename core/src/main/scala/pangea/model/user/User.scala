package pangea.model.user

/** Игрок как человек, а не как персонаж: герои приходят и уходят (рестарт
  * сносит строку `heroes` целиком), а всё, что принадлежит самому человеку,
  * живёт здесь и это переживает.
  *
  * `receiptEmail` — адрес для кассового чека (см. [[ReceiptEmail]]). Нужен
  * только для чека и больше ни для чего. */
case class User(
  userId:       UserId,
  vkId:         VkId,
  telegramId:   TelegramId,
  receiptEmail: Option[String] = None
)
