package pangea.model.monster

import enumeratum._
import io.circe.{Decoder, HCursor}
import pangea.model.monster.Race.RaceFactor

sealed trait Race extends EnumEntry {
  val factor: RaceFactor
  val description: String
  val genitive: String // родительный падеж названия расы («Демона») — для имён трофеев
}

object Race extends Enum[Race] with DoobieEnum[Race] {
  case class RaceFactor(
    hpFactor: Double,
    defenceFactor: Double,
    attackFactor: Double,
    concentrationFactor: Double,
    accuracyFactor: Double,
    evasionFactor: Double
  )

  val values = findValues

  case object Human extends Race {
    override def toString: String = "Человек"
    val genitive: String          = "Человека"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1,
        defenceFactor = 1.5,
        attackFactor = 1,
        concentrationFactor = 2,
        accuracyFactor = 1,
        evasionFactor = 0.7
      )
    override val description: String = """Человек:
                                         |Внешность:
                                         |Разнообразные
                                         |черты лица и цвета кожи.
                                         |Разнообразные
                                         |цвета глаз и волос.
                                         |Средний рост и
                                         |телосложение.
                                         |Обычно не имеют
                                         |особенностей, отличающих их от других рас.
                                         |Сложно описать
                                         |столь невыдающееся существо, много плодятся, мало живут.
                                         |Места обитания:
                                         |Живут в городах,
                                         |деревнях, поселениях, распространены по всему миру.
                                         |Обладают
                                         |адаптивностью и способностью приспосабливаться к различным условиям, что так же
                                         |оказывает влияние на их тела.
                                         |
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  case object Elf extends Race {
    override def toString: String = "Эльф"
    val genitive: String          = "Эльфа"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1,
        defenceFactor = 1,
        attackFactor = 0.6,
        concentrationFactor = 1.4,
        accuracyFactor = 1.6,
        evasionFactor = 1.6
      )
    override val description: String = """Внешность:
                                         |Высокие,
                                         |стройные, с утонченными чертами лица.
                                         |Длинные,
                                         |заостренные уши, часто украшенные украшениями.
                                         |Глаза больших
                                         |размеров, с яркими, пронзительными
                                         |цветами - от глубокого синего до золотистого.
                                         |Волосы длинные,
                                         |прямые или волнистые, с оттенками от золотого до черного.
                                         |Кожа бледная, с
                                         |легким оливковым оттенком.
                                         |Места обитания:
                                         |Леса и поляны,
                                         |изредка горы и подземелье, где они строят свои деревни и храмы.
                                         |Известны своей
                                         |любовью к природе и гармонии.
                                         |
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  case object Murloc extends Race {
    override def toString: String = "Мурлок"
    val genitive: String          = "Мурлока"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 0.8,
        attackFactor = 1.5,
        concentrationFactor = 1.3,
        accuracyFactor = 1.5,
        evasionFactor = 0.5
      )
    override val description: String = """Мурлок:
                                         |Внешность:
                                         |Амфибии с различными
                                         |оттенками кожей (чаще, но не всегда с холодными оттенками), покрытой чешуей.
                                         |Гуманоидное строение, напоминают сильно горбящихся людей
                                         |Глаза большие и
                                         |выпученные, с желтыми и реже белыми радужками.
                                         |Рот и челюсть
                                         |сильно выдвинуты вперед, с острыми клыками.
                                         |Пальцы на руках
                                         |и ногах напоминают перепонки.
                                         |Взаимная
                                         |ненависть с Аргонианцами
                                         |Места обитания:
                                         |Обитают в реках,
                                         |озерах и морях.
                                         |Хорошо себя чувствуют,
                                         |как в воде, так и на суше.
                                         |
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  case object Orc extends Race {
    override def toString: String = "Орк"
    val genitive: String          = "Орка"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 1.5,
        attackFactor = 2,
        concentrationFactor = 0.8,
        accuracyFactor = 0.6,
        evasionFactor = 0.4
      )
    override val description: String = """Орк:
                                         |Внешность:
                                         |Крупные,
                                         |мускулистые, с грубыми чертами лица.
                                         |Кожа зеленая,
                                         |серая или коричневая.
                                         |Волосы темные,
                                         |короткие, часто заплетенные в косы.
                                         |Клыки торчат
                                         |из-под губ, а кожа на лбу обычно покрыта шрамами.
                                         |Массивные, широкие
                                         |плечи и сильные руки.
                                         |Места обитания:
                                         |Горные долины,
                                         |пещеры, крепости.
                                         |Предпочитают
                                         |суровые условия и тяжелую работу.
                                         |
                                         |
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  case object Goblin extends Race {
    override def toString: String = "Гоблин"
    val genitive: String          = "Гоблина"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 0.6,
        defenceFactor = 0.5,
        attackFactor = 1,
        concentrationFactor = 1.1,
        accuracyFactor = 1.5,
        evasionFactor = 2
      )
    override val description: String = """Гоблин:
                                         |Внешность:
                                         |Низкорослые, с
                                         |кривыми конечностями и толстой зеленой кожей.
                                         |Большие головы с
                                         |плоскими лицами, выпученными глазами и острыми зубами.
                                         |Волосы редкие,
                                         |грязно-зеленого цвета.
                                         |Не обладают
                                         |особой силой, но ловки и хитры.
                                         |Места обитания:
                                         |Болота, пещеры,
                                         |заброшенные руины.
                                         |Известны своим
                                         |коварством и склонностью к грабежу.
                                         |
                                         |
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  case object Demon extends Race {
    override def toString: String = "Демон"
    val genitive: String          = "Демона"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 0.8,
        defenceFactor = 0.8,
        attackFactor = 1.5,
        concentrationFactor = 1,
        accuracyFactor = 1.2,
        evasionFactor = 0.7
      )
    override val description: String = """Демон:
                                         |Внешность:
                                         |В зависимости от типа демона:
                                         |Разнообразные размеры, формы, цвета и черты.
                                         |Часто
                                         |крылья, рога, копыта, клыки, когти, хвосты.
                                         |Глаза,
                                         |светящиеся зловещим светом.
                                         |Кожа может
                                         |быть красной, черной, зеленой, синей, а также с чешуей или шипами.
                                         |Места обитания:
                                         |Обитают в разных
                                         |местах, включая подземелья, развалины, темные леса.
                                         |Их местонахождение
                                         |зависит от их типа и власти которой обладает демон.
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  case object Gnome extends Race {
    override def toString: String = "Гном"
    val genitive: String          = "Гнома"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 2,
        attackFactor = 1.5,
        concentrationFactor = 1.3,
        accuracyFactor = 1.2,
        evasionFactor = 0.5
      )
    override val description: String = """Гном:
                                         |Внешность:
                                         |Невысокие, с
                                         |коренастым телосложением и крепкими костями.
                                         |Большие руки и
                                         |ступни, покрытые густыми волосами, практически всегда имеют бороду.
                                         |Кожа грубая, с
                                         |розовым или красноватым оттенком.
                                         |Нос короткий, с
                                         |широкими ноздрями.
                                         |Глаза яркие,
                                         |глубоко посаженные, с серым или голубым цветом.
                                         |Места обитания:
                                         |Глубокие
                                         |подземные пещеры, где они строят свои города и мастерские.
                                         |Известны своим
                                         |умением обращаться с металлами и камнями.
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  case object Khajiit extends Race {
    override def toString: String = "Каджит"
    val genitive: String          = "Каджита"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 0.6,
        attackFactor = 1.6,
        concentrationFactor = 1.5,
        accuracyFactor = 1.3,
        evasionFactor = 2
      )
    override val description: String = """Каджит:
                                         |Внешность:
                                         |Антропоморфные
                                         |кошки с густой шерстью различных цветов.
                                         |Уши длинные и
                                         |заостренные, хвосты пушистые.
                                         |Глаза большие и
                                         |миндалевидные, с вертикальными зрачками.
                                         |Лица с
                                         |выразительными чертами и тонкими губами.
                                         |Места обитания:
                                         |Пустыни, леса,
                                         |горы.
                                         |Известны своей
                                         |независимостью, любовью к свободе и склонностью к торговле.
                                         |
                                         |
                                         |
                                         |Заметки путешественника Ашалдарона о расах Пангеи""".stripMargin
  }

  implicit val decoder: Decoder[Race] = (c: HCursor) =>
    c.get[String]("race").map(Race.withName)
}
