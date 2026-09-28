package pangea.service.admin

import pangea.dao.admin.{AdminDao, AdminStats}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Choice, ChoiceColor, Renderer, Screen}
import pangea.generator.item.ItemGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.StatesMap
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Random, Ref, Task, UIO, ZIO, ZLayer}

/** Где сейчас находится админ внутри панели. Сессия живёт в памяти процесса и
  * ничего не пишет ни в состояние героя, ни в `scene_data`: панель открывается
  * поверх любой сцены — хоть посреди боя, — и, закрывшись, возвращает игрока
  * ровно туда, где он был. */
sealed trait AdminScreen
object AdminScreen {
  case object Password extends AdminScreen
  case object Main     extends AdminScreen
  case object Items    extends AdminScreen
  case object Money    extends AdminScreen
  case object Stats    extends AdminScreen

  /** Каталог: разделы и листание внутри раздела. */
  case object Sections                                  extends AdminScreen
  final case class Listing(category: String, page: Int) extends AdminScreen
  final case class Found(query: String, page: Int)      extends AdminScreen

  /** Сборка экипировки по частям. */
  case object EquipType                                          extends AdminScreen
  final case class EquipRarity(itemType: ItemType)               extends AdminScreen
  final case class EquipLevel(itemType: ItemType, rarity: Rarity) extends AdminScreen
}

/** Пароль панели. Берётся из окружения (`ADMIN_PASSWORD`) и в репозитории не
  * хранится: пока переменная не задана, панель не пускает никого. */
final case class AdminConfig(password: Option[String])

object AdminConfig {
  val EnvName: String = "ADMIN_PASSWORD"

  val live: ZLayer[Any, Nothing, AdminConfig] =
    ZLayer.fromZIO(
      System.getenv(EnvName) match {
        case null                       => ZIO.succeed(AdminConfig(None))
        case p if p.trim.isEmpty        => ZIO.succeed(AdminConfig(None))
        case p                          => ZIO.succeed(AdminConfig(Some(p.trim)))
      })
}

trait AdminPanel {

  /** Взять ввод себе, если это «/admin» или игрок уже в панели. `false` —
    * сообщение обычное, пусть его разбирает игровой автомат. */
  def intercept(user: User, hero: Hero, action: UserAction, renderer: Renderer): Task[Boolean]
}

class AdminPanelLive(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  adminDao:      AdminDao,
  states:        Map[StateType, State],
  config:        AdminConfig,
  sessions:      Ref[Map[UserId, AdminScreen]]
) extends AdminPanel {

  import AdminPanel._

  override def intercept(user: User, hero: Hero, action: UserAction, renderer: Renderer): Task[Boolean] =
    sessions.get.map(_.get(user.userId)).flatMap {
      case _ if isCommand(action)   => open(user, renderer).as(true)
      // Аварийные команды сильнее панели: /home должен вытаскивать откуда угодно.
      case Some(_) if escapes(action) => sessions.update(_ - user.userId).as(false)
      case Some(AdminScreen.Password) => checkPassword(user, action, renderer).as(true)
      case Some(screen)               => handle(user, hero, screen, action, renderer).as(true)
      case None                       => ZIO.succeed(false)
    }

  // ── Вход ──────────────────────────────────────────────────────────────────

  private def open(user: User, renderer: Renderer): Task[Unit] =
    sessions.update(_.updated(user.userId, AdminScreen.Password)) *>
      renderer.show(user, Screen(PasswordAsk, List(cancelButton)))

  private def checkPassword(user: User, action: UserAction, renderer: Renderer): Task[Unit] =
    if (parse(action).contains(CancelId)) leave(user, renderer)
    else config.password match {
      case None =>
        sessions.update(_ - user.userId) *>
          renderer.show(user, Screen(NoPassword, Nil))
      case Some(expected) if action.text.trim == expected =>
        goTo(user, AdminScreen.Main, renderer)
      case Some(_) =>
        sessions.update(_ - user.userId) *>
          renderer.show(user, Screen(WrongPassword, Nil))
    }

  // ── Разбор нажатий ────────────────────────────────────────────────────────

  private def handle(user: User, hero: Hero, screen: AdminScreen, action: UserAction, renderer: Renderer): Task[Unit] = {
    val id   = parse(action)
    val text = action.text.trim
    id match {
      case Some(CancelId)                       => leave(user, renderer)
      case Some("AdminMain")                    => goTo(user, AdminScreen.Main, renderer)
      case Some("AdminItems")                   => goTo(user, AdminScreen.Items, renderer)
      case Some("AdminSections")                => goTo(user, AdminScreen.Sections, renderer)
      case Some("AdminStats")                   => goTo(user, AdminScreen.Stats, renderer)
      case Some("AdminMoney")                   => goTo(user, AdminScreen.Money, renderer)
      case Some("AdminEquip")                   => goTo(user, AdminScreen.EquipType, renderer)
      case Some(a) if a.startsWith(SectionPrefix) =>
        goTo(user, AdminScreen.Listing(a.drop(SectionPrefix.length), 0), renderer)
      case Some(a) if a.startsWith(GivePrefix)  => give(user, hero, a.drop(GivePrefix.length), screen, renderer)
      case Some(a) if a.startsWith(TypePrefix)  =>
        ItemType.withNameOption(a.drop(TypePrefix.length))
          .fold(goTo(user, AdminScreen.EquipType, renderer))(t => goTo(user, AdminScreen.EquipRarity(t), renderer))
      case Some(a) if a.startsWith(RarityPrefix) =>
        (screen, Rarity.withNameOption(a.drop(RarityPrefix.length))) match {
          case (AdminScreen.EquipRarity(t), Some(r)) => goTo(user, AdminScreen.EquipLevel(t, r), renderer)
          case _                                     => goTo(user, AdminScreen.EquipType, renderer)
        }
      case Some(a) if a.startsWith(SilverPrefix) =>
        a.drop(SilverPrefix.length).toLongOption.fold(show(user, screen, renderer))(addSilver(user, hero, _, renderer))
      case Some(a) if a.startsWith(DoubloonPrefix) =>
        a.drop(DoubloonPrefix.length).toLongOption.fold(show(user, screen, renderer))(addDoubloons(user, hero, _, renderer))
      case Some(PrevId)                         => turn(user, screen, -1, renderer)
      case Some(NextId)                         => turn(user, screen, +1, renderer)
      // Текст без кнопки: на экране уровня это сам уровень, в остальных
      // местах — поиск по названию.
      case _ if text.nonEmpty                   => screen match {
        case AdminScreen.EquipLevel(t, r) => level(user, hero, t, r, text, renderer)
        case _                            => goTo(user, AdminScreen.Found(text, 0), renderer)
      }
      case _                                    => show(user, screen, renderer)
    }
  }

  private def turn(user: User, screen: AdminScreen, delta: Int, renderer: Renderer): Task[Unit] =
    screen match {
      case AdminScreen.Listing(c, p) => goTo(user, AdminScreen.Listing(c, (p + delta).max(0)), renderer)
      case AdminScreen.Found(q, p)   => goTo(user, AdminScreen.Found(q, (p + delta).max(0)), renderer)
      case other                     => show(user, other, renderer)
    }

  private def goTo(user: User, screen: AdminScreen, renderer: Renderer): Task[Unit] =
    sessions.update(_.updated(user.userId, screen)) *> show(user, screen, renderer)

  /** Уйти из панели и вернуть игрока в ту сцену, где он был. */
  private def leave(user: User, renderer: Renderer): Task[Unit] =
    for {
      _    <- sessions.update(_ - user.userId)
      _    <- renderer.show(user, Screen(Closed, Nil))
      hero <- heroDao.getHeroByUserId(user.userId)
      _    <- ZIO.foreachDiscard(hero.flatMap(h => states.get(h.state)))(_.enter(user, renderer))
    } yield ()

  // ── Экраны ────────────────────────────────────────────────────────────────

  private def show(user: User, screen: AdminScreen, renderer: Renderer): Task[Unit] = screen match {
    case AdminScreen.Password => renderer.show(user, Screen(PasswordAsk, List(cancelButton)))

    case AdminScreen.Main =>
      renderer.show(user, Screen(MainText, List(
        Choice("AdminItems", "🎁 Вещи", color = ChoiceColor.Positive, row = Some(0)),
        Choice("AdminMoney", "💰 Деньги", color = ChoiceColor.Positive, row = Some(0)),
        Choice("AdminStats", "📊 Статистика", row = Some(1)),
        cancelButton.copy(row = Some(2)))))

    case AdminScreen.Items =>
      renderer.show(user, Screen(ItemsText, List(
        Choice("AdminEquip", "⚔ Экипировка", color = ChoiceColor.Positive, row = Some(0)),
        Choice("AdminSections", "📦 Каталог", color = ChoiceColor.Positive, row = Some(0)),
        back("AdminMain", 1))))

    case AdminScreen.Sections =>
      val buttons = AdminCatalog.categories.zipWithIndex.map { case (c, i) =>
        Choice(s"$SectionPrefix${c.id}", c.label, row = Some(i / PerRow))
      }
      val rows = (AdminCatalog.categories.size + PerRow - 1) / PerRow
      renderer.show(user, Screen(SectionsText, buttons :+ back("AdminItems", rows)))

    case AdminScreen.Listing(id, page) =>
      AdminCatalog.category(id) match {
        case None    => goTo(user, AdminScreen.Sections, renderer)
        case Some(c) => renderer.show(user, listScreen(s"${c.label}. Что выдать?", c.entries, page, "AdminSections"))
      }

    case AdminScreen.Found(query, page) =>
      val found = AdminCatalog.search(query)
      if (found.isEmpty) renderer.show(user, Screen(nothingFound(query), List(back("AdminItems", 0))))
      else renderer.show(user, listScreen(foundHeader(query, found.size), found, page, "AdminItems"))

    case AdminScreen.EquipType =>
      val buttons = ItemType.equippable.zipWithIndex.map { case (t, i) =>
        Choice(s"$TypePrefix${t.entryName}", typeLabel(t), row = Some(i / TypesPerRow))
      }
      val rows = (ItemType.equippable.size + TypesPerRow - 1) / TypesPerRow
      renderer.show(user, Screen(EquipTypeText, buttons :+ back("AdminItems", rows)))

    case AdminScreen.EquipRarity(t) =>
      val buttons = Rarity.values.toList.zipWithIndex.map { case (r, i) =>
        Choice(s"$RarityPrefix${r.entryName}", s"${r.emoji} ${r.entryName}", row = Some(i / TypesPerRow))
      }
      val rows = (Rarity.values.size + TypesPerRow - 1) / TypesPerRow
      renderer.show(user, Screen(s"${typeLabel(t)}: какая редкость?", buttons :+ back("AdminEquip", rows)))

    case AdminScreen.EquipLevel(t, r) =>
      renderer.show(user, Screen(
        s"${r.emoji} ${typeLabel(t)}: пришлите уровень числом (1–$MaxLevel).",
        List(back("AdminEquip", 0))))

    case AdminScreen.Money =>
      val silver = SilverSteps.zipWithIndex.map { case (n, i) =>
        Choice(s"$SilverPrefix$n", s"+${amount(n)} 🪙", color = ChoiceColor.Positive, row = Some(0 + i / 3)) }
      val doubloons = DoubloonSteps.zipWithIndex.map { case (n, i) =>
        Choice(s"$DoubloonPrefix$n", s"+${amount(n)} 💰", color = ChoiceColor.Positive, row = Some(1 + i / 3)) }
      renderer.show(user, Screen(MoneyText, silver ++ doubloons :+ back("AdminMain", 2)))

    case AdminScreen.Stats =>
      adminDao.stats.either.flatMap {
        case Left(e)  => renderer.show(user, Screen(s"Статистика не собралась: ${e.getMessage}", List(back("AdminMain", 0))))
        case Right(s) => renderer.show(user, Screen(statsText(s), List(
          Choice("AdminStats", "🔄 Обновить", row = Some(0)), back("AdminMain", 1))))
      }
  }

  /** Экран со списком вещей: восемь на страницу и навигация, как в сумке. */
  private def listScreen(header: String, entries: List[AdminCatalog.Entry], page: Int, backTo: String): Screen = {
    val (slice, pages, p) = ItemMenu.page(entries, page)
    val buttons = slice.zipWithIndex.map { case (e, i) =>
      Choice(s"$GivePrefix${e.id}", ItemMenu.truncate(e.label), row = Some(i))
    }
    val nav = List(
      Some(back(backTo, ItemMenu.NavRow)),
      Option.when(p > 0)(Choice(PrevId, "◀ Пред.", row = Some(ItemMenu.NavRow))),
      Option.when(p < pages - 1)(Choice(NextId, "След. ▶", row = Some(ItemMenu.NavRow)))
    ).flatten
    Screen(s"$header (стр. ${p + 1}/$pages)", buttons ++ nav)
  }

  // ── Выдача ────────────────────────────────────────────────────────────────

  private def give(user: User, hero: Hero, entryId: String, screen: AdminScreen, renderer: Renderer): Task[Unit] =
    AdminCatalog.find(entryId) match {
      case None        => show(user, screen, renderer)
      case Some(entry) => handOver(user, hero, entry.item, screen, renderer)
    }

  /** Уровень, присланный числом. Не число или не в диапазоне — так и скажем,
    * а экран оставим на месте: переспрашивать удобнее, чем начинать сначала. */
  private def level(user: User, hero: Hero, itemType: ItemType, rarity: Rarity, text: String, renderer: Renderer): Task[Unit] =
    text.trim.toLongOption.filter(l => l >= 1L && l <= MaxLevel) match {
      case Some(lvl) => forge(user, hero, itemType, rarity, lvl, renderer)
      case None      =>
        renderer.show(user, Screen(BadLevel, Nil)) *> show(user, AdminScreen.EquipLevel(itemType, rarity), renderer)
    }

  /** Собрать экипировку по частям и выдать. */
  private def forge(user: User, hero: Hero, itemType: ItemType, rarity: Rarity, lvl: Long, renderer: Renderer): Task[Unit] =
    for {
      seed      <- Random.nextLong
      (item, _)  = ItemGenerator.createItemOfType(itemType, lvl, rarity, Rng(seed))
      _         <- handOver(user, hero, item, AdminScreen.EquipLevel(itemType, rarity), renderer)
    } yield ()

  /** Вещь кладётся прямо в сумку, мимо артефактов: в панели важно, чтобы она
    * оказалась там, где её ждут. Сумка полна — так и скажем. */
  private def handOver(user: User, hero: Hero, item: Item, screen: AdminScreen, renderer: Renderer): Task[Unit] =
    itemRepo.persist(hero.id, item).flatMap { persisted =>
      inventoryRepo.addItem(hero.id, persisted).either.flatMap {
        case Right(_) => renderer.show(user, Screen(s"$Given ${persisted.displayTitle}", Nil)) *> show(user, screen, renderer)
        case Left(_)  => renderer.show(user, Screen(BagFull, Nil)) *> show(user, screen, renderer)
      }
    }

  private def addSilver(user: User, hero: Hero, delta: Long, renderer: Renderer): Task[Unit] = {
    val total = (hero.silver + delta).max(0L)
    heroDao.updateSilver(user.userId, total) *>
      renderer.show(user, Screen(s"🪙 Серебро: ${amount(hero.silver)} → ${amount(total)}", Nil)) *>
      show(user, AdminScreen.Money, renderer)
  }

  private def addDoubloons(user: User, hero: Hero, delta: Long, renderer: Renderer): Task[Unit] = {
    val total = (hero.doubloons + delta).max(0L)
    heroDao.updateDoubloons(user.userId, total) *>
      renderer.show(user, Screen(s"💰 Дублоны: ${amount(hero.doubloons)} → ${amount(total)}", Nil)) *>
      show(user, AdminScreen.Money, renderer)
  }

  // ── Мелочи ────────────────────────────────────────────────────────────────

  private def back(to: String, row: Int): Choice =
    Choice(to, "◀ Назад", color = ChoiceColor.Negative, row = Some(row))

  private def cancelButton: Choice =
    Choice(CancelId, "✖ Выйти из панели", color = ChoiceColor.Negative, row = Some(0))

  private def parse(action: UserAction): Option[String] =
    action.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))
}

object AdminPanel {

  /** Чем заменяется пароль в логах. */
  val Masked: String = "<скрыто>"

  /** Текст входящего сообщения для лога. Пароль панели пишется игроком обычным
    * сообщением, а входящие мы логируем целиком — и лог отдаётся наружу
    * (`GET /logs`). Поэтому ровно это значение в логе подменяется. */
  def maskSecrets(text: String, password: Option[String]): String =
    password.filter(p => p.nonEmpty && text.trim == p).fold(text)(_ => Masked)

  /** Команда входа. Как и `/home`, ловится на голый текст без payload. */
  def isCommand(action: UserAction): Boolean =
    action.payload.isEmpty && action.text.trim.equalsIgnoreCase("/admin")

  /** Команды, которые сильнее панели: они закрывают её и идут своим чередом. */
  def escapes(action: UserAction): Boolean =
    action.payload.isEmpty && {
      val t = action.text.trim.toLowerCase
      t == "/home" || t == "/restart"
    }

  val CancelId: String       = "AdminExit"
  val PrevId: String         = "AdminPrev"
  val NextId: String         = "AdminNext"
  val GivePrefix: String     = "AdminGive_"
  val SectionPrefix: String  = "AdminSection_"
  val TypePrefix: String     = "AdminType_"
  val RarityPrefix: String   = "AdminRarity_"
  val SilverPrefix: String   = "AdminSilver_"
  val DoubloonPrefix: String = "AdminDoubloon_"

  /** Раскладка кнопок: панель служебная, но в клавиатуру ВК влезать обязана. */
  val PerRow: Int      = 2
  val TypesPerRow: Int = 3

  /** Выше этого уровня вещей в игре не бывает (см. `Hero.MaxLevel`). */
  val MaxLevel: Long = 150L

  /** Номиналы кнопок «начислить себе». */
  val SilverSteps: List[Long]   = List(1000L, 10000L, 100000L)
  val DoubloonSteps: List[Long] = List(10L, 100L, 1000L)

  // Тексты панели живут здесь, а не в scenes.yaml: это не игровой контент, а
  // служебный экран, которого игрок не видит.
  val PasswordAsk: String =
    "🔒 Админ-панель. Пришлите пароль одним сообщением."
  val NoPassword: String =
    s"🔒 Пароль не задан: панель выключена. Задайте переменную окружения ${AdminConfig.EnvName} и перезапустите бота."
  val WrongPassword: String = "🔒 Неверно. Панель закрыта."
  val Closed: String        = "Панель закрыта."
  val MainText: String      = "🛠 Админ-панель."
  val ItemsText: String     =
    "🎁 Выдать вещь.\n«Экипировка» — собрать по типу, редкости и уровню.\n«Каталог» — всё остальное списком.\nИли просто пришлите часть названия — найду по ней."
  val SectionsText: String  = "📦 Каталог. Какой раздел?"
  val EquipTypeText: String = "⚔ Экипировка. Какой слот?"
  val MoneyText: String     = "💰 Начислить себе."
  val Given: String         = "✅ Выдано:"
  val BagFull: String       = "❌ Сумка полна — вещь не влезла."
  val BadLevel: String      = s"Нужно число от 1 до $MaxLevel."

  def nothingFound(query: String): String = s"По «$query» ничего не нашлось."

  def foundHeader(query: String, count: Int): String = s"Найдено по «$query»: $count"

  /** Число с пробелами по три знака: 1 234 567. */
  def amount(n: Long): String =
    n.abs.toString.reverse.grouped(3).mkString(" ").reverse match {
      case s if n < 0 => s"-$s"
      case s          => s
    }

  def typeLabel(t: ItemType): String = t.entryName

  /** Сводка по серверу одним сообщением. */
  def statsText(s: AdminStats): String = {
    val sets =
      if (s.sets.isEmpty) "  никто не собрал и двух предметов набора"
      else s.sets.map(r => s"  ${r.set.label} на ${r.worn}: ${r.heroes}").mkString("\n")
    s"""📊 Сервер
       |
       |👥 Игроки
       |  всего героев: ${s.heroes}
       |  заходили за сутки: ${s.active24h}
       |  заходили за неделю: ${s.active7d}
       |
       |💰 Деньги
       |  серебро у героев: ${amount(s.heroSilver)}
       |  серебро в ячейках: ${amount(s.vaultSilver)}
       |  серебро всего: ${amount(s.silverTotal)}
       |  дублоны: ${amount(s.doubloons)}
       |
       |🛡 Наборы (сколько предметов надето: сколько героев)
       |$sets""".stripMargin
  }

  val live: ZLayer[
    HeroDao with InventoryRepository with ItemRepository with AdminDao with StatesMap with AdminConfig,
    Nothing,
    AdminPanel
  ] =
    ZLayer.fromZIO(
      for {
        heroDao   <- ZIO.service[HeroDao]
        inventory <- ZIO.service[InventoryRepository]
        items     <- ZIO.service[ItemRepository]
        admin     <- ZIO.service[AdminDao]
        statesMap <- ZIO.service[StatesMap]
        config    <- ZIO.service[AdminConfig]
        sessions  <- Ref.make(Map.empty[UserId, AdminScreen])
      } yield new AdminPanelLive(heroDao, inventory, items, admin, statesMap.states, config, sessions))

  /** Панель, которой нет: для тестов и сборок без админки. */
  val disabled: AdminPanel = new AdminPanel {
    def intercept(user: User, hero: Hero, action: UserAction, renderer: Renderer): Task[Boolean] =
      ZIO.succeed(false)
  }

  private[admin] def noSessions: UIO[Ref[Map[UserId, AdminScreen]]] = Ref.make(Map.empty[UserId, AdminScreen])
}
