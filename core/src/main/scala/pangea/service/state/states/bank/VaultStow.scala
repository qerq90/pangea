package pangea.service.state.states.bank

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.model.artifact.ArtifactKind
import pangea.model.bank.{StowGroup, StowSettings}
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.user.UserId
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.bank.{BankRepoError, BankRepository}
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Что легло в ячейку за одну «Положить всё»: сколько вещей какого рода, что
  * пришло из других хранилищ и сколько ушло серебра. `full` — место в ячейке
  * кончилось раньше, чем вещи. */
final case class StowReport(
  byGroup:      Map[StowGroup, Int]    = Map.empty,
  fromArtifact: Map[ArtifactKind, Int] = Map.empty,
  silver:       Long                   = 0L,
  full:         Boolean                = false
) {
  def items: Int = byGroup.values.sum + fromArtifact.values.sum
  def empty: Boolean = items == 0 && silver == 0L

  def plus(g: StowGroup): StowReport         = copy(byGroup = byGroup.updated(g, byGroup.getOrElse(g, 0) + 1))
  def plus(k: ArtifactKind): StowReport      = copy(fromArtifact = fromArtifact.updated(k, fromArtifact.getOrElse(k, 0) + 1))
}

/** «Положить всё»: разом сгружает в ячейку то, что герой отметил в настройке.
  *
  * Порядок такой же, как если бы он перекладывал руками: сперва сумка, потом
  * ларец, шкаф и живая сумка, в самом конце серебро. Забитая ячейка
  * останавливает укладку — остальное остаётся при герое, и он об этом узнает
  * из отчёта.
  */
object VaultStow {

  /** Ларец, шкаф и живая сумка — какие переключатели их открывают. */
  val storages: List[(StowGroup, ArtifactKind)] = List(
    StowGroup.Casket    -> ArtifactKind.Casket,
    StowGroup.Wardrobe  -> ArtifactKind.Wardrobe,
    StowGroup.LivingBag -> ArtifactKind.LivingBag)

  def read(heroDao: HeroDao, userId: UserId): Task[StowSettings] =
    heroDao.readVaultStow(userId).map(_.flatMap(_.as[StowSettings].toOption).getOrElse(StowSettings.default))

  def write(heroDao: HeroDao, userId: UserId, settings: StowSettings): Task[Unit] =
    heroDao.writeVaultStow(userId, settings.asJson)

  def toggle(heroDao: HeroDao, userId: UserId, group: StowGroup): Task[StowSettings] =
    read(heroDao, userId).flatMap { s =>
      val next = s.toggle(group)
      write(heroDao, userId, next).as(next)
    }

  def run(
    heroDao:       HeroDao,
    inventoryRepo: InventoryRepository,
    bankRepo:      BankRepository,
    artifacts:     Option[ArtifactRepository],
    user:          UserId,
    hero:          Hero,
    settings:      StowSettings
  ): Task[StowReport] =
    for {
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      bag     = inv.items.data.filter(settings.takes)
      afterBag <- ZIO.foldLeft(bag)(StowReport()) { (report, item) =>
                    if (report.full) ZIO.succeed(report)
                    else fromBag(inventoryRepo, bankRepo, hero, item, report, settings)
                  }
      afterArt <- ZIO.foldLeft(storages)(afterBag) { case (report, (group, kind)) =>
                    if (report.full || !settings.on(group)) ZIO.succeed(report)
                    else emptyArtifact(bankRepo, artifacts, hero, kind, report)
                  }
      done   <- if (!settings.silver) ZIO.succeed(afterArt)
                else stowSilver(heroDao, bankRepo, user, hero, afterArt)
    } yield done

  /** Вещь из сумки: сперва в ячейку, и только потом из сумки — если ячейка
    * откажет, вещь останется при герое. */
  private def fromBag(
    inventoryRepo: InventoryRepository,
    bankRepo:      BankRepository,
    hero:          Hero,
    item:          Item,
    report:        StowReport,
    settings:      StowSettings
  ): Task[StowReport] =
    bankRepo.deposit(hero.id, item).either.flatMap {
      case Right(_) =>
        inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable)
          .as(settings.groupOf(item).fold(report)(report.plus))
      // Горстей этого вида в ячейке уже предел — вещь остаётся, но укладка идёт дальше.
      case Left(BankRepoError.DustLimitReached) => ZIO.succeed(report)
      case Left(BankRepoError.VaultFull)        => ZIO.succeed(report.copy(full = true))
      case Left(e)                              => ZIO.fail(asThrowable(e))
    }

  /** Выгрести хранилище целиком: что не влезло в ячейку — возвращаем на место. */
  private def emptyArtifact(
    bankRepo:  BankRepository,
    artifacts: Option[ArtifactRepository],
    hero:      Hero,
    kind:      ArtifactKind,
    report:    StowReport
  ): Task[StowReport] =
    artifacts match {
      case None => ZIO.succeed(report)
      case Some(repo) =>
        repo.get(hero.id).mapError(asThrowable).flatMap { all =>
          val inside = all.of(kind).items.data.filterNot(_.isQuestItem)
          ZIO.foldLeft(inside)(report) { (acc, item) =>
            if (acc.full) ZIO.succeed(acc)
            else
              repo.take(hero.id, kind, item.id).either.flatMap {
                case Left(_) => ZIO.succeed(acc)
                case Right(taken) =>
                  bankRepo.deposit(hero.id, taken).either.flatMap {
                    case Right(_) => ZIO.succeed(acc.plus(kind))
                    // Не влезло — кладём обратно туда, откуда взяли.
                    case Left(e) =>
                      repo.put(hero.id, kind, taken).either
                        .as(if (e == BankRepoError.VaultFull) acc.copy(full = true) else acc)
                  }
              }
          }
        }
    }

  /** Серебро с рук — сколько примет ячейка. */
  private def stowSilver(
    heroDao:  HeroDao,
    bankRepo: BankRepository,
    user:     UserId,
    hero:     Hero,
    report:   StowReport
  ): Task[StowReport] =
    bankRepo.get(hero.id).mapError(asThrowable).flatMap { vault =>
      val amount = hero.silver.min(vault.freeSilverSpace).max(0L)
      if (amount <= 0L) ZIO.succeed(report)
      else
        bankRepo.depositSilver(hero.id, amount).mapError(asThrowable) *>
          heroDao.updateSilver(user, hero.silver - amount).as(report.copy(silver = amount))
    }

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
