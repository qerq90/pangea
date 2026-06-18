package pangea.engine

import doobie.Fragment
import doobie.implicits._
import doobie.util.transactor.Transactor
import zio.interop.catz._
import zio.{Task, ZIO}

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object PartitionManager {

  private val fmt = DateTimeFormatter.ISO_LOCAL_DATE

  def ensurePartitions(xa: Transactor[Task]): Task[Unit] = {
    val now    = LocalDate.now()
    val months = List(now, now.plusMonths(1), now.plusMonths(2))
    ZIO.foreach(months)(createPartition(xa, _)).unit
  }

  private def createPartition(xa: Transactor[Task], month: LocalDate): Task[Unit] = {
    val name  = s"event_log_${month.getYear}_${"%02d".format(month.getMonthValue)}"
    val from  = month.withDayOfMonth(1)
    val until = from.plusMonths(1)
    Fragment.const(
      s"CREATE TABLE IF NOT EXISTS $name PARTITION OF event_log " +
      s"FOR VALUES FROM ('${fmt.format(from)}') TO ('${fmt.format(until)}')"
    ).update.run.transact(xa).unit
  }
}
