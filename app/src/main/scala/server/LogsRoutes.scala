package server

import cats.effect.kernel.Ref
import fs2.Stream
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.{Charset, HttpRoutes, MediaType, ServerSentEvent}
import zio.Task
import zio.interop.catz._

import java.io.{File, RandomAccessFile}
import java.nio.charset.StandardCharsets
import scala.concurrent.duration._

object LogsRoutes {
  private val LogFile = "logs/app.log"

  private val dsl = Http4sDsl[Task]
  import dsl._

  private object TailParam extends OptionalQueryParamDecoderMatcher[Int]("tail")
  private object GrepParam extends OptionalQueryParamDecoderMatcher[String]("grep")

  val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "logs" :? TailParam(tail) +& GrepParam(grep) =>
      val n = tail.getOrElse(200).max(1).min(100000)
      for {
        lines <- zio.ZIO.attempt(tailLines(LogFile, n))
        filtered = grep.fold(lines)(g => lines.filter(_.contains(g)))
        resp <- Ok(filtered.mkString("\n"), `Content-Type`(MediaType.text.plain, Charset.`UTF-8`))
      } yield resp

    case GET -> Root / "logs" / "stream" :? GrepParam(grep) =>
      val stream = tailStream(LogFile)
        .filter(line => grep.forall(line.contains))
        .map(line => ServerSentEvent(data = Some(line)))
      Ok(stream)
  }

  private def tailLines(path: String, n: Int): List[String] = {
    val file = new File(path)
    if (!file.exists()) return List.empty
    val raf = new RandomAccessFile(file, "r")
    try {
      var acc       = Array.emptyByteArray
      var pos       = raf.length()
      val chunkSize = 8192
      var newlines  = 0
      while (pos > 0 && newlines <= n) {
        val readSize = math.min(chunkSize.toLong, pos).toInt
        pos -= readSize
        raf.seek(pos)
        val chunk = new Array[Byte](readSize)
        raf.readFully(chunk)
        acc = chunk ++ acc
        var i = 0
        while (i < readSize) {
          if (chunk(i) == '\n') newlines += 1
          i += 1
        }
      }
      val text = new String(acc, StandardCharsets.UTF_8)
      text.split('\n').toList.takeRight(n)
    } finally raf.close()
  }

  private def tailStream(path: String): Stream[Task, String] =
    Stream
      .eval(Ref.of[Task, (Long, String)]((initialPos(path), "")))
      .flatMap { ref =>
        Stream
          .awakeEvery[Task](500.millis)
          .evalMap(_ => ref.modify(readNew(path, _)))
          .flatMap(Stream.emits)
      }

  private def readNew(path: String, state: (Long, String)): ((Long, String), Vector[String]) = {
    val (pos, leftover) = state
    val file            = new File(path)
    if (!file.exists()) return (state, Vector.empty)
    val len   = file.length()
    val start = if (len < pos) 0L else pos
    if (len == start) return ((len, leftover), Vector.empty)
    val raf = new RandomAccessFile(file, "r")
    try {
      raf.seek(start)
      val buf = new Array[Byte]((len - start).toInt)
      raf.readFully(buf)
      val text  = leftover + new String(buf, StandardCharsets.UTF_8)
      val parts = text.split("\n", -1)
      ((len, parts.last), parts.init.toVector)
    } finally raf.close()
  }

  private def initialPos(path: String): Long = {
    val f = new File(path)
    if (f.exists()) f.length() else 0L
  }
}
