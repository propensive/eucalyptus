package eucalyptus

import java.text.SimpleDateFormat
import scala.reflect._, macros._
import language.experimental.macros, language.higherKinds

object `package` {
  implicit final val Stdout: Destination[String] = System.out.print(_)
  implicit final val Stderr: Destination[String] = System.err.print(_)
  implicit final val Raw: Format[String, String] = (msg, _, _, _, _, _) => msg
  
  implicit object Standard extends Format[String, String] {
    private def pad(str: String, length: Int) =
      if(str.length < length) str+(" "*(length - str.length)) else str.take(length)
   
    private val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS ")
    
    def entry(msg: String, tag: Tag, timestamp: Long, level: Level, lineNo: Int,
        source: String): String = {
      val sb = new StringBuilder()
      sb.append(dateFormat.format(timestamp))
      sb.append(level.name)
      sb.append(' ')
      sb.append(pad(tag.name, 10))
      sb.append(' ')
      sb.append(pad(s"$source:$lineNo", 15))
      sb.append(' ')
      sb.append(msg)
      sb.append('\n')
      sb.toString
    }
  }
}

case class Engine[Msg, Out](loggers: Logger[Msg, Out]*) {
  def trace(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def debug(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def audit(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def issue(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def error(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
  def fault(msg: Msg)(implicit tag: Tag): Unit = macro Macros.doLog
}

final case class Level(value: Int) extends AnyVal { final def name: String = Level.names(value) }

object Level {
  final val List(trace, debug, audit, issue, error, fault) = List.range(0, 6).map(Level(_))
  
  private[eucalyptus] final val names: Array[String] =
    Array("TRACE", "DEBUG", "AUDIT", "ISSUE", "ERROR", "FAULT")
}

trait Destination[T] { def consume(msg: T): Unit }

trait Format[-Msg, +Out] {
  def entry(msg: Msg, tag: Tag, timestamp: Long, level: Level, lineNo: Int, source: String): Out
}

final case class Logger[Msg, Out](min: Level, max: Level, tagSet: Set[Tag],
    format: Format[Msg, Out], destination: Destination[Out]) {

  final def above(level: Level): Logger[Msg, Out] = copy(level, max, tagSet, format, destination)
  final def below(level: Level): Logger[Msg, Out] = copy(min, level, tagSet, format, destination)
}

object Tag { implicit final val Default = Tag("") }

object Log {
  def apply(tags: Tag*): LogTags = LogTags(tags.to[Set])

  case class LogTags(tagSet: Set[Tag]) {
    def as[Msg, Out](format: Format[Msg, Out]): LogTagsFormat[Msg, Out] =
      LogTagsFormat(tagSet, format)
  }

  case class LogTagsFormat[Msg, Out](tagSet: Set[Tag], format: Format[Msg, Out]) {
    def to(destination: Destination[Out]): Logger[Msg, Out] =
      Logger(Level.trace, Level.fault, tagSet, format, destination)
  }
}

final case class Tag(name: String) extends AnyVal

object Macros {
  def doLog(c: blackbox.Context)(msg: c.Tree)(tag: c.Tree): c.Tree = {
    import c._, universe._
    val lineNo = enclosingPosition.line
    val source = enclosingPosition.source.toString
    val q"$base.$method($_)($_)" = macroApplication // "
    val levelInt = Level.names.indexOf(method.toString.toUpperCase)
    val level = q"_root_.eucalyptus.Level($levelInt)"
    val now = q"_root_.java.lang.System.currentTimeMillis"
    
    q"""$base.loggers.foreach { log =>
          if($levelInt >= log.min.value && $levelInt <= log.max.value && log.tagSet.contains($tag))
            log.destination.consume(log.format.entry($msg, $tag, $now, $level, $lineNo, $source))
        }"""
  }
}
