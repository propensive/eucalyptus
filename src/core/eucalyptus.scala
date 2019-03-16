/*
  
  Eucalyptus, version 1.0.0. Copyright 2019 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at
  
      http://www.apache.org/licenses/LICENSE-2.0
 
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

*/
package eucalyptus

import scala.reflect._, macros._
import language.experimental.macros

object Log {
  val interval: Long = 100L
  private var continue: Boolean = true
  private var loggers: Vector[Logger[_]] = Vector()

  private[this] val thread: Thread = new Thread() {
    override def run(): Unit = {
      while(continue) {
        val t0 = System.currentTimeMillis
        loggers.foreach(_.processUpdates())
        Thread.sleep(interval - System.currentTimeMillis + t0)
      }

      join()
    }
  }
  thread.setDaemon(true)
  thread.start()

  def apply[T, Msg](routes: Route[Msg]*): Logger[Msg] = {
    val logger: Logger[Msg] = new Logger(routes: _*)
    synchronized { loggers = loggers :+ logger }
    logger
  }

  def borrow[T, Msg](routes: Route[Msg]*)(block: Logger[Msg] => T): T = {
    val logger: Logger[Msg] = new Logger(routes: _*)
    synchronized { loggers = loggers :+ logger }
    try block(logger) finally synchronized { loggers = loggers.filter(_ != logger) }
  }

  def shutdown(): Unit = continue = false

  class Logger[Msg] private[eucalyptus] (routes: Route[Msg]*) {

    private[this] var buffer: Vector[Log[Msg]] = Vector()
    private[this] var closed: Boolean = false
    
    def trace(msg: Msg): Unit = macro Macros.doLog
    def debug(msg: Msg): Unit = macro Macros.doLog
    def check(msg: Msg): Unit = macro Macros.doLog
    def alert(msg: Msg): Unit = macro Macros.doLog
    def error(msg: Msg): Unit = macro Macros.doLog
    def fatal(msg: Msg): Unit = macro Macros.doLog

    def log(msg: Log[Msg]): Unit = synchronized { buffer = buffer :+ msg }

    def shutdown(): Unit = closed = true

    private[eucalyptus] def updates(): Vector[Log[Msg]] = synchronized {
      val snapshot = buffer
      buffer = Vector()
      snapshot
    }
    
    private[eucalyptus] def processUpdates(): Unit = updates().foreach { msg =>
      routes.foreach(_.process(msg))
    }
  }

  object Level {
    final val Trace = Level(0)
    final val Debug = Level(1)
    final val Check = Level(2)
    final val Alert = Level(3)
    final val Error = Level(4)
    final val Fatal = Level(5)

    private[eucalyptus] final val names: Array[String] =
      Array("TRACE", "DEBUG", "CHECK", "ALERT", "ERROR", "FATAL")
  }

  final case class Level(value: Int) extends AnyVal { final def name: String = Level.names(value) }

  implicit val ordering: Ordering[Log[_]] = Ordering[Long].on(_.timestamp)
}

case class Log[Msg](timestamp: Long, level: Log.Level, message: Msg, source: String, lineNo: Int)

case class Route[Msg](handle: Log[Msg] => Unit, min: Log.Level = Log.Level.Trace, max: Log.Level = Log.Level.Fatal) {
  private[eucalyptus] def process(msg: Log[Msg]): Unit = if(min.value <= msg.level.value && max.value >= msg.level.value) handle(msg)
}

object Macros {
  def doLog(c: blackbox.Context)(msg: c.Tree): c.Tree = {
    import c._, universe._
    
    val lineNo = enclosingPosition.line
    val source = enclosingPosition.source.toString
    val q"$base.$method($_)" = macroApplication // "
    val levelInt = Log.Level.names.indexOf(method.toString.toUpperCase)
    val level = q"_root_.eucalyptus.Log.Level($levelInt)"
    val now = q"_root_.java.lang.System.currentTimeMillis"
    
    q"$base.log(_root_.eucalyptus.Log($now, $level, $msg, $source, $lineNo))"
  }
}
