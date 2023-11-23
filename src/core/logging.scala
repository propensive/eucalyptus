/*
    Eucalyptus, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package eucalyptus

import gossamer.*
import rudiments.*
import fulminate.*
import anticipation.*
import perforate.*
import parasite.*
import turbulence.*
import spectacular.*
import hieroglyph.*

import scala.quoted.*

//import language.experimental.captureChecking

object Realm:
  given Show[Realm] = _.name
  def make(name: Text)(using Unsafe): Realm = Realm(name)

case class Realm private(name: Text):
  def unapply(entry: Entry[?]): Boolean = entry.realm == this

object Level:
  given Ordering[Level] = Ordering[Int].on[Level](_.ordinal)
  given Communicable[Level] = level => Message(level.show.upper)
  given Show[Level] = _.toString.tt.upper

enum Level:
  case Fine, Info, Warn, Fail
  def unapply(entry: Entry[?]): Boolean = entry.level == this
  
case class Entry[TextType](realm: Realm, level: Level, message: TextType, timestamp: Long, envelopes: List[Text])

object Envelope:
  given [EnvelopeType](using Show[EnvelopeType]): Envelope[EnvelopeType] = _.show

trait Envelope[-EnvelopeType]:
  def envelope(value: EnvelopeType): Text

  given silent: Log[Text] = entry => ()

object Logger:
  def drain[AnyType]: Logger[AnyType] = stream => ()

  def apply
      [TargetType, TextType]
      (target: TargetType, appendable: Appendable[TargetType, TextType], format: LogFormat[TargetType, TextType])
      (using monitor: Monitor)
      : Logger[TextType]/*^{monitor}*/ =
    LiveLogger(target)(using format)(using appendable)

object LogWriter:
  given active
      [TargetType, TextType]
      (using format: LogFormat[TargetType, TextType])
      (using appendable: Appendable[TargetType, TextType], monitor: Monitor)
      : LogWriter[TargetType, TextType]/*^{monitor}*/ =
    LiveLogger[TargetType, TextType](_)(using format)(using appendable, monitor)

trait LogWriter[TargetType, TextType]:
  def logger(target: TargetType): Logger[TextType]

trait Logger[TextType]:
  def put(entry: Entry[TextType]): Unit

class LiveLogger
    [TargetType, TextType]
    (target: TargetType)
    (using format: LogFormat[TargetType, TextType])
    (using appendable: Appendable[TargetType, TextType], monitor: Monitor)
extends Logger[TextType]:
  private val funnel: Funnel[Entry[TextType]] = Funnel()
  
  private val async: Async[Unit] = Async:
    appendable.append(target, unsafely(funnel.stream.map(format(_))))
  
  def put(entry: Entry[TextType]): Unit = funnel.put(entry)

object LogFormat:
  given standard[TargetType]: LogFormat[TargetType, Text] = entry =>
    import textWidthCalculation.uniform
    val realm: Message = msg"${entry.realm.show.fit(8)}"
    msg"${Log.dateFormat.format(entry.timestamp).nn.tt} ${entry.level} $realm ${entry.message}".text+t"\n"
  
trait LogFormat[TargetType, TextType]:
  def apply(entry: Entry[TextType]): TextType

extension (inline context: StringContext)
  inline def realm(): Realm = ${Eucalyptus.realm('context)}

package logging:

  given pinned: Log[Text] = Log.pinned

  given stdout(using Stdio, Monitor): Log[Text] = Log.route[Text]:
    case _ => Out

  given stderr(using Stdio, Monitor): Log[Text] = Log.route[Text]:
    case _ => Err

  given silent: Log[Text] = new Log:
    def record(entry: Entry[Text]): Unit = ()
