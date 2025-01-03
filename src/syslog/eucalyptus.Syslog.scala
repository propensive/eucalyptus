/*
    Eucalyptus, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import guillotine.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

case class Syslog(tag: Optional[Text] = Unset)

object Syslog:
  given (using Monitor) => Syslog is Writable by Text as appendable = (syslog, stream) =>
    import workingDirectories.default

    mend:
      case StreamError(_)     => ()
      case ExecError(_, _, _) => ()

    . within:
        syslog.tag match
          case tag: Text => mute[ExecEvent](stream.writeTo(sh"logger -t $tag".fork[Unit]()))
          case _         => mute[ExecEvent](stream.writeTo(sh"logger".fork[Unit]()))
