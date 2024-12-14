/*
    Eucalyptus, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import contingency.*, strategies.throwUnsafely
import escapade.*
import fulminate.*
import gossamer.*
import parasite.*
import probably.*
import spectacular.*
import turbulence.*, stdioSources.virtualMachine

object Tests extends Suite(t"Eucalyptus tests"):
  def run(): Unit =
    import Level.*
    given LogFormat[Out.type, Display] = logFormats.standardColor[Out.type]
    given LogFormat[Err.type, Display] = logFormats.standardColor[Err.type]

    supervise:
      given Log[Display] = Log.route[Display]:
        case Warn() => Out
        case Fail() => Out
        case _      => Out//Syslog(t"tab")

      test(t"Log something"):
        given realm: Realm = realm"test"
        Log.fine(t"Fine message")
        Log.info(t"Info message")
        Log.warn(t"Warn message")
        Log.fail(t"Fail message")
      .assert()
      Thread.sleep(1000L)
