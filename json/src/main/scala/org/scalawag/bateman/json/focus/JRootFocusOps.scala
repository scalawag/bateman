// bateman -- Copyright 2021-2026 -- Justin Patterson
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalawag.bateman.json.focus

import org.scalawag.bateman.json._

class JRootFocusOps[InValue <: JAny](me: JRootFocus[InValue]) {
  def modify[O <: JAny](fn: InValue => O): JRootFocus[O] = JRootFocus(fn(me.value))

  def modify[O <: JAny](fn: InValue => JResult[O])(implicit d: DummyImplicit): JResult[JRootFocus[O]] =
    fn(me.value).map(JRootFocus(_))

  def modifyFocus[O <: JAny](fn: JRootFocus[InValue] => O): JRootFocus[O] =
    JRootFocus(fn(me))

  def modifyFocus[O <: JAny](fn: JRootFocus[InValue] => JResult[O])(implicit d: DummyImplicit): JResult[JRootFocus[O]] =
    fn(me).map(JRootFocus(_))

  def replace[O <: JAny](newValue: O): JRootFocus[O] = JRootFocus(newValue)

  def replace[O <: JAny](newValue: JResult[O])(implicit d: DummyImplicit): JResult[JRootFocus[O]] =
    newValue.map(JRootFocus(_))
}
