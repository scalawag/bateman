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

/** Extends [[JFocus]] with methods that can be used when the JSON value in focus is strong (it's parentage is
  * known) and the value is an item within a [[JArray]]. The operations all return strong foci.
  */

class JItemFocusOps[A <: JAny, P <: JFocus[JArray]](me: JItemFocus[A, P]) {
  def delete()(implicit replacer: ValueReplacer.Aux[JArray, P, P]): P =
    replacer(me.parent.value.delete(me.index), me.parent)

  def modify[O <: JAny](fn: A => O)(implicit
      replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
  ): JItemFocus[O, P] = replacer(fn(me.value), me)

  def modify[O <: JAny](fn: A => JResult[O])(implicit
      d: DummyImplicit,
      replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
  ): JResult[JItemFocus[O, P]] = fn(me.value).map(replacer(_, me))

  def modifyFocus[O <: JAny](fn: JItemFocus[A, P] => O)(implicit
      replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
  ): JItemFocus[O, P] = replacer(fn(me), me)

  def modifyFocus[O <: JAny](fn: JItemFocus[A, P] => JResult[O])(implicit
      d: DummyImplicit,
      replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
  ): JResult[JItemFocus[O, P]] = fn(me).map(replacer(_, me))

  def replace[O <: JAny](newValue: O)(implicit
      replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
  ): JItemFocus[O, P] = replacer(newValue, me)

  def replace[O <: JAny](newValue: JResult[O])(implicit
      d: DummyImplicit,
      replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
  ): JResult[JItemFocus[O, P]] = newValue.map(replacer(_, me))
}
