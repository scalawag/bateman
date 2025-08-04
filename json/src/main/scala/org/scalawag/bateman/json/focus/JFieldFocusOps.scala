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

import cats.syntax.either._
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json._

import scala.reflect.ClassTag

/** Extends [[JFocus]] with methods that can be used when the JSON value in focus is strong (it's parentage is
  * known) and the value is the value of a field within a [[JObject]]. The operations all return strong foci.
  */

class JFieldFocusOps[A <: JAny, P <: JFocus[JObject]](me: JFieldFocus[A, P]) {
  def narrow[B <: JAny: ClassTag: Summoner]: JResult[JFieldFocus[B, P]] =
    me.value match {
      case b: B => JFieldFocus(b, me.name, me.index, me.parent).rightNec
      case _    => JsonTypeMismatch(me, JType[B]).leftNec
    }

  def asNull: JResult[JFieldFocus[JNull, P]] = narrow[JNull]
  def asArray: JResult[JFieldFocus[JArray, P]] = narrow[JArray]
  def asObject: JResult[JFieldFocus[JObject, P]] = narrow[JObject]
  def asString: JResult[JFieldFocus[JString, P]] = narrow[JString]
  def asNumber: JResult[JFieldFocus[JNumber, P]] = narrow[JNumber]
  def asBoolean: JResult[JFieldFocus[JBoolean, P]] = narrow[JBoolean]

  def delete()(implicit replacer: ValueReplacer.Aux[JObject, P, P]): P =
    replacer(me.parent.value.delete(me.index), me.parent)

  def modify(magnet: JFieldFocusOps.ModifyMagnet[A, P]): magnet.Out = magnet(me)

  def root(implicit rootFinder: RootFinder[P]): rootFinder.Root = rootFinder(me.parent)
}

object JFieldFocusOps {
  trait ModifyMagnet[A <: JAny, P <: JFocus[JObject]] {
    type Out
    def apply(focus: JFieldFocus[A, P]): Out
  }

  object ModifyMagnet extends JFieldFocusModifyMagnetLowPriority {
    implicit def focusFallible[A <: JAny, P <: JFocus[JObject], O <: JAny](
        fn: JFieldFocus[A, P] => JResult[O]
    )(implicit
        replacer: ValueReplacer.Aux[O, JFieldFocus[A, P], JFieldFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JResult[JFieldFocus[O, P]] } =
      new ModifyMagnet[A, P] {
        type Out = JResult[JFieldFocus[O, P]]
        def apply(focus: JFieldFocus[A, P]): JResult[JFieldFocus[O, P]] = fn(focus).map(replacer(_, focus))
      }

    implicit def valueFallible[A <: JAny, P <: JFocus[JObject], O <: JAny](
        fn: A => JResult[O]
    )(implicit
        replacer: ValueReplacer.Aux[O, JFieldFocus[A, P], JFieldFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JResult[JFieldFocus[O, P]] } =
      new ModifyMagnet[A, P] {
        type Out = JResult[JFieldFocus[O, P]]
        def apply(focus: JFieldFocus[A, P]): JResult[JFieldFocus[O, P]] = fn(focus.value).map(replacer(_, focus))
      }
  }

  trait JFieldFocusModifyMagnetLowPriority {
    implicit def focusPure[A <: JAny, P <: JFocus[JObject], O <: JAny](
        fn: JFieldFocus[A, P] => O
    )(implicit
        replacer: ValueReplacer.Aux[O, JFieldFocus[A, P], JFieldFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JFieldFocus[O, P] } =
      new ModifyMagnet[A, P] {
        type Out = JFieldFocus[O, P]
        def apply(focus: JFieldFocus[A, P]): JFieldFocus[O, P] = replacer(fn(focus), focus)
      }

    implicit def valuePure[A <: JAny, P <: JFocus[JObject], O <: JAny](
        fn: A => O
    )(implicit
        replacer: ValueReplacer.Aux[O, JFieldFocus[A, P], JFieldFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JFieldFocus[O, P] } =
      new ModifyMagnet[A, P] {
        type Out = JFieldFocus[O, P]
        def apply(focus: JFieldFocus[A, P]): JFieldFocus[O, P] = replacer(fn(focus.value), focus)
      }
  }
}
