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
  * known) and the value is an item within a [[JArray]]. The operations all return strong foci.
  */

class JItemFocusOps[A <: JAny, P <: JFocus[JArray]](me: JItemFocus[A, P]) {
  def narrow[B <: JAny: ClassTag: Summoner]: JResult[JItemFocus[B, P]] =
    me.value match {
      case b: B => JItemFocus(b, me.index, me.parent).rightNec
      case _    => JsonTypeMismatch(me, JType[B]).leftNec
    }

  def asNull: JResult[JItemFocus[JNull, P]] = narrow[JNull]
  def asArray: JResult[JItemFocus[JArray, P]] = narrow[JArray]
  def asObject: JResult[JItemFocus[JObject, P]] = narrow[JObject]
  def asString: JResult[JItemFocus[JString, P]] = narrow[JString]
  def asNumber: JResult[JItemFocus[JNumber, P]] = narrow[JNumber]
  def asBoolean: JResult[JItemFocus[JBoolean, P]] = narrow[JBoolean]

  def delete()(implicit replacer: ValueReplacer.Aux[JArray, P, P]): P =
    replacer(me.parent.value.delete(me.index), me.parent)

  def modify(magnet: JItemFocusOps.ModifyMagnet[A, P]): magnet.Out = magnet(me)

  def root(implicit rootFinder: RootFinder[P]): rootFinder.Root = rootFinder(me.parent)
}

object JItemFocusOps {
  trait ModifyMagnet[A <: JAny, P <: JFocus[JArray]] {
    type Out
    def apply(focus: JItemFocus[A, P]): Out
  }

  object ModifyMagnet extends JItemFocusModifyMagnetLowPriority {
    implicit def focusFallible[A <: JAny, P <: JFocus[JArray], O <: JAny](
        fn: JItemFocus[A, P] => JResult[O]
    )(implicit
        replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JResult[JItemFocus[O, P]] } =
      new ModifyMagnet[A, P] {
        type Out = JResult[JItemFocus[O, P]]
        def apply(focus: JItemFocus[A, P]): JResult[JItemFocus[O, P]] = fn(focus).map(replacer(_, focus))
      }

    implicit def valueFallible[A <: JAny, P <: JFocus[JArray], O <: JAny](
        fn: A => JResult[O]
    )(implicit
        replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JResult[JItemFocus[O, P]] } =
      new ModifyMagnet[A, P] {
        type Out = JResult[JItemFocus[O, P]]
        def apply(focus: JItemFocus[A, P]): JResult[JItemFocus[O, P]] = fn(focus.value).map(replacer(_, focus))
      }
  }

  trait JItemFocusModifyMagnetLowPriority {
    implicit def focusPure[A <: JAny, P <: JFocus[JArray], O <: JAny](
        fn: JItemFocus[A, P] => O
    )(implicit
        replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JItemFocus[O, P] } =
      new ModifyMagnet[A, P] {
        type Out = JItemFocus[O, P]
        def apply(focus: JItemFocus[A, P]): JItemFocus[O, P] = replacer(fn(focus), focus)
      }

    implicit def valuePure[A <: JAny, P <: JFocus[JArray], O <: JAny](
        fn: A => O
    )(implicit
        replacer: ValueReplacer.Aux[O, JItemFocus[A, P], JItemFocus[O, P]]
    ): ModifyMagnet[A, P] { type Out = JItemFocus[O, P] } =
      new ModifyMagnet[A, P] {
        type Out = JItemFocus[O, P]
        def apply(focus: JItemFocus[A, P]): JItemFocus[O, P] = replacer(fn(focus.value), focus)
      }
  }
}
