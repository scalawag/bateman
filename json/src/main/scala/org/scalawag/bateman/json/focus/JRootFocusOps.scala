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

class JRootFocusOps[InValue <: JAny](me: JRootFocus[InValue]) {
  def narrow[B <: JAny: ClassTag: Summoner]: JResult[JRootFocus[B]] =
    me.value match {
      case b: B => JRootFocus(b).rightNec
      case _    => JsonTypeMismatch(me, JType[B]).leftNec
    }

  def asNull: JResult[JRootFocus[JNull]] = narrow[JNull]
  def asArray: JResult[JRootFocus[JArray]] = narrow[JArray]
  def asObject: JResult[JRootFocus[JObject]] = narrow[JObject]
  def asString: JResult[JRootFocus[JString]] = narrow[JString]
  def asNumber: JResult[JRootFocus[JNumber]] = narrow[JNumber]
  def asBoolean: JResult[JRootFocus[JBoolean]] = narrow[JBoolean]

  def modify(magnet: JRootFocusOps.ModifyMagnet[InValue]): magnet.Out = magnet(me)
}

object JRootFocusOps {
  trait ModifyMagnet[V <: JAny] {
    type Out
    def apply(focus: JRootFocus[V]): Out
  }

  object ModifyMagnet extends JRootFocusModifyMagnetLowPriority {
    implicit def focusFallible[V <: JAny, O <: JAny](
        fn: JRootFocus[V] => JResult[O]
    ): ModifyMagnet[V] { type Out = JResult[JRootFocus[O]] } =
      new ModifyMagnet[V] {
        type Out = JResult[JRootFocus[O]]
        def apply(focus: JRootFocus[V]): JResult[JRootFocus[O]] = fn(focus).map(JRootFocus(_))
      }

    implicit def valueFallible[V <: JAny, O <: JAny](
        fn: V => JResult[O]
    ): ModifyMagnet[V] { type Out = JResult[JRootFocus[O]] } =
      new ModifyMagnet[V] {
        type Out = JResult[JRootFocus[O]]
        def apply(focus: JRootFocus[V]): JResult[JRootFocus[O]] = fn(focus.value).map(JRootFocus(_))
      }
  }

  trait JRootFocusModifyMagnetLowPriority {
    implicit def focusPure[V <: JAny, O <: JAny](
        fn: JRootFocus[V] => O
    ): ModifyMagnet[V] { type Out = JRootFocus[O] } =
      new ModifyMagnet[V] {
        type Out = JRootFocus[O]
        def apply(focus: JRootFocus[V]): JRootFocus[O] = JRootFocus(fn(focus))
      }

    implicit def valuePure[V <: JAny, O <: JAny](fn: V => O): ModifyMagnet[V] { type Out = JRootFocus[O] } =
      new ModifyMagnet[V] {
        type Out = JRootFocus[O]
        def apply(focus: JRootFocus[V]): JRootFocus[O] = JRootFocus(fn(focus.value))
      }
  }
}
