// bateman -- Copyright 2021-2023 -- Justin Patterson
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

class JItemFocusOps[A <: JAny, P <: JStrongFocus[JArray]](me: JItemFocus[A, P]) {
  def left: JResult[JItemFocus[JAny, P]] = me.parent.item(me.index - 1)
  def right: JResult[JItemFocus[JAny, P]] = me.parent.item(me.index + 1)
  def first: JItemFocus[JAny, P] = me.parent.items.head
  def last: JItemFocus[JAny, P] = me.parent.items.last

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
//    def asAny: JItemFocus[JAny, A] = me.copy(value = me.value: JAny)

  /** Returns a decoded representation of value in focus. */
  def decode[Out](implicit dec: Decoder[A, Out]): JResult[Out] = dec.decode(me)

  def root(implicit rootFinder: RootFinder[P]): rootFinder.Root = rootFinder(me.parent)
}
