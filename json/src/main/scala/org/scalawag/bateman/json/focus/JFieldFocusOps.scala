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

class JFieldFocusOps[A <: JAny, P <: JStrongFocus[JObject]](me: JFieldFocus[A, P]) {
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
//    def asAny: JFieldFocus[JAny, A] = me.copy(value = me.value: JAny)

  def previous: JResult[JFieldFocus[JAny, P]] = me.parent.field(me.index - 1)
  def next: JResult[JFieldFocus[JAny, P]] = me.parent.field(me.index + 1)
  def first: JFieldFocus[JAny, P] = me.parent.fields.head
  def last: JFieldFocus[JAny, P] = me.parent.fields.last

  /** Returns a decoded representation of value in focus. */
  def decode[Out](implicit dec: Decoder[A, Out]): JResult[Out] = dec.decode(me)

  def root(implicit rootFinder: RootFinder[P]): rootFinder.Root = rootFinder(me.parent)
}
