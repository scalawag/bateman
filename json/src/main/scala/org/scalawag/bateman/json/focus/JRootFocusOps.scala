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
import org.scalawag.bateman.json.lens.{CreatableJLens, JLens}

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

  def modifyF[OutValue <: JAny](fn: JRootFocus[InValue] => JResult[OutValue])(implicit
      replacer: ValueReplacer[OutValue, JRootFocus[InValue]],
  ): JResult[replacer.NewFocus] = fn(me).map(replacer(_, me))

  def modify[OutValue <: JAny](fn: JRootFocus[InValue] => OutValue)(implicit
      replacer: ValueReplacer[OutValue, JRootFocus[InValue]],
  ): replacer.NewFocus = replacer(fn(me), me)

  def modifyValueF[OutValue <: JAny](fn: InValue => JResult[OutValue])(implicit
      replacer: ValueReplacer[OutValue, JRootFocus[InValue]],
  ): JResult[replacer.NewFocus] = fn(me.value).map(replacer(_, me))

  def modifyValue[OutValue <: JAny](fn: InValue => OutValue)(implicit
      replacer: ValueReplacer[OutValue, JRootFocus[InValue]],
  ): replacer.NewFocus = replacer(fn(me.value), me)

  /** Returns a decoded representation of value in focus. */
  def decode[Out](implicit dec: Decoder[InValue, Out]): JResult[Out] = dec.decode(me)

  def root: JRootFocus[InValue] = me
}
