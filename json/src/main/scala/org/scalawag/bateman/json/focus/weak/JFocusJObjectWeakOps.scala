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

package org.scalawag.bateman.json.focus.weak

import cats.syntax.either._
import cats.data.NonEmptyChain
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.{JFieldFocus, JFocus}

class JFocusJObjectWeakOps(me: JFocus[JObject]) {

  def fields: List[JFocus[JAny]] =
    me.value.fieldList.zipWithIndex.map {
      case (f, n) =>
        JFieldFocus(f.value, f.name, n, me)
    }

  def fields(name: String): List[JFocus[JAny]] =
    fields.collect { case f: JFieldFocus[JAny, _] if f.name.value == name => f }

  def fieldOption(name: String): JResult[Option[JFocus[JAny]]] =
    fields(name) match {
      case Nil     => None.rightNec
      case List(f) => Some(f).rightNec
      case ff      => DuplicateField(me, NonEmptyChain.fromSeq(ff).get).leftNec
    }

  def field(name: String): JResult[JFocus[JAny]] =
    fieldOption(name).flatMap {
      case None        => MissingField(me, name).leftNec
      case Some(field) => field.rightNec
    }

  def field(index: Int): JResult[JFocus[JAny]] =
    fields.lift(index) match {
      case None        => MissingFieldIndex(me, index).leftNec
      case Some(field) => field.rightNec
    }

  def append(name: String, value: JAny): JFocus[JObject] = me.modify(_.value.append(name, value))

  def prepend(name: String, value: JAny): JFocus[JObject] = me.modify(_.value.prepend(name, value))
}
