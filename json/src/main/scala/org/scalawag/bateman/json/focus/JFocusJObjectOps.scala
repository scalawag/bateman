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
import cats.data.NonEmptyChain
import org.scalawag.bateman.json._

/** Extends [[JStrongFocus]] with methods that can be used when the JSON value in focus is a [[JObject]]. */
class JFocusJObjectOps[A <: JStrongFocus[JObject]](me: A) {

  /** Refocuses on the values of the fields of the focused JSON object.
    *
    * @return a list of foci, one per field
    */
  def fields: List[JFieldFocus[JAny, A]] =
    me.value.fieldList.zipWithIndex.map {
      case (JField(k, v), n) =>
        JFieldFocus(v, k, n, me)
    }

  /** Refocuses on the values of the fields of the focused JSON object with the specified name.
    *
    * @return a list of foci, one per matching field
    */
  def fields(name: String): List[JFieldFocus[JAny, A]] =
    fields.collect { case cur if cur.name.value == name => cur }

  /** Refocuses on the value of the single field of the focused JSON object with the specified name.
    * If there is a single matching field, a focus to its value is returned, wrapped in a [[Some]].
    * If there is no matching field, [[None]] is returned.
    * If there are multiple matching fields, an error is returned.
    *
    * @return an optional focus to the value of the single matching field
    */
  def fieldOption(name: String): JResult[Option[JFieldFocus[JAny, A]]] =
    fields(name) match {
      case Nil       => None.rightNec
      case List(cur) => Some(cur).rightNec
      // TODO: Maybe list all duplicates here?
      case curs =>
        DuplicateField(me, NonEmptyChain.fromSeq(curs).get).leftNec
    }

  /** Refocuses on the value of the single field of the focused JSON object with the specified name.
    * If there is a single matching field, a focus to its value is returned. Otherwise, an error is returned.
    *
    * @return a focus to the value of the single matching field
    */

  def field(name: String): JResult[JFieldFocus[JAny, A]] =
    fieldOption(name).flatMap {
      case None        => MissingField(me, name).leftNec
      case Some(field) => field.rightNec
    }

  /** Refocuses on the value of the field of the focused JSON object at the specified index.
    * If the index is outside the bounds of the fields for this JSON object, an error is returned.
    *
    * @return a focus to the value of the field with the specified index
    */

  def field(index: Int): JResult[JFieldFocus[JAny, A]] =
    fields.lift(index) match {
      case None        => MissingFieldIndex(me, index).leftNec
      case Some(field) => field.rightNec
    }

  // TODO: add support for modification here once we can support the modify* calls in a strongly-typed way.
//  def append(name: String, value: JAny)(implicit valueFinder: ValueFinder.Aux[A, JObject]): A =
//    me.modifyValue(_.append(name, value))

//  def prepend(name: String, value: JAny)(implicit valueFinder: ValueFinder.Aux[A, JObject]): A =
//    me.modifyValue(_.prepend(item))

}
