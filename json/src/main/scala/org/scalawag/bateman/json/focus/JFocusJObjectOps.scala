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
import cats.data.NonEmptyChain
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.lens.CreatableJLens

/** Extends [[JFocus]] with methods that can be used when the JSON value in focus is strong (it's parentage is
  * known) and the value is a [[JObject]]. The operations all return strong foci.
  */

class JFocusJObjectOps[A <: JFocus[JObject]](me: A) {

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

  def modify(magnet: JFocusJObjectOps.ModifyMagnet[A]): magnet.Out = magnet(me)

  def append[B: JAnyEncoder](name: String, value: B)(implicit replacer: ValueReplacer.Aux[JObject, A, A]): A =
    replacer(me.value.append(name, value.toJAny), me)

  def prepend[B: JAnyEncoder](name: String, value: B)(implicit replacer: ValueReplacer.Aux[JObject, A, A]): A =
    replacer(me.value.prepend(name, value.toJAny), me)

  def updated[B: JAnyEncoder](index: Int, value: B)(implicit replacer: ValueReplacer.Aux[JObject, A, A]): A =
    replacer(me.value.updated(index, value.toJAny), me)

  def insert[B: JAnyEncoder](index: Int, name: String, value: B)(implicit
      replacer: ValueReplacer.Aux[JObject, A, A]
  ): A =
    replacer(me.value.insert(index, name, value.toJAny), me)

  def insert(index: Int, field: JField)(implicit replacer: ValueReplacer.Aux[JObject, A, A]): A =
    replacer(me.value.insert(index, field), me)

  def ++(that: JObject)(implicit replacer: ValueReplacer.Aux[JObject, A, A]): A =
    replacer(me.value ++ that, me)

  def delete(index: Int)(implicit replacer: ValueReplacer.Aux[JObject, A, A]): A =
    replacer(me.value.delete(index), me)

  def overwriteTo[B <: JAny, C <: JAny, D: JAnyEncoder](
      lens: CreatableJLens[B, C],
      value: D,
      prepend: Boolean = false
  )(implicit replacer: ValueReplacer.Aux[JObject, A, A]): A = {
    val result = new JFocusWeakOps(JRootFocus(me.value)).overwriteTo(lens, value, prepend)
    result.root.value match {
      case o: JObject => replacer(o, me)
      case other => throw new IllegalStateException(s"expected JObject but got ${other.jType}")
    }
  }

  def writeTo[B <: JAny, C <: JAny, D, E <: JAny](
      lens: CreatableJLens[B, C],
      value: D,
      prepend: Boolean = false
  )(implicit enc: Encoder[D, E], replacer: ValueReplacer.Aux[JObject, A, A]): JResult[A] =
    new JFocusWeakOps(JRootFocus(me.value))
      .writeTo(lens, value, prepend)
      .map(fb => fb.root.value match {
        case o: JObject => replacer(o, me)
        case other => throw new IllegalStateException(s"expected JObject but got ${other.jType}")
      })
}

object JFocusJObjectOps {
  trait ModifyMagnet[A <: JFocus[JObject]] {
    type Out
    def apply(focus: A): Out
  }

  object ModifyMagnet extends JFocusJObjectModifyMagnetLowPriority {
    implicit def fallible[A <: JFocus[JObject]](fn: JObject => JResult[JObject])(implicit
        replacer: ValueReplacer.Aux[JObject, A, A]
    ): ModifyMagnet[A] { type Out = JResult[A] } =
      new ModifyMagnet[A] {
        type Out = JResult[A]
        def apply(focus: A): JResult[A] = fn(focus.value).map(replacer(_, focus))
      }
  }

  trait JFocusJObjectModifyMagnetLowPriority {
    implicit def pure[A <: JFocus[JObject]](fn: JObject => JObject)(implicit
        replacer: ValueReplacer.Aux[JObject, A, A]
    ): ModifyMagnet[A] { type Out = A } =
      new ModifyMagnet[A] {
        type Out = A
        def apply(focus: A): A = replacer(fn(focus.value), focus)
      }
  }
}
