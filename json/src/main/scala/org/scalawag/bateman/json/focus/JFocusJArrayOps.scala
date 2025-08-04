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
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._

/** Extends [[JFocus]] with methods that can be used when the JSON value in focus is strong (it's parentage is
  * known) and the value is a [[JArray]]. The operations all return strong foci.
  */

class JFocusJArrayOps[A <: JFocus[JArray]](me: A) {

  /** Refocuses on the items of the focused JSON array, one focus per item. */
  def items: List[JItemFocus[JAny, A]] =
    me.value.items.zipWithIndex.map {
      case (item, n) =>
        JItemFocus(item, n, me)
    }

  /** Refocuses on the specified item of the focused JSON array.
    * If there is an item at the specified index, a focus to it is returned, wrapped in a [[scala.Some Some]].
    * If the index is out-of-bounds, [[scala.None None]] is returned.
    */
  def itemOption(index: Int): Option[JItemFocus[JAny, A]] =
    items.lift(index)

  /** Refocuses on the specified item of the focused JSON array. If the index is out-of-bounds, an error is returned. */
  def item(index: Int): JResult[JItemFocus[JAny, A]] =
    itemOption(index) match {
      case Some(x) => x.rightNec
      case None    => MissingIndex(me, index).leftNec
    }

  def modify(magnet: JFocusJArrayOps.ModifyMagnet[A]): magnet.Out = magnet(me)

  def append[B: JAnyEncoder](item: B)(implicit replacer: ValueReplacer.Aux[JArray, A, A]): A =
    replacer(me.value.append(item.toJAny), me)

  def prepend[B: JAnyEncoder](item: B)(implicit replacer: ValueReplacer.Aux[JArray, A, A]): A =
    replacer(me.value.prepend(item.toJAny), me)

  def updated[B: JAnyEncoder](index: Int, value: B)(implicit replacer: ValueReplacer.Aux[JArray, A, A]): A =
    replacer(me.value.updated(index, value.toJAny), me)

  def insert[B: JAnyEncoder](index: Int, item: B)(implicit replacer: ValueReplacer.Aux[JArray, A, A]): A =
    replacer(me.value.insert(index, item.toJAny), me)

  def ++(that: JArray)(implicit replacer: ValueReplacer.Aux[JArray, A, A]): A =
    replacer(me.value ++ that, me)

  def delete(index: Int)(implicit replacer: ValueReplacer.Aux[JArray, A, A]): A =
    replacer(me.value.delete(index), me)
}

object JFocusJArrayOps {
  trait ModifyMagnet[A <: JFocus[JArray]] {
    type Out
    def apply(focus: A): Out
  }

  object ModifyMagnet extends JFocusJArrayModifyMagnetLowPriority {
    implicit def fallible[A <: JFocus[JArray]](fn: JArray => JResult[JArray])(implicit
        replacer: ValueReplacer.Aux[JArray, A, A]
    ): ModifyMagnet[A] { type Out = JResult[A] } =
      new ModifyMagnet[A] {
        type Out = JResult[A]
        def apply(focus: A): JResult[A] = fn(focus.value).map(replacer(_, focus))
      }
  }

  trait JFocusJArrayModifyMagnetLowPriority {
    implicit def pure[A <: JFocus[JArray]](fn: JArray => JArray)(implicit
        replacer: ValueReplacer.Aux[JArray, A, A]
    ): ModifyMagnet[A] { type Out = A } =
      new ModifyMagnet[A] {
        type Out = A
        def apply(focus: A): A = replacer(fn(focus.value), focus)
      }
  }
}
