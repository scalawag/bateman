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
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.weak._
import scala.annotation.tailrec

trait ValueReplacer[-NewValue <: JAny, -OldFocus <: JStrongFocus[JAny]] {
  type NewFocus <: JStrongFocus[JAny]
  def apply(value: NewValue, focus: OldFocus): NewFocus
}

object ValueReplacer extends ValueReplacerLowP {
  def apply[NewValue <: JAny, OldFocus <: JStrongFocus[JAny]](value: NewValue, focus: OldFocus)(implicit
      replacer: ValueReplacer[NewValue, OldFocus]
  ): replacer.NewFocus =
    replacer(value, focus)

  type Aux[A <: JAny, B <: JStrongFocus[JAny], Out <: JStrongFocus[JAny]] = ValueReplacer[A, B] { type NewFocus = Out }

  implicit def valueReplacerForRootFocus[OldValue <: JAny, NewValue <: JAny]
      : ValueReplacer.Aux[NewValue, JRootFocus[OldValue], JRootFocus[NewValue]] =
    new ValueReplacer[NewValue, JRootFocus[OldValue]] {
      override type NewFocus = JRootFocus[NewValue]
      override def apply(newValue: NewValue, oldFocus: JRootFocus[OldValue]): NewFocus = JRootFocus(newValue)
    }

  implicit def valueReplacerForFieldFocus[OldValue <: JAny, NewValue <: JAny, ParentFocus <: JStrongFocus[
    JObject
  ], NewParentFocus <: JStrongFocus[JAny]]
      : ValueReplacer.Aux[NewValue, JFieldFocus[OldValue, ParentFocus], JFieldFocus[NewValue, ParentFocus]] =
    new ValueReplacer[NewValue, JFieldFocus[OldValue, ParentFocus]] {
      override type NewFocus = JFieldFocus[NewValue, ParentFocus]

      override def apply(value: NewValue, focus: JFieldFocus[OldValue, ParentFocus]): NewFocus = {
        replace(value, focus).getOrThrow.asInstanceOf[NewFocus]
      }
    }

  implicit def valueReplacerForItemFocus[OldValue <: JAny, NewValue <: JAny, ParentFocus <: JStrongFocus[JArray]]
      : ValueReplacer.Aux[NewValue, JItemFocus[OldValue, ParentFocus], JItemFocus[NewValue, ParentFocus]] =
    new ValueReplacer[NewValue, JItemFocus[OldValue, ParentFocus]] {
      override type NewFocus = JItemFocus[NewValue, ParentFocus]

      override def apply(value: NewValue, focus: JItemFocus[OldValue, ParentFocus]): NewFocus = {
        replace(value, focus).getOrThrow.asInstanceOf[NewFocus]
      }
    }

}

trait ValueReplacerLowP {

  /** Handles the case where there's no detailed information about the incoming focus by returning similarly
    * a vague focus in response.
    */
  implicit def valueReplacerForWeakFocus[OldValue <: JAny, NewValue <: JAny]
      : ValueReplacer.Aux[NewValue, JStrongFocus[OldValue], JStrongFocus[NewValue]] =
    new ValueReplacer[NewValue, JStrongFocus[OldValue]] {
      override type NewFocus = JStrongFocus[NewValue]

      override def apply(value: NewValue, focus: JStrongFocus[OldValue]): NewFocus = {
        replace(value, focus).getOrThrow.asInstanceOf[NewFocus]
      }
    }

  /** This only uses the path information from the focus (because focus info could be out of date. */
  @tailrec
  private def rebuildDocument(value: JAny, focus: JFocus[JAny]): JAny =
    focus match {
      case l: JRootFocus[_] =>
        value
      case l: JItemFocus[_, _] =>
        rebuildDocument(l.parent.value.copy(items = l.parent.value.items.updated(l.index, value)), l.parent)
      case l: JFieldFocus[_, _] =>
        val newField = l.parent.value.fieldList(l.index).copy(value = value)
        rebuildDocument(l.parent.value.copy(fieldList = l.parent.value.fieldList.updated(l.index, newField)), l.parent)
    }

  /** Creates a new focus with the same path as the specified focus in the specified document.
    * This will not fail unless the structure of the new document is different than the structure of the old one.
    */
  private def rebuildFocus(root: JAny, focus: JFocus[JAny]): JResult[JFocus[JAny]] =
    focus match {
      case l: JRootFocus[_] =>
        JRootFocus(root).rightNec
      case l: JItemFocus[_, _] =>
        rebuildFocus(root, l.parent).flatMap(_.asArray.flatMap(_.item(l.index)))
      case l: JFieldFocus[_, _] =>
        rebuildFocus(root, l.parent).flatMap(_.asObject.flatMap(_.field(l.index)))
    }

  // A non-type-safe version for situations that require it.
  def replace(value: JAny, focus: JFocus[JAny]): JResult[JFocus[JAny]] =
    rebuildFocus(rebuildDocument(value, focus), focus)
}
