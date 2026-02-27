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

import org.scalawag.bateman.json._

trait ValueReplacer[-NewValue <: JAny, -OldFocus <: JFocus[JAny]] {
  type NewFocus <: JFocus[JAny]
  def apply(value: NewValue, focus: OldFocus): NewFocus
}

object ValueReplacer extends ValueReplacerLowP {
  def apply[NewValue <: JAny, OldFocus <: JFocus[JAny]](value: NewValue, focus: OldFocus)(implicit
      replacer: ValueReplacer[NewValue, OldFocus]
  ): replacer.NewFocus =
    replacer(value, focus)

  type Aux[A <: JAny, B <: JFocus[JAny], Out <: JFocus[JAny]] = ValueReplacer[A, B] { type NewFocus = Out }

  implicit def valueReplacerForRootFocus[OldValue <: JAny, NewValue <: JAny]
      : ValueReplacer.Aux[NewValue, JRootFocus[OldValue], JRootFocus[NewValue]] =
    new ValueReplacer[NewValue, JRootFocus[OldValue]] {
      override type NewFocus = JRootFocus[NewValue]
      override def apply(newValue: NewValue, oldFocus: JRootFocus[OldValue]): NewFocus = JRootFocus(newValue)
    }

  implicit def valueReplacerForFieldFocus[OldValue <: JAny, NewValue <: JAny, ParentFocus <: JFocus[JObject]](implicit
      parentReplacer: ValueReplacer.Aux[JObject, ParentFocus, ParentFocus]
  ): ValueReplacer.Aux[NewValue, JFieldFocus[OldValue, ParentFocus], JFieldFocus[NewValue, ParentFocus]] =
    new ValueReplacer[NewValue, JFieldFocus[OldValue, ParentFocus]] {
      override type NewFocus = JFieldFocus[NewValue, ParentFocus]

      override def apply(value: NewValue, focus: JFieldFocus[OldValue, ParentFocus]): NewFocus = {
        val newField = focus.parent.value.fieldList(focus.index).copy(value = value)
        val newParentValue =
          focus.parent.value.copy(fieldList = focus.parent.value.fieldList.updated(focus.index, newField))
        val newParent = parentReplacer(newParentValue, focus.parent)
        JFieldFocus(value, focus.name, focus.index, newParent)
      }
    }

  implicit def valueReplacerForItemFocus[OldValue <: JAny, NewValue <: JAny, ParentFocus <: JFocus[JArray]](implicit
      parentReplacer: ValueReplacer.Aux[JArray, ParentFocus, ParentFocus]
  ): ValueReplacer.Aux[NewValue, JItemFocus[OldValue, ParentFocus], JItemFocus[NewValue, ParentFocus]] =
    new ValueReplacer[NewValue, JItemFocus[OldValue, ParentFocus]] {
      override type NewFocus = JItemFocus[NewValue, ParentFocus]

      override def apply(value: NewValue, focus: JItemFocus[OldValue, ParentFocus]): NewFocus = {
        val newParentValue = focus.parent.value.copy(items = focus.parent.value.items.updated(focus.index, value))
        val newParent = parentReplacer(newParentValue, focus.parent)
        JItemFocus(value, focus.index, newParent)
      }
    }

}

trait ValueReplacerLowP {

  /** Handles the case where there's no detailed information about the incoming focus by returning similarly
    * a vague focus in response.
    */
  implicit def valueReplacerForWeakFocus[OldValue <: JAny, NewValue <: JAny]
      : ValueReplacer.Aux[NewValue, JFocus[OldValue], JFocus[NewValue]] =
    new ValueReplacer[NewValue, JFocus[OldValue]] {
      override type NewFocus = JFocus[NewValue]

      override def apply(value: NewValue, focus: JFocus[OldValue]): JFocus[NewValue] = {
        val newRoot = rebuildDocument(value, focus)
        focus.replicate(newRoot).as(value)
      }
    }

  /** This only uses the path information from the focus (because focus info could be out of date. */
  @scala.annotation.tailrec
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
}
