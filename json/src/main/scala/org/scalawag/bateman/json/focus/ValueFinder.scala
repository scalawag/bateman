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

import org.scalawag.bateman.json._

trait ValueFinder[InFocus <: JFocus[JAny]] {
  type Out <: JAny
  def apply(focus: InFocus): Out
}

object ValueFinder {
  def apply[InFocus <: JFocus[JAny]](focus: InFocus)(implicit vf: ValueFinder[InFocus]): vf.Out = vf(focus)

  type Aux[InFocus <: JFocus[JAny], Root0] = ValueFinder[InFocus] { type Out = Root0 }

  implicit def valueFinderForRootFocus[Value <: JAny]: Aux[JRootFocus[Value], Value] =
    new ValueFinder[JRootFocus[Value]] {
      type Out = Value
      override def apply(focus: JRootFocus[Value]): Value = focus.value
    }

  implicit def valueFinderForFieldFocus[Value <: JAny, ParentFocus <: JFocus[JObject]]
      : Aux[JFieldFocus[Value, ParentFocus], Value] =
    new ValueFinder[JFieldFocus[Value, ParentFocus]] {
      type Out = Value
      override def apply(focus: JFieldFocus[Value, ParentFocus]): Value = focus.value
    }

  implicit def valueFinderForItemFocus[Value <: JAny, ParentFocus <: JFocus[JArray]]
      : Aux[JItemFocus[Value, ParentFocus], Value] =
    new ValueFinder[JItemFocus[Value, ParentFocus]] {
      type Out = Value
      override def apply(focus: JItemFocus[Value, ParentFocus]): Value = focus.value
    }
}
