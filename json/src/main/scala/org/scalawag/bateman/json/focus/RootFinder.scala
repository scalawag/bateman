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

trait RootFinder[InFocus <: JStrongFocus[JAny]] {
  type Root
  def apply(in: InFocus): Root
}

object RootFinder {
  def apply[InFocus <: JStrongFocus[JAny]](b: InFocus)(implicit root: RootFinder[InFocus]): root.Root =
    root(b)

  type Aux[InFocus <: JStrongFocus[JAny], Root0] = RootFinder[InFocus] { type Root = Root0 }

  implicit def rootForRootLens[Value <: JAny]: Aux[JRootFocus[Value], JRootFocus[Value]] =
    new RootFinder[JRootFocus[Value]] {
      type Root = JRootFocus[Value]
      override def apply(in: JRootFocus[Value]): Root = in
    }

  implicit def rootForFieldLens[Value <: JAny, ParentFocus <: JStrongFocus[JObject], ParentRoot](implicit
      parentRoot: RootFinder.Aux[ParentFocus, ParentRoot]
  ): Aux[JFieldFocus[Value, ParentFocus], ParentRoot] =
    new RootFinder[JFieldFocus[Value, ParentFocus]] {
      type Root = ParentRoot
      override def apply(in: JFieldFocus[Value, ParentFocus]): ParentRoot = parentRoot(in.parent)
    }

  implicit def rootForItemLens[Value <: JAny, ParentFocus <: JStrongFocus[JArray], ParentRoot](implicit
      parentRoot: RootFinder.Aux[ParentFocus, ParentRoot]
  ): Aux[JItemFocus[Value, ParentFocus], ParentRoot] =
    new RootFinder[JItemFocus[Value, ParentFocus]] {
      type Root = ParentRoot
      override def apply(in: JItemFocus[Value, ParentFocus]): ParentRoot = parentRoot(in.parent)
    }
}
