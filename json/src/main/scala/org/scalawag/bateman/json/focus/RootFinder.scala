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

trait RootFinder[InFocus <: JFocus[JAny]] {
  type Root
  def apply(in: InFocus): Root
}

object RootFinder extends RootFinderLowPriority {
  def apply[InFocus <: JFocus[JAny]](b: InFocus)(implicit root: RootFinder[InFocus]): root.Root =
    root(b)

  type Aux[InFocus <: JFocus[JAny], Root0] = RootFinder[InFocus] { type Root = Root0 }

  implicit def rootForRootFocus[Value <: JAny]: Aux[JRootFocus[Value], JRootFocus[Value]] =
    new RootFinder[JRootFocus[Value]] {
      type Root = JRootFocus[Value]
      override def apply(in: JRootFocus[Value]): Root = in
    }

  implicit def rootForFieldFocus[Value <: JAny, ParentFocus <: JFocus[JObject], ParentRoot](implicit
      parentRoot: RootFinder.Aux[ParentFocus, ParentRoot]
  ): Aux[JFieldFocus[Value, ParentFocus], ParentRoot] =
    new RootFinder[JFieldFocus[Value, ParentFocus]] {
      type Root = ParentRoot
      override def apply(in: JFieldFocus[Value, ParentFocus]): ParentRoot = parentRoot(in.parent)
    }

  implicit def rootForItemFocus[Value <: JAny, ParentFocus <: JFocus[JArray], ParentRoot](implicit
      parentRoot: RootFinder.Aux[ParentFocus, ParentRoot]
  ): Aux[JItemFocus[Value, ParentFocus], ParentRoot] =
    new RootFinder[JItemFocus[Value, ParentFocus]] {
      type Root = ParentRoot
      override def apply(in: JItemFocus[Value, ParentFocus]): ParentRoot = parentRoot(in.parent)
    }
}

trait RootFinderLowPriority {

  /** Fallback for abstract focus types where the specific instances can't match.
    * Walks the parent chain at runtime, returning [[JFocus]].
    */
  implicit def rootForAnyFocus[F <: JFocus[JAny]]: RootFinder.Aux[F, JFocus[JAny]] =
    new RootFinder[F] {
      type Root = JFocus[JAny]
      override def apply(in: F): JFocus[JAny] = {
        @scala.annotation.tailrec
        def go(f: JFocus[JAny]): JFocus[JAny] =
          f match {
            case r: JRootFocus[_]     => r
            case c: JChildFocus[_, _] => go(c.parent)
          }
        go(in)
      }
    }
}
