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

trait FocusRebuilder[Focus <: JStrongFocus[JAny]] {
  def apply(root: JAny, focus: Focus): JResult[Focus]
}

object FocusRebuilder {

  /** Creates a new focus with the same path as the specified focus in the specified document.
    * This will not fail unless the structure of the new document is different than the structure of the old one.
    *
    * Note: You may have to call .toAny on your focus to get one whose focus is JAny, depending on why you're
    * recreating the focus. The ancestors of that value should match the correct focus type due to the path requiring
    * objects or arrays at each step. For the leaf value, though, all bets are off. For this reason (and because this
    * returns the same focus type as is passed in), it may require that you widen the focus type of the focus you pass
    * in.
    */
  def apply[Root <: JAny, Focus <: JStrongFocus[JAny]](root: Root, focus: Focus)(implicit
      rebuilder: FocusRebuilder[Focus],
  ): JResult[Focus] =
    rebuilder(root, focus)

  implicit def rebuildFocusForRootFocus: FocusRebuilder[JRootFocus[JAny]] =
    (root, _) => JRootFocus(root).rightNec

  implicit def rebuildFocusForFieldFocus[Parent <: JStrongFocus[JObject]](implicit
      parentRebuilder: FocusRebuilder[Parent]
  ): FocusRebuilder[JFieldFocus[JAny, Parent]] =
    (root, focus) => parentRebuilder(root, focus.parent).flatMap(_.field(focus.index))

  implicit def rebuildFocusForItemFocus[Parent <: JStrongFocus[JArray]](implicit
      parentRebuilder: FocusRebuilder[Parent]
  ): FocusRebuilder[JItemFocus[JAny, Parent]] =
    (root, focus) => parentRebuilder(root, focus.parent).flatMap(_.item(focus.index))
}
