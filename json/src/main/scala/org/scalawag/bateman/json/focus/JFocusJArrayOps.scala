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

class JFocusJArrayOps[A <: JStrongFocus[JArray]](me: A) {
  def items: List[JItemFocus[JAny, A]] =
    me.value.items.zipWithIndex.map {
      case (item, n) =>
        JItemFocus(item, n, me)
    }

  def itemOption(index: Int): Option[JItemFocus[JAny, A]] =
    items.lift(index)

  def item(index: Int): JResult[JItemFocus[JAny, A]] =
    itemOption(index) match {
      case Some(x) => x.rightNec
      case None    => MissingIndex(me, index).leftNec
    }
}
