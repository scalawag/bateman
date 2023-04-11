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

package org.scalawag.bateman.json.focus.weak

import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.{JFocus, JItemFocus}

class JFocusJArrayWeakOps(me: JFocus[JArray]) {
  def items: List[JFocus[JAny]] = me.value.items.zipWithIndex.map { case (item, n) => JItemFocus(item, n, me) }

  def itemOption(index: Int): Option[JFocus[JAny]] = items.lift(index)

  def item(index: Int): JResult[JFocus[JAny]] =
    itemOption(index) match {
      case Some(x) => x.rightNec
      case None    => MissingIndex(me, index).leftNec
    }

  def append(item: JAny): JFocus[JArray] = me.modifyValue(_.append(item))

  def prepend(item: JAny): JFocus[JArray] = me.modifyValue(_.prepend(item))
}
