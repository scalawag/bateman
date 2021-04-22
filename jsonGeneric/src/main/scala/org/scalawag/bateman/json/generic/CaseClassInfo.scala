// bateman -- Copyright 2021 -- Justin Patterson
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

package org.scalawag.bateman.json.generic

import shapeless.tag.@@
import shapeless.{::, HList, tag}

case class CaseClassInfo[Defaults <: HList](defaults: Defaults, fieldNames: List[String])

object CaseClassInfo {

  /** Adds the ability to easily get the tail of a [[CaseClassInfo]]. Practically, this means that the defaults
    * are the tail of the original's defaults and the field names are the tail of the original's field names.
    * This is only available when the defaults HList is not empty. Note that there's no static type checking against
    * the length of `fieldNames`, but in our usage, it should always correspond to the length of `defaults`.
    */
  implicit class CaseClassInfoOps[H, T <: HList](in: CaseClassInfo[H :: T]) {
    def tail: CaseClassInfo[T] = in.copy(defaults = in.defaults.tail, fieldNames = in.fieldNames.tail)
  }

  implicit class OptionCaseClassInfoOps[H, Tg <: Tag, T <: HList](in: CaseClassInfo[Option[Option[H] @@ Tg] :: T]) {
    def flattenHead: CaseClassInfo[Option[H @@ Tg] :: T] = {
      val newHead =
        in.defaults.head match {
          case Some(outer) =>
            (outer: Option[H]) match {
              case Some(inner) => Some(tag[Tg](inner))
              case None        => None
            }
          case None => None
        }
      in.copy(defaults = newHead :: in.defaults.tail)
    }

    def flattenHeadOrNone: CaseClassInfo[Option[H @@ Tg] :: T] = {
      val newHead: Option[H @@ Tg] =
        in.defaults.head match {
          case Some(outer) =>
            (outer: Option[H]) match {
              case Some(inner) => Some(tag[Tg](inner))
              case None        => None
            }
          case None =>
            // In the case where there's no default specified, act as though None was specified.
            None
        }
      in.copy(defaults = newHead :: in.defaults.tail)
    }
  }
}
