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

package org.scalawag.bateman.json.generic

import shapeless.{::, Default, HList}

case class CaseClassInfo[Defaults <: HList](defaults: Defaults, fieldNames: List[String])

object CaseClassInfo {
  def apply[A](implicit
      fieldNames: MemberLabels[A],
      defaults: Default.AsOptions[A]
  ): CaseClassInfo[defaults.Out] = CaseClassInfo(defaults(), fieldNames())

  implicit def forCaseClass[A, D <: HList](implicit
      fieldNames: MemberLabels[A],
      defaults: Default.AsOptions.Aux[A, D]
  ): CaseClassInfo[D] =
    CaseClassInfo(defaults(), fieldNames())

  /** Adds the ability to easily get the tail of a [[CaseClassInfo]]. Practically, this means that the defaults
    * are the tail of the original's defaults and the field names are the tail of the original's field names.
    * This is only available when the defaults HList is not empty. Note that there's no static type checking against
    * the length of `fieldNames`, but in our usage, it should always correspond to the length of `defaults`.
    */
  implicit class CaseClassInfoOps[H, T <: HList](in: CaseClassInfo[Option[H] :: T]) {
    def tail: CaseClassInfo[T] = in.copy(defaults = in.defaults.tail, fieldNames = in.fieldNames.tail)

    /** Effectively double the Some on the head if it's already Some and leave it alone if it's None. We can't just
      * blindly apply Some to the head or else it will always make it look like a default was specified.
      */
    def doubleSomeHead: CaseClassInfo[Option[Option[H]] :: T] =
      in.copy(defaults = in.defaults.head.map(Option(_)) :: in.defaults.tail)
  }
}
