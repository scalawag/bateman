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

package org.scalawag.bateman.json.generic.encoding

import org.scalawag.bateman.json.{JObject, JObjectEncoder}
import org.scalawag.bateman.json.generic.encoding.HListEncoderFactory.Input
import shapeless.HList

/** An encoder that can turn the generic HList representation of a case class and turn it into a JObject.
  *
  * @tparam A the generic representation of the case class
  * @tparam Defaults the generic representation of the case class defaults (as [[Option]]s)
  * @tparam Annotations the generic representations annotations of the field annotations (as HLists)
  */
trait HListEncoder[A <: HList, Defaults <: HList, Annotations <: HList] extends JObjectEncoder[Input[A]]
