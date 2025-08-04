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

// These extension methods are needed in Scala 3 because the magnet pattern used by modify requires an implicit
// conversion on the argument (fn -> ModifyMagnet). When modify is accessed through an implicit class conversion
// (e.g., toJFieldFocusOps), Scala 3 cannot chain two implicit conversions (receiver + argument). Direct extension
// methods avoid this by only requiring the argument conversion.

extension [InValue <: JAny](me: JRootFocus[InValue])
  def modify(magnet: JRootFocusOps.ModifyMagnet[InValue]): magnet.Out = magnet(me)

extension [A <: JAny, P <: JFocus[JObject]](me: JFieldFocus[A, P])
  def modify(magnet: JFieldFocusOps.ModifyMagnet[A, P]): magnet.Out = magnet(me)

extension [A <: JAny, P <: JFocus[JArray]](me: JItemFocus[A, P])
  def modify(magnet: JItemFocusOps.ModifyMagnet[A, P]): magnet.Out = magnet(me)

extension [A <: JFocus[JObject]](me: A)
  def modify(magnet: JFocusJObjectOps.ModifyMagnet[A]): magnet.Out = magnet(me)

extension [A <: JFocus[JArray]](me: A)
  def modify(magnet: JFocusJArrayOps.ModifyMagnet[A]): magnet.Out = magnet(me)