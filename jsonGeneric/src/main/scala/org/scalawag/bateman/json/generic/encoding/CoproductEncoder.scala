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

import org.scalawag.bateman.json.{JAny, JObject}
import shapeless.Coproduct

import scala.reflect.ClassTag

trait CoproductEncoder[In <: Coproduct] {
  def encode(input: In, discriminators: JObject): JObject
  def discriminatorValues: Map[JAny, List[ClassTag[_]]]
}
