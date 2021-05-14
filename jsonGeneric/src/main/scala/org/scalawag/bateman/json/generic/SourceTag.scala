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

import org.scalawag.bateman.json.decoding.JObject

/** Marks a case class field for source injection. This means that, instead of pulling the value from a field of the
  * input JSON object, the JSON object itself is injected into the field. All fields with this tag should be of type
  * [[Option]][ [[JObject]] ] or else you will not be able to generate a [[Decoder]].
  */

sealed trait SourceTag extends Tag
object SourceTag extends TagCompanion[SourceTag] with SourceTag
