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

package org.scalawag.bateman.json

import org.scalawag.bateman.json.focus.JFocus

/** This is its own trait (and not just an alias) because it provides the ability to pass in
  * a set of fields that have already been used as discriminators, so that they will not be
  * flagged as "unexpected" when decoding.
  */
trait JObjectDecoder[To] extends Decoder[JObject, To] {

  override def decode(in: JFocus[JObject]): JResult[To] = decode(in, Set.empty)

  /** Gives a containing decoder the ability to pass in the lenses of the discriminator fields that
    * it has used so that those field can be excluded from the extraneous fields error checking.
    */
  def decode(in: JFocus[JObject], discriminatorFields: Set[JFocus[JAny]]): JResult[To]
}

object JObjectDecoder extends DecoderAliasCompanion[JObject]
