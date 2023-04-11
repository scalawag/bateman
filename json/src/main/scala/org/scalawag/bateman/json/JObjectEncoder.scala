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

/** This is its own trait (and not just an alias) because it provides the ability to pass in an object containing
  * encoded discriminators.
  */
trait JObjectEncoder[In] extends Encoder[In, JObject] {

  override def encode(in: In): JObject = encode(in, JObject())

  /** Gives a containing encoder the ability to pass in the discriminator fields that it has used to arrive at the
    * current encoder so that those fields can be included in the output and used in detecting programming errors.
    */
  def encode(in: In, discriminators: JObject): JObject
}

object JObjectEncoder extends EncoderAliasCompanion[JObject, JObjectEncoder]
