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

package org.scalawag.bateman.jsonapi.generic.decoding

import org.scalawag.bateman.json.{JAny, JResult}
import org.scalawag.bateman.json.generic.decoding.CoproductDecoderFactory.Input
import shapeless.Coproduct

import scala.reflect.ClassTag

/** Represents an automatically-derived [[Coproduct]] decoder. A [[CoproductResourceDecoder]] is essentially a delegate
  * decoder (called the headDecoder) and a fallback decoder to try if the headDecoder is not appropriate, based
  * on the discriminator value (called the tailDecoder). This fallback itself is another [[CoproductResourceDecoder]],
  * so the structure is recursive. Usually, the delegate decoder will be concrete, but that's not necessarily the case
  * (i.e., when there are multiple levels of discriminators being used).
  *
  * Beginning at the head and working to the tail, the actual discriminator value is compared against the one in
  * the input until a discriminator match is made. At that point, the search stops and the delegate is used to
  * decode, whether it works or not. If the search reaches the end of the structure without finding a discriminator
  * match, an invalid discriminator is reported. This error uses the data gathered during the traversal in the
  * Input object to report discriminator values that _would_ have been decoded had they been used.
  *
  * @tparam A the target type of the decoder (a trait)
  */

trait CoproductResourceDecoder[A <: Coproduct] {

  /** Decodes the input to an instance of type [[A]].
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def decode(input: Input): JResult[A]

  /** Returns a map of discriminator values to [[ClassTag]]s that can be decoded by this decoder. */
  def discriminatorValues: Map[JAny, List[ClassTag[_]]]
}
