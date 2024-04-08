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

package org.scalawag.bateman.json.validating

import org.scalawag.bateman.json.decoding.{JAny, ContextualDecoder}

/** Adds semantic validation to the empty trait [[Trait]]'s companion object that marks values as validated by
  * mixing in the empty with their type.
  *
  * @param dec the decoder used to get from a [[JAny]] to the input type [[VIn]]
  * @tparam In the input type of the decoder
  * @tparam VIn the input type of the validator
  * @tparam Trait the empty trait to be mixed in with the input type on successful validation
  * @tparam Context the context required by the underlying decoder
  */
abstract class EmptyTraitValidatedCompanionGen[In <: JAny, VIn, Trait, Context](implicit
    dec: ContextualDecoder[In, VIn, Context]
) extends ValidatedCompanionGen[In, VIn, VIn with Trait, Context] {
  def validate(in: VIn): List[String]

  override val validator: Validator[VIn, VIn with Trait] =
    Validator[VIn, VIn with Trait]((_: VIn).asInstanceOf[VIn with Trait])(validate)
}

abstract class EmptyTraitValidatedCompanion[VIn, Trait, Context](implicit d: ContextualDecoder[JAny, VIn, Context])
    extends EmptyTraitValidatedCompanionGen[JAny, VIn, Trait, Context]
