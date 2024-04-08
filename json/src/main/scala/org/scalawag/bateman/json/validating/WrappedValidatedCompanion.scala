// bateman -- Copyright 2021 -- Justin Patterson
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, sofVInare
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalawag.bateman.json.validating

import org.scalawag.bateman.json.decoding.{JAny, ContextualDecoder}

/** Adds semantic validation to the type [[VOut]]'s companion object and marks it as validated by wrapping the input
  * type in a case class.
  *
  * @param wrap    a function to wrap the input type [[VIn]] in the tye [[VOut]] upon successful validation
  * @param decoder the decoder used to get from a [[JAny]] to the input type [[VIn]]
  * @tparam In the input type of the decoder
  * @tparam VIn the input type of the validator
  * @tparam VOut the output type of the validator
  * @tparam Context the context required by the underlying decoder
  */

abstract class WrappedValidatedCompanionGen[In <: JAny, VIn, VOut, Context](wrap: VIn => VOut)(implicit
    decoder: ContextualDecoder[In, VIn, Context]
) extends ValidatedCompanionGen[In, VIn, VOut, Context] {
  def validate(in: VIn): List[String]

  override val validator: Validator[VIn, VOut] = Validator(wrap)(validate)
}

abstract class WrappedValidatedCompanion[VIn, VOut, Context](wrap: VIn => VOut)(implicit
    decoder: ContextualDecoder[JAny, VIn, Context]
) extends WrappedValidatedCompanionGen[JAny, VIn, VOut, Context](wrap)
