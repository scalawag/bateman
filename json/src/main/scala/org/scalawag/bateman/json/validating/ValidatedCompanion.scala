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

import cats.data.ValidatedNec
import org.scalawag.bateman.json.decoding.{ContextualDecoder, Decoder, JAny}

/** Designed to be extended by the companion objects for types that needs to be semantically validated. It provides
  * an implicit [[Validator]], an implicit [[Decoder]] and factory methods. There must already be a [[Decoder]]
  * available for the [[In]] type to the [[VIn]] type.
  *
  * @param dec a decoder for turning a JAny to the input type for the validator
  * @tparam In the input type of the decoder
  * @tparam VIn the input type of the validator
  * @tparam VOut the output type of the validator
  */

abstract class ValidatedCompanionGen[In <: JAny, -VIn, VOut, Context](implicit
    dec: ContextualDecoder[In, VIn, Context]
) {

  /** A validator that must be provided by the companion object. */
  implicit val validator: Validator[VIn, VOut]

  /** Creates an [[VOut]] from an [[VIn]] after validating it.
    *
    * @param in the value to be validated
    * @return a valid [[VOut]] ''or'' a list of validation failures
    */
  def apply(in: VIn): ValidatedNec[ValidationFailure, VOut] = validator.validate(in)

  /** Creates an [[VOut]] from an [[VIn]] after validating it.
    *
    * @param in the value to be validated
    * @return the validated [[VOut]]
    * @throws ValidationFailedException when validation fails
    */
  def unsafe(in: VIn): VOut = apply(in).fold(ValidationFailure.throwValidationErrors, identity)

  /** A decoder that decodes a [[JAny]] to an instance of [[VOut]] by using the implicit decoder and then running
    * the result through the validation defined by [[validator]].
    */
  implicit def decoder: ContextualDecoder[In, VOut, Context] = dec.withValidation[VOut]
}

abstract class ValidatedCompanion[-VIn, VOut, Context](implicit dec: ContextualDecoder[JAny, VIn, Context])
    extends ValidatedCompanionGen[JAny, VIn, VOut, Context]
