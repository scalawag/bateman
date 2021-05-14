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
import org.scalawag.bateman.json.decoding.{ContextualDecoder, Decoder, JAny, JAnyDecoder}

/** Designed to be extended by the companion objects for types that needs to be semantically validated. It provides
  * an implicit [[Validator]], an implicit [[Decoder]] and factory methods. There must already be a [[JAnyDecoder]]
  * available for the [[In]] type.
  *
  * @param dec a decoder for turning a JAny to the input type for the validator
  * @tparam In the input type of the validator
  * @tparam Out the output type of the validator
  */

abstract class ValidatedCompanion[-In, Out, Context](implicit dec: ContextualDecoder[JAny, In, Context]) {

  /** A validator that must be provided by the companion object. */
  implicit val validator: Validator[In, Out]

  /** Creates an [[Out]] from an [[In]] after validating it.
    *
    * @param in the value to be validated
    * @return a valid [[Out]] ''or'' a list of validation failures
    */
  def apply(in: In): ValidatedNec[ValidationFailure, Out] = validator.validate(in)

  /** Creates an [[Out]] from an [[In]] after validating it.
    *
    * @param in the value to be validated
    * @return the validated [[Out]]
    * @throws ValidationFailedException when validation fails
    */
  def unsafe(in: In): Out = apply(in).fold(ValidationFailure.throwValidationErrors, identity)

  /** A decoder that decodes a [[JAny]] to an instance of [[Out]] by using the implicit decoder and then running
    * the result through the validation defined by [[validator]].
    */
  implicit def decoder: ContextualDecoder[JAny, Out, Context] = dec.withValidation[Out]
}
