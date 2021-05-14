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

import cats.data.NonEmptyChain
import cats.syntax.validated._
import org.scalawag.bateman.json.validIfEmpty

/** Validates an instance of type [[In]], returning either a list of [[ValidationFailure]]s of, if there are no
  * failures, an instance of type [[Out]]. This is intentionally not specific to decoding, which already has a way
  * to represent the structure of the value being decoded. It allows factory methods for validated types to return
  * deep validation failures (through [[ValidationFailure]]) while
  *
  * @tparam In the input type to be validated
  * @tparam Out the output upon successful validation
  */
trait Validator[-In, +Out] {

  /** Validates an input value, transforming it to an output value.
    *
    * @param in the input value
    * @return either a list of failures or an instance of type [[Out]].
    */
  def validate(in: In): ValidationResult[Out]
}

object Validator {

  /** Summons a validator by type from implicit scope. */
  def apply[In, Out](implicit validator: Validator[In, Out]): Validator[In, Out] = validator

  /** Creates a validator from a transformation function and a validation function.
    *
    * @param transform turns an [[In]] to an [[Out]] upon successful validation
    * @param validate returns a list of validation failure messages for the input value (empty means it's valid)
    * @tparam In the input type
    * @tparam Out the output type
    * @return a validator that validates an [[In]] and produces an [[Out]] using the specified functions
    */
  def apply[In, Out](transform: In => Out)(validate: In => List[String]): Validator[In, Out] = { in =>
    val errors = validate(in).map(ValidationFailure(_))
    validIfEmpty(errors, transform(in))
  }
}
