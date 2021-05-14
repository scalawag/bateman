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

import org.scalawag.bateman.json.decoding.{JAny, JAnyContextualDecoder}

/** Adds semantic validation to the type [[Out]]'s companion object and marks it as validated by wrapping the input
  * type in a case class.
  *
  * @param wrap a function to wrap the input type [[In]] in the tye [[Out]] upon successful validation
  * @param decoder the decoder used to get from a [[JAny]] to the input type [[In]]
  * @tparam In the input type of the validator
  * @tparam Out the output type of the validator
  * @tparam Context the context required by the underlying decoder
  */

abstract class WrappedValidatedCompanion[In, Out, Context](wrap: In => Out)(implicit
    decoder: JAnyContextualDecoder[In, Context]
) extends ValidatedCompanion[In, Out, Context] {
  def validate(in: In): List[String]

  override val validator: Validator[In, Out] = Validator(wrap)(validate)
}
