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

package org.scalawag.bateman.json.validating

/** Adds semantic validation to the type `Out`'s companion object and marks it as validated by wrapping the input
  * type in a case class.
  *
  * @param wrap a function to wrap the input type `In` in the tye `Out` upon successful validation
  * @tparam In the input type of the validator
  * @tparam Out the output type of the validator
  */

abstract class WrappedValidatedCompanion[In, Out](wrap: In => Out) extends ValidatedCompanion[In, Out] {
  def validate(in: In): List[String]

  override val validator: Validator[In, Out] = Validator(wrap)(validate)
}
