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

/** Adds semantic validation (that marks values as validated by mixing in the empty trait with the original type)
  * to the empty trait's companion object.
  *
  * @tparam In the input type of the validator
  * @tparam Trait the empty trait to be mixed in with the input type on successful validation
  */
trait EmptyTraitValidatedCompanion[In, Trait] extends ValidatedCompanion[In, In with Trait] {
  def validate(in: In): List[String]

  override val validator: Validator[In, In with Trait] =
    Validator[In, In with Trait]((_: In).asInstanceOf[In with Trait])(validate)
}
