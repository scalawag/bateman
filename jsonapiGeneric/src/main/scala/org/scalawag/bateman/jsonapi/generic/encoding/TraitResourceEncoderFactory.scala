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

package org.scalawag.bateman.jsonapi.generic.encoding

import org.scalawag.bateman.json.generic.TraitDeriverParams
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import shapeless.{Coproduct, Generic}

trait TraitResourceEncoderFactory[In] {
  def apply(params: TraitDeriverParams[ResourceEncoder]): ResourceEncoder[In]
}

object TraitResourceEncoderFactory {

  implicit def traitEncoder[Trait, Gen <: Coproduct](implicit
      gen: Generic.Aux[Trait, Gen],
      genericEncoderFactory: CoproductResourceEncoderFactory[Gen],
  ): TraitResourceEncoderFactory[Trait] = { params =>
    val genericEncoder = genericEncoderFactory(params)

    (in, includeSpec, fieldsSpec, discriminators) =>
      genericEncoder.encodeResource(gen.to(in), includeSpec, fieldsSpec, discriminators)
  }
}
