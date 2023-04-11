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

package org.scalawag.bateman.json.generic.encoding

import org.scalawag.bateman.json.JObjectEncoder
import org.scalawag.bateman.json.generic.{DiscriminatorCollision, Discriminators, MemberLabels, TraitDeriverParams}
import shapeless.{Coproduct, Generic}

trait TraitEncoderFactory[Trait] {
  def apply(params: TraitDeriverParams[JObjectEncoder]): JObjectEncoder[Trait]
}

object TraitEncoderFactory {

  /** Generates an encoder for the specified sealed trait.
    *
    * @param gen produces the labelled generic representation of our input instance
    * @param encoder generates an encoder that can encode our generic representation
    * @tparam Trait the trait to generate an encoder for
    * @tparam Gen the generic representation of [[Trait]]
    * @return the generated encoder
    */

  implicit def traitEncoder[Trait, Gen <: Coproduct](implicit
      gen: Generic.Aux[Trait, Gen],
      genericEncoderFactory: CoproductEncoderFactory[Gen]
  ): TraitEncoderFactory[Trait] =
    params => {
      val genericEncoder = genericEncoderFactory(params)

      if (params.discriminator.duplicateValuesForbidden)
        DiscriminatorCollision.detect(genericEncoder.discriminatorValues)

      (instance, discriminators) => {
        genericEncoder.encode(gen.to(instance), discriminators)
      }
    }
}
