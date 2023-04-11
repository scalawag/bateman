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

package org.scalawag.bateman.json.generic.decoding

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.{
  Config,
  DiscriminatorCollision,
  Discriminators,
  MemberLabels,
  TraitDeriverParams
}
import shapeless.{Coproduct, Generic}

trait TraitDecoderFactory[To] {
  def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[To]
}

object TraitDecoderFactory {

  /** Generates a decoder for a trait according to the configuration specified implicitly.
    *
    * Note that the discriminator field will be stripped from the object by the generated trait decoder ''before''
    * the generated case class decoder sees it. This is only a problem when there's a name collision between one of
    * the case class fields and the discriminator, which should be caught by the macro.
    *
    * Note also that, while this function does not make use of an implicit [[Config]] object, the generated coproduct
    * decoder ([[decoder]]) that it materializes ''does''. It is the lexical scope of this call that will need to
    * have any desired configuration in scope.
    *
    * @param generic creates the labelled generic representation of [[Trait]] as a [[Coproduct]]
    * @param decoder generates a decoder that can decode the generic representation ([[Generic]])
    * @tparam Trait the trait for which we're generating decoder
    * @tparam Generic the labelled generic representation of [[Trait]] as a [[Coproduct]]
    * @return the generated decoder
    */

  implicit def traitDecoder[Trait, Generic <: Coproduct](implicit
      generic: Generic.Aux[Trait, Generic],
      decoderFactory: CoproductDecoderFactory[Generic]
  ): TraitDecoderFactory[Trait] = { params =>
    {
      val genericDecoder = decoderFactory(params)
      if (params.discriminator.duplicateValuesForbidden)
        DiscriminatorCollision.detect(genericDecoder.discriminatorValues)

      (in, discriminatorFields) =>
        in(params.discriminatorLens).flatMap { disc =>
          val input = CoproductDecoderFactory.Input(in, disc, discriminatorFields + disc, Nil)
          genericDecoder.decode(input).map(generic.from)
        }
    }
  }
}
