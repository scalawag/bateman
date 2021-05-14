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

package org.scalawag.bateman.json.generic.decoding

import cats.syntax.monoid._
import org.scalawag.bateman.json.OptionLike
import org.scalawag.bateman.json.decoding._
import org.scalawag.bateman.json.generic.{Config, DetectDiscriminatorCollisions, MemberLabels, TraitInfo}
import org.scalawag.bateman.json.generic.decoding.CoproductDecoderFactoryFactory.Params
import shapeless.{:+:, CNil, Coproduct, Generic, Lazy}

trait TraitDecoder[To, Context] extends JObjectContextualDecoder[To, Context]

trait TraitDecoderFactory[To, Context] {
  def apply(discriminatorFieldOverride: OptionLike[String], config: Config): TraitDecoder[To, Context]
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
    * @param config configures the behavior of the generated encoder
    * @tparam Trait the trait for which we're generating decoder
    * @tparam Generic the labelled generic representation of [[Trait]] as a [[Coproduct]]
    * @tparam Context the context in which the generated decoder will operate
    * @return the generated decoder
    */

  implicit def traitDecoder[Trait, Generic <: Coproduct, Context](implicit
      generic: Generic.Aux[Trait, Generic],
      subclassNames: MemberLabels[Trait],
      concreteDecoderDiscriminators: ConcreteDecoderDiscriminators[Generic, Context],
      decoderFactoryFactory: CoproductDecoderFactoryFactory[JObject, Generic, Context]
  ): TraitDecoderFactory[Trait, Context] = {
    val typeInfo = TraitInfo(subclassNames())
    val decoderFactory = decoderFactoryFactory(typeInfo)

    DetectDiscriminatorCollisions("decoders", concreteDecoderDiscriminators(typeInfo))

    (discriminatorFieldOverride, config) => {
      val params = Params(config, discriminatorFieldOverride.value)
      val decoder = decoderFactory(params)

      (in, context) => {
        val input = CoproductDecoderFactoryFactory.Input(in, context, Set.empty)
        decoder.decode(input).map(generic.from)
      }
    }
  }

  // This type class is used to collect all the discriminators from the concrete decoders used by the TraitDecoder.
  trait ConcreteDecoderDiscriminators[A <: Coproduct, Context] {
    def apply(info: TraitInfo): Map[String, List[String]]
  }

  object ConcreteDecoderDiscriminators {
    implicit val forCNil: ConcreteDecoderDiscriminators[CNil, Any] = { _ => Map.empty }

    implicit def forCCons[Head, Tail <: Coproduct, Context](implicit
        headDecoder: Lazy[CaseClassDecoder[Head, Context]],
        tailDiscriminatorValues: ConcreteDecoderDiscriminators[Tail, Context]
    ): ConcreteDecoderDiscriminators[Head :+: Tail, Context] = { info =>
      val tailDiscriminators = tailDiscriminatorValues(info.tail)
      Map(headDecoder.value.discriminatorValue -> List(info.subclassNames.head)) combine tailDiscriminators
    }
  }
}
