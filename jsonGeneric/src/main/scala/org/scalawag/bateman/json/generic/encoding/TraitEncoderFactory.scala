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

package org.scalawag.bateman.json.generic.encoding

import cats.syntax.monoid._
import org.scalawag.bateman.json.OptionLike
import org.scalawag.bateman.json.encoding.{Encoder, JObject, JObjectEncoder}
import org.scalawag.bateman.json.generic.encoding.CoproductEncoderFactoryFactory.Params
import org.scalawag.bateman.json.generic.{Config, DetectDiscriminatorCollisions, MemberLabels, TraitInfo}
import shapeless.{:+:, CNil, Coproduct, Generic, Lazy}

trait TraitEncoder[Trait] extends JObjectEncoder[Trait]

trait TraitEncoderFactory[Trait] {
  def apply(discriminatorFieldOverride: OptionLike[String], config: Config): TraitEncoder[Trait]
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
      subclasses: MemberLabels[Trait],
      concreteDiscriminatorValues: ConcreteDiscriminatorValues[Gen],
      genericEncoderFactoryFactory: CoproductEncoderFactoryFactory[Gen]
  ): TraitEncoderFactory[Trait] = {
    val info = TraitInfo(subclasses())
    val genericEncoderFactory = genericEncoderFactoryFactory(info)

    DetectDiscriminatorCollisions("encoders", concreteDiscriminatorValues(info))

    (discriminatorFieldOverride, config) => {
      val params = Params(config, discriminatorFieldOverride.value)
      val genericEncoder = genericEncoderFactory(params)

      instance => {
        genericEncoder.encode(gen.to(instance))
      }
    }
  }

  // This type class is used to collect all the discriminators from the concrete encoders used by a TraitEncoder.
  trait ConcreteDiscriminatorValues[A <: Coproduct] {
    def apply(info: TraitInfo): Map[String, List[String]]
  }

  object ConcreteDiscriminatorValues {
    implicit val forCNil: ConcreteDiscriminatorValues[CNil] = { _ => Map.empty }

    implicit def forCCons[Head, Tail <: Coproduct](implicit
        headEncoder: Lazy[CaseClassEncoder[Head]],
        tailDiscriminatorValues: ConcreteDiscriminatorValues[Tail]
    ): ConcreteDiscriminatorValues[Head :+: Tail] = { info =>
      val tailDiscriminators = tailDiscriminatorValues(info.tail)
      Map(headEncoder.value.discriminatorValue -> List(info.subclassNames.head)) combine tailDiscriminators
    }
  }
}
