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

package org.scalawag.bateman.jsonapi.generic.encoding

import cats.syntax.monoid._
import org.scalawag.bateman.json.generic.{Config, MemberLabels, TraitInfo}
import org.scalawag.bateman.jsonapi.encoding.{ResourceEncoder, ResourceLike}
import org.scalawag.bateman.jsonapi.generic.DetectResourceTypeCollisions
import org.scalawag.bateman.jsonapi.generic.encoding.CoproductResourceEncoderFactoryFactory.Params
import shapeless.{:+:, CNil, Coproduct, Generic, Lazy}

trait TraitResourceEncoder[In, +Out <: ResourceLike] extends ResourceEncoder[In, Out]

object TraitResourceEncoder {
  def apply[In, Out <: ResourceLike](implicit enc: TraitResourceEncoder[In, Out]): TraitResourceEncoder[In, Out] = enc
}

trait TraitResourceEncoderFactory[In, Out <: ResourceLike] {
  def apply(config: Config): TraitResourceEncoder[In, Out]
}

object TraitResourceEncoderFactory {

  implicit def traitEncoder[Out <: ResourceLike, Trait, Gen <: Coproduct](implicit
      gen: Generic.Aux[Trait, Gen],
      subclasses: MemberLabels[Trait],
      concreteResourceTypes: ConcreteEncoderResourceTypes[Gen, Out],
      genericEncoderFactoryFactory: CoproductResourceEncoderFactoryFactory[Gen, Out],
  ): TraitResourceEncoderFactory[Trait, Out] = {
    val info = TraitInfo(subclasses())
    val genericEncoderFactory = genericEncoderFactoryFactory(info)

    DetectResourceTypeCollisions("encoders", concreteResourceTypes(info))

    config => {
      val params = Params(config)
      val genericEncoder = genericEncoderFactory(params)

      (in, includeSpec, fieldsSpec) => {
        genericEncoder.encodeResource(gen.to(in), includeSpec, fieldsSpec)
      }
    }
  }

  // This type class is used to collect all the discriminators from the concrete encoders used by a TraitEncoder.
  trait ConcreteEncoderResourceTypes[A <: Coproduct, Out <: ResourceLike] {
    def apply(info: TraitInfo): Map[String, List[String]]
  }

  object ConcreteEncoderResourceTypes {
    implicit def forCNil[Out <: ResourceLike]: ConcreteEncoderResourceTypes[CNil, Out] = { _ => Map.empty }

    implicit def forCCons[InHead, InTail <: Coproduct, Out <: ResourceLike](implicit
        headEncoder: Lazy[CaseClassResourceEncoder[InHead, Out]],
        tailDiscriminatorValues: ConcreteEncoderResourceTypes[InTail, Out]
    ): ConcreteEncoderResourceTypes[InHead :+: InTail, Out] = { info =>
      val tailDiscriminators = tailDiscriminatorValues(info.tail)
      Map(headEncoder.value.resourceType -> List(info.subclassNames.head)) combine tailDiscriminators
    }
  }
}
