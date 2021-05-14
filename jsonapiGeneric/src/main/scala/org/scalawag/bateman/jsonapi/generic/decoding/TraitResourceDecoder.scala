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

package org.scalawag.bateman.jsonapi.generic.decoding

import cats.syntax.monoid._
import org.scalawag.bateman.json.generic.{Config, MemberLabels, TraitInfo}
import org.scalawag.bateman.jsonapi.decoding.ResourceLike
import org.scalawag.bateman.jsonapi.generic.DetectResourceTypeCollisions
import org.scalawag.bateman.jsonapi.generic.decoding.CoproductResourceDecoderFactoryFactory.{Input, Params}
import shapeless.{:+:, CNil, Coproduct, Generic, Lazy}

trait TraitResourceDecoder[In <: ResourceLike, Out] extends ResourceDecoder[In, Out]

object TraitResourceDecoder {
  def apply[In <: ResourceLike, Out](implicit dec: TraitResourceDecoder[In, Out]): TraitResourceDecoder[In, Out] = dec
}

trait TraitResourceDecoderFactory[In <: ResourceLike, Out] {
  def apply(config: Config): TraitResourceDecoder[In, Out]
}

object TraitResourceDecoderFactory {

  implicit def traitDecoder[In <: ResourceLike, Trait, Generic <: Coproduct](implicit
      generic: Generic.Aux[Trait, Generic],
      subclassNames: MemberLabels[Trait],
      concreteResourceTypes: ConcreteResourceTypes[In, Generic],
      decoderFactoryFactory: CoproductResourceDecoderFactoryFactory[In, Generic],
  ): TraitResourceDecoderFactory[In, Trait] = {
    val typeInfo = TraitInfo(subclassNames())
    val decoderFactory = decoderFactoryFactory(typeInfo)

    DetectResourceTypeCollisions("decoders", concreteResourceTypes(typeInfo))

    config => {
      val params = Params(config)
      val decoder = decoderFactory(params)

      (in, document) => decoder.decode(Input(in, document)).map(generic.from)
    }
  }

  // This type class is used to collect all the resource types from the concrete decoders used by the TraitDecoder.
  trait ConcreteResourceTypes[In <: ResourceLike, A <: Coproduct] {
    def apply(info: TraitInfo): Map[String, List[String]]
  }

  object ConcreteResourceTypes {
    implicit def forCNil[In <: ResourceLike]: ConcreteResourceTypes[In, CNil] = { _ => Map.empty }

    implicit def forCCons[In <: ResourceLike, Head, Tail <: Coproduct](implicit
        headDecoder: Lazy[CaseClassResourceDecoder[In, Head]],
        tailDiscriminatorValues: ConcreteResourceTypes[In, Tail]
    ): ConcreteResourceTypes[In, Head :+: Tail] = { info =>
      val tailDiscriminators = tailDiscriminatorValues(info.tail)
      Map(headDecoder.value.resourceType -> List(info.subclassNames.head)) combine tailDiscriminators
    }
  }
}
