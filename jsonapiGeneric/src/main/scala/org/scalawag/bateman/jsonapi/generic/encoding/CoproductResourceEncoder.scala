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

import org.scalawag.bateman.json.generic.{Config, TraitInfo}
import org.scalawag.bateman.jsonapi.encoding.{ResourceEncoder, ResourceLike}
import org.scalawag.bateman.jsonapi.generic.encoding.CoproductResourceEncoderFactoryFactory.Params
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy}

trait CoproductResourceEncoderFactory[In <: Coproduct, Out <: ResourceLike] {
  def apply(params: Params): ResourceEncoder[In, Out]
}

trait CoproductResourceEncoderFactoryFactory[In <: Coproduct, Out <: ResourceLike] {
  def apply(traitInfo: TraitInfo): CoproductResourceEncoderFactory[In, Out]
}

object CoproductResourceEncoderFactoryFactory {
  final case class Params(config: Config)

  implicit def forCNil[Out <: ResourceLike]: CoproductResourceEncoderFactoryFactory[CNil, Out] = {
    _ => _ => (_, _, _) => ??? // Getting here should be impossible.
  }

  implicit def forCCons[InHead, InTail <: Coproduct, Out <: ResourceLike](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, Out]],
      tailEncoderFactoryFactory: CoproductResourceEncoderFactoryFactory[InTail, Out]
  ): CoproductResourceEncoderFactoryFactory[InHead :+: InTail, Out] =
    traitInfo => {
      val tailEncoderFactory = tailEncoderFactoryFactory(traitInfo.tail)

      params => {
        val tailEncoder = tailEncoderFactory(params)

        (in, includeSpec, fieldsSpec) =>
          in match {
            case Inl(h) => headEncoder.value.encodeResource(h, includeSpec, fieldsSpec)
            case Inr(t) => tailEncoder.encodeResource(t, includeSpec, fieldsSpec)
          }
      }
    }
}
