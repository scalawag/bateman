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

import org.scalawag.bateman.json.{JObject, JObjectEncoder}
import org.scalawag.bateman.json.generic.TraitDeriverParams
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.jsonapi.encoding.{EncodeResult, FieldsSpec, IncludeSpec}
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy}

import scala.reflect.{ClassTag, classTag}

trait CoproductResourceEncoderFactory[In <: Coproduct] {
  def apply(params: TraitDeriverParams[ResourceEncoder]): ResourceEncoder[In]
}

object CoproductResourceEncoderFactory {
  // Nothing needs to be implemented here because this is only used in a branch that can't be reached (CNil).
  implicit val forCNil: CoproductResourceEncoderFactory[CNil] = _ => (_, _, _, _) => ???

  implicit def forCCons[InHead: ClassTag, InTail <: Coproduct](implicit
      lazyHeadEncoder: Lazy[ResourceEncoder[InHead]],
      tailEncoderFactory: CoproductResourceEncoderFactory[InTail]
  ): CoproductResourceEncoderFactory[InHead :+: InTail] =
    params => {
      import params.implicitConfig
      implicit val headEncoder = lazyHeadEncoder.value
      val disc = params.discriminator[InHead]

      val tailEncoder = tailEncoderFactory(params)

      (in, includeSpec, fieldsSpec, discriminators) =>
        in match {
          case Inr(t) =>
            // This is not the decoder for the instance we've received as input. Defer to the tail encoder.
            tailEncoder.encodeResource(t, includeSpec, fieldsSpec, discriminators)

          case Inl(h) =>
            // This is the type we've received as input, so encode it and then add our discriminator to it.
            // If we got back an encoder from the discriminator, use it. Otherwise, use the one we got implicitly.
            val effectiveHeadEncoder = disc.explicit.getOrElse(headEncoder)
            val newDiscriminatorValue = discriminators
            // Call the encoder to get the base resource and then add our discriminator to it.
            effectiveHeadEncoder.encodeResource(h, includeSpec, fieldsSpec, newDiscriminatorValue).map { encoded =>
              encoded.map(params.addDiscriminator(_, disc.value))
            }
        }
    }
}
