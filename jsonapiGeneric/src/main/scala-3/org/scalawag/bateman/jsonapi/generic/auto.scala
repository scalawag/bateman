// bateman -- Copyright 2021-2026 -- Justin Patterson
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

package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.{Encoder, JAnyEncoder, Nullable}
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.JObjectDecoder
import org.scalawag.bateman.jsonapi.ResourceCodec
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.jsonapi.generic.decoding.{CaseClassResourceDecoderFactory, TraitResourceDecoderFactory}
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator.UUIDLidGenerator
import org.scalawag.bateman.jsonapi.generic.encoding.{CaseClassResourceEncoderFactory, TraitResourceEncoderFactory}

import org.scalawag.bateman.json.lens.stringToLens

import scala.reflect.ClassTag

// Use implicit def (not given) so that `import auto._` works consistently in both Scala 2 and 3.
// By-name factory parameters (=>) replace Shapeless's Lazy and prevent divergent implicit search.
//
// Three-level priority hierarchy (matching jsonGeneric's pattern):
//   auto (highest)   - nullableEncoder override to prevent covariant ResourceEncoder from beating companion's JAnyEncoder
//   AutoMidP         - individual encoder/decoder derivers (higher than codecs to avoid hard inline expansion errors)
//   AutoLowP         - codec derivers (lowest priority fallback when both encoder and decoder are needed)
object auto extends AutoMidP {
  // Nullable's companion defines JAnyEncoder[Nullable[A]]. Without this override, the trait deriver
  // would produce ResourceEncoder[Nullable[A]] which beats the companion encoder due to covariance
  // (ResourceEncoder <: JAnyEncoder) and import-over-companion priority in Scala 3.
  implicit inline def nullableEncoder[A](implicit enc: JAnyEncoder[A]): JAnyEncoder[Nullable[A]] = Encoder.nullableEncoder
}

trait AutoMidP extends AutoLowP {
  implicit def autoDeriveResourceEncoderForCaseClass[A](implicit
      ct: ClassTag[A],
      config: Config = Config.default,
      lidGenerator: LidGenerator = UUIDLidGenerator,
      encoderFactory: CaseClassResourceEncoderFactory[A],
  ): ResourceEncoder[A] =
    semiauto.deriveResourceEncoderForCaseClass[A].apply(config = config)

  implicit def autoDeriveResourceDecoderForCaseClass[A](implicit
      ct: ClassTag[A],
      config: Config = Config.default,
      decoderFactory: CaseClassResourceDecoderFactory[A],
  ): JObjectDecoder[A] =
    semiauto.deriveResourceDecoderForCaseClass[A].apply(config = config)

  implicit inline def autoDeriveResourceEncoderForTrait[A](using
      encoderFactory: => TraitResourceEncoderFactory[A],
      config: Config = Config.default,
  ): ResourceEncoder[A] =
    encoderFactory(
      org.scalawag.bateman.json.generic.TraitDeriverParams[ResourceEncoder](
        config, "type",
        org.scalawag.bateman.json.generic.Discriminators.SimpleClassNameDiscriminator[ResourceEncoder]
      )
    )

  implicit inline def autoDeriveResourceDecoderForTrait[A](using
      decoderFactory: => TraitResourceDecoderFactory[A],
      config: Config = Config.default,
  ): JObjectDecoder[A] =
    decoderFactory(
      org.scalawag.bateman.json.generic.TraitDeriverParams[JObjectDecoder](
        config, "type",
        org.scalawag.bateman.json.generic.Discriminators.SimpleClassNameDiscriminator[JObjectDecoder]
      )
    )
}

trait AutoLowP {
  implicit def autoDeriveResourceCodecForCaseClass[A](implicit
      ct: ClassTag[A],
      config: Config = Config.default,
      lidGenerator: LidGenerator = UUIDLidGenerator,
      decoderFactory: CaseClassResourceDecoderFactory[A],
      encoderFactory: CaseClassResourceEncoderFactory[A],
  ): ResourceCodec[A] = {
    val decoder = decoderFactory(config, None)
    val encoder = encoderFactory(CaseClassResourceEncoderFactory.Params(None, config, lidGenerator))
    new ResourceCodec(encoder, decoder)
  }

  implicit inline def autoDeriveResourceCodecForTrait[A](using
      encoderFactory: => TraitResourceEncoderFactory[A],
      decoderFactory: => TraitResourceDecoderFactory[A],
      config: Config = Config.default,
  ): ResourceCodec[A] =
    new ResourceCodec(
      encoderFactory(
        org.scalawag.bateman.json.generic.TraitDeriverParams[ResourceEncoder](
          config, "type",
          org.scalawag.bateman.json.generic.Discriminators.SimpleClassNameDiscriminator[ResourceEncoder]
        )
      ),
      decoderFactory(
        org.scalawag.bateman.json.generic.TraitDeriverParams[JObjectDecoder](
          config, "type",
          org.scalawag.bateman.json.generic.Discriminators.SimpleClassNameDiscriminator[JObjectDecoder]
        )
      )
    )
}
