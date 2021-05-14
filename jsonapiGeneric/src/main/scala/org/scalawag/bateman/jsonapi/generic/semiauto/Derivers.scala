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

package org.scalawag.bateman.jsonapi.generic.semiauto

import org.scalawag.bateman.json.OptionLike
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.jsonapi
import org.scalawag.bateman.jsonapi.generic.{CaseClassResourceCodec, TraitResourceCodec}
import org.scalawag.bateman.jsonapi.generic.decoding.{
  CaseClassResourceDecoder,
  CaseClassResourceDecoderFactory,
  TraitResourceDecoder,
  TraitResourceDecoderFactory
}
import org.scalawag.bateman.jsonapi.generic.encoding.{
  CaseClassResourceEncoder,
  CaseClassResourceEncoderFactory,
  TraitResourceEncoder,
  TraitResourceEncoderFactory
}
import shapeless.Lazy

trait Derivers {

  class TraitResourceEncoderDeriver[In, Out <: jsonapi.encoding.ResourceLike] {
    def apply(
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[TraitResourceEncoderFactory[In, Out]],
        defaultConfig: Config = Config.default
    ): TraitResourceEncoder[In, Out] =
      encoderFactory.value(config.value.getOrElse(defaultConfig))
  }

  class CaseClassResourceEncoderDeriver[In, Out <: jsonapi.encoding.ResourceLike] {
    def apply(
        resourceTypeOverride: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[CaseClassResourceEncoderFactory[In, Out]],
        defaultConfig: Config = Config.default
    ): CaseClassResourceEncoder[In, Out] =
      encoderFactory.value(resourceTypeOverride.value, config.value.getOrElse(defaultConfig))
  }

  class TraitResourceDecoderDeriver[In <: jsonapi.decoding.ResourceLike, Out] {
    def apply(
        config: OptionLike[Config] = None
    )(implicit
        decoderFactory: Lazy[TraitResourceDecoderFactory[In, Out]],
        defaultConfig: Config = Config.default
    ): TraitResourceDecoder[In, Out] =
      decoderFactory.value(config.value.getOrElse(defaultConfig))
  }

  class CaseClassResourceDecoderDeriver[In <: jsonapi.decoding.ResourceLike, Out] {
    def apply(
        resourceTypeOverride: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        decoderFactory: Lazy[CaseClassResourceDecoderFactory[In, Out]],
        defaultConfig: Config = Config.default
    ): CaseClassResourceDecoder[In, Out] =
      decoderFactory.value(resourceTypeOverride.value, config.value.getOrElse(defaultConfig))
  }

  class TraitResourceCodecDeriver[In <: jsonapi.decoding.ResourceLike, A, Out <: jsonapi.encoding.ResourceLike] {
    def apply(
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[TraitResourceEncoderFactory[A, Out]],
        decoderFactory: Lazy[TraitResourceDecoderFactory[In, A]],
        defaultConfig: Config = Config.default
    ): TraitResourceCodec[In, A, Out] =
      TraitResourceCodec(
        new TraitResourceEncoderDeriver[A, Out].apply(config),
        new TraitResourceDecoderDeriver[In, A].apply(config),
      )
  }

  class CaseClassResourceCodecDeriver[In <: jsonapi.decoding.ResourceLike, A, Out <: jsonapi.encoding.ResourceLike] {
    def apply(
        resourceTypeOverride: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[CaseClassResourceEncoderFactory[A, Out]],
        decoderFactory: Lazy[CaseClassResourceDecoderFactory[In, A]],
        defaultConfig: Config = Config.default
    ): CaseClassResourceCodec[In, A, Out] =
      CaseClassResourceCodec(
        new CaseClassResourceEncoderDeriver[A, Out].apply(resourceTypeOverride, config),
        new CaseClassResourceDecoderDeriver[In, A].apply(resourceTypeOverride, config),
      )
  }
}
