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

package org.scalawag.bateman.json.generic.semiauto

import org.scalawag.bateman.json.OptionLike
import org.scalawag.bateman.json.generic.codec.{CaseClassCodec, TraitCodec}
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.decoding.{
  CaseClassDecoder,
  CaseClassDecoderFactory,
  TraitDecoder,
  TraitDecoderFactory
}
import org.scalawag.bateman.json.generic.encoding.{
  CaseClassEncoder,
  CaseClassEncoderFactory,
  TraitEncoder,
  TraitEncoderFactory
}
import shapeless.Lazy

object Derivers {

  class TraitEncoderDeriver[A] {
    def apply(
        discriminatorField: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[TraitEncoderFactory[A]],
        defaultConfig: Config = Config.default
    ): TraitEncoder[A] =
      encoderFactory.value(discriminatorField, config.value.getOrElse(defaultConfig))
  }

  class CaseClassEncoderDeriver[A] {
    def apply(
        discriminatorValue: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[CaseClassEncoderFactory[A]],
        defaultConfig: Config = Config.default
    ): CaseClassEncoder[A] =
      encoderFactory.value(discriminatorValue.value, config.value.getOrElse(defaultConfig))
  }

  class TraitDecoderDeriver[A, Context] {
    def apply(
        discriminatorField: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        decoderFactory: Lazy[TraitDecoderFactory[A, Context]],
        defaultConfig: Config = Config.default
    ): TraitDecoder[A, Context] =
      decoderFactory.value(discriminatorField, config.value.getOrElse(defaultConfig))
  }

  class CaseClassDecoderDeriver[A, Context] {
    def apply(
        discriminatorValue: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        decoderFactory: Lazy[CaseClassDecoderFactory[A, Context]],
        defaultConfig: Config = Config.default
    ): CaseClassDecoder[A, Context] =
      decoderFactory.value(discriminatorValue.value, config.value.getOrElse(defaultConfig))
  }

  class TraitCodecDeriver[A, Context] {
    def apply(
        discriminatorField: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[TraitEncoderFactory[A]],
        decoderFactory: Lazy[TraitDecoderFactory[A, Context]],
        defaultConfig: Config = Config.default
    ): TraitCodec[A, Context] =
      TraitCodec(
        encoderFactory.value(discriminatorField, config.value.getOrElse(defaultConfig)),
        decoderFactory.value(discriminatorField, config.value.getOrElse(defaultConfig))
      )
  }

  class CaseClassCodecDeriver[A, Context] {
    def apply(
        discriminatorValue: OptionLike[String] = None,
        config: OptionLike[Config] = None
    )(implicit
        encoderFactory: Lazy[CaseClassEncoderFactory[A]],
        decoderFactory: Lazy[CaseClassDecoderFactory[A, Context]],
        defaultConfig: Config = Config.default
    ): CaseClassCodec[A, Context] =
      CaseClassCodec(
        encoderFactory.value(discriminatorValue.value, config.value.getOrElse(defaultConfig)),
        decoderFactory.value(discriminatorValue.value, config.value.getOrElse(defaultConfig))
      )
  }

}
