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

package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{Encoder, JAnyEncoder, JObjectCodec, JObjectDecoder, JObjectEncoder, Nullable}
import org.scalawag.bateman.json.generic.decoding.{CaseClassDecoderFactory, TraitDecoderFactory}
import org.scalawag.bateman.json.generic.encoding.{CaseClassEncoderFactory, TraitEncoderFactory}
import shapeless.Lazy

// I don't seem to be able to call the checked version here (so that we get the benefits of the macro) due to the
// fact that the macro can't know the actual type at the time when it is called. A is still just an abstract type
// parameter at this point.

object auto extends AutoMediumP {
  // The wildcard import for auto takes higher precedence than the built-in nullableEncoder. So, being it in here
  // to put it back at the top of the priority for any Nullable[A]
  implicit def nullableEncoder[A: JAnyEncoder]: JAnyEncoder[Nullable[A]] = Encoder.nullableEncoder
}

trait AutoMediumP extends AutoLowP {
  // To avoid implicit ambiguities, a codec will be generated anywhere an encoder or decoder is needed.
  // This is a little wasteful, but I haven't figured out a way to make it less so. The individual encoder
  // or decoder deriver will only be used in cases where that thing is requested and it's impossible to
  // generate a codec (e.g., an encoder is available, but a decoder is not).

  implicit def autoDeriveCodecForTrait[A](implicit
      encoderFactory: Lazy[TraitEncoderFactory[A]],
      decoderFactory: TraitDecoderFactory[A],
      defaultConfig: Config = Config.default,
  ): JObjectCodec[A] =
    semiauto.unchecked.deriveCodecForTrait[A]()(encoderFactory, decoderFactory, defaultConfig)

  implicit def autoDeriveCodecForCaseClass[A](implicit
      encoderFactory: Lazy[CaseClassEncoderFactory[A]],
      decoderFactory: CaseClassDecoderFactory[A],
      defaultConfig: Config = Config.default,
  ): JObjectCodec[A] =
    semiauto.unchecked.deriveCodecForCaseClass[A]()(encoderFactory, decoderFactory, defaultConfig)
}

trait AutoLowP {
  implicit def autoDeriveEncoderForTrait[A](implicit
      encoderFactory: Lazy[TraitEncoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectEncoder[A] =
    semiauto.unchecked.deriveEncoderForTrait[A]()(encoderFactory, defaultConfig)

  implicit def autoDeriveEncoderForCaseClass[A](implicit
      encoderFactory: Lazy[CaseClassEncoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectEncoder[A] =
    semiauto.unchecked.deriveEncoderForCaseClass[A]()(encoderFactory, defaultConfig)

  implicit def autoDeriveDecoderForTrait[A](implicit
      decoderFactory: Lazy[TraitDecoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectDecoder[A] =
    semiauto.unchecked.deriveDecoderForTrait[A]()(decoderFactory, defaultConfig)

  implicit def autoDeriveDecoderForCaseClass[A](implicit
      decoderFactory: Lazy[CaseClassDecoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectDecoder[A] =
    semiauto.unchecked.deriveDecoderForCaseClass[A]()(decoderFactory, defaultConfig)
}
