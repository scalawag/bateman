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

package org.scalawag.bateman.json.generic

import scala.language.experimental.macros
import org.scalawag.bateman.json.generic.codec.{CaseClassCodec, TraitCodec}
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

object auto {
  // I don't seem to be able to called the checked version here (so that we get the benefits of the macro) due to
  // the fact that the macro can't know the actual type at the time when it is called. A is still an abstract type
  // parameter at this point.

  implicit def deriveEncoderForTrait[A](implicit
      encoderFactory: Lazy[TraitEncoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): TraitEncoder[A] =
    semiauto.unchecked.deriveEncoderForTrait[A]()(encoderFactory, defaultConfig)

  implicit def deriveEncoderForCaseClass[A](implicit
      encoderFactory: Lazy[CaseClassEncoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): CaseClassEncoder[A] =
    semiauto.unchecked.deriveEncoderForCaseClass[A]()(encoderFactory, defaultConfig)

  implicit def deriveDecoderForTrait[A, Context](implicit
      decoderFactory: Lazy[TraitDecoderFactory[A, Context]],
      defaultConfig: Config = Config.default,
  ): TraitDecoder[A, Context] =
    semiauto.unchecked.deriveDecoderForTrait[A, Context]()(decoderFactory, defaultConfig)

  implicit def deriveDecoderForCaseClass[A, Context](implicit
      decoderFactory: Lazy[CaseClassDecoderFactory[A, Context]],
      defaultConfig: Config = Config.default,
  ): CaseClassDecoder[A, Context] =
    semiauto.unchecked.deriveDecoderForCaseClass[A, Context]()(decoderFactory, defaultConfig)

  implicit def deriveCodecForTrait[A, Context](implicit
      encoderFactory: Lazy[TraitEncoderFactory[A]],
      decoderFactory: TraitDecoderFactory[A, Context],
      defaultConfig: Config = Config.default,
  ): TraitCodec[A, Context] =
    semiauto.unchecked.deriveCodecForTrait[A, Context]()(encoderFactory, decoderFactory, defaultConfig)

  implicit def deriveCodecForCaseClass[A, Context](implicit
      encoderFactory: Lazy[CaseClassEncoderFactory[A]],
      decoderFactory: CaseClassDecoderFactory[A, Context],
      defaultConfig: Config = Config.default,
  ): CaseClassCodec[A, Context] =
    semiauto.unchecked.deriveCodecForCaseClass[A, Context]()(encoderFactory, decoderFactory, defaultConfig)
}
