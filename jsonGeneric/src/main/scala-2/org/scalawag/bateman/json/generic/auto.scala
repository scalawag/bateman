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

package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{Encoder, JAnyEncoder, JObjectDecoder, JObjectEncoder, Nullable}
import org.scalawag.bateman.json.generic.decoding.{CaseClassDecoderFactory, TraitDecoderFactory}
import org.scalawag.bateman.json.generic.encoding.{CaseClassEncoderFactory, TraitEncoderFactory}
import shapeless.Lazy

object auto extends AutoDerivers {
  implicit def nullableEncoder[A: JAnyEncoder]: JAnyEncoder[Nullable[A]] = Encoder.nullableEncoder
}

trait AutoDerivers {
  implicit def autoDeriveEncoderForTrait[A](implicit
      encoderFactory: Lazy[TraitEncoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectEncoder[A] =
    semiauto.deriveEncoderForTrait[A]()(encoderFactory, defaultConfig)

  implicit def autoDeriveEncoderForCaseClass[A](implicit
      encoderFactory: Lazy[CaseClassEncoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectEncoder[A] =
    semiauto.deriveEncoderForCaseClass[A]()(encoderFactory, defaultConfig)

  implicit def autoDeriveDecoderForTrait[A](implicit
      decoderFactory: Lazy[TraitDecoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectDecoder[A] =
    semiauto.deriveDecoderForTrait[A]()(decoderFactory, defaultConfig)

  implicit def autoDeriveDecoderForCaseClass[A](implicit
      decoderFactory: Lazy[CaseClassDecoderFactory[A]],
      defaultConfig: Config = Config.default,
  ): JObjectDecoder[A] =
    semiauto.deriveDecoderForCaseClass[A]()(decoderFactory, defaultConfig)
}
