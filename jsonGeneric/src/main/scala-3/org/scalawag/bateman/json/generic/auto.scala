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

import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.generic.decoding.{CaseClassDecoderFactory, TraitDecoderFactory}
import org.scalawag.bateman.json.generic.encoding.{CaseClassEncoderFactory, TraitEncoderFactory}
import scala.deriving.Mirror

object auto extends AutoDerivers:
  implicit inline def nullableEncoder[A](implicit enc: JAnyEncoder[A]): JAnyEncoder[Nullable[A]] = Encoder.nullableEncoder

trait AutoDerivers extends AutoCodecDerivers:
  implicit inline def autoDeriveEncoderForTrait[A](implicit
      m: Mirror.SumOf[A],
      encoderFactory: TraitEncoderFactory[A],
      config: Config = Config.default
  ): JObjectEncoder[A] =
    semiauto.deriveEncoderForTrait[A]()

  implicit inline def autoDeriveEncoderForCaseClass[A](implicit
      m: Mirror.ProductOf[A],
      encoderFactory: CaseClassEncoderFactory[A],
      config: Config = Config.default
  ): JObjectEncoder[A] =
    semiauto.deriveEncoderForCaseClass[A]()

  implicit inline def autoDeriveDecoderForTrait[A](implicit
      m: Mirror.SumOf[A],
      decoderFactory: TraitDecoderFactory[A],
      config: Config = Config.default
  ): JObjectDecoder[A] =
    semiauto.deriveDecoderForTrait[A]()

  implicit inline def autoDeriveDecoderForCaseClass[A](implicit
      m: Mirror.ProductOf[A],
      decoderFactory: CaseClassDecoderFactory[A],
      config: Config = Config.default
  ): JObjectDecoder[A] =
    semiauto.deriveDecoderForCaseClass[A]()

trait AutoCodecDerivers:
  implicit inline def autoDeriveCodec[A](implicit
      encoder: JObjectEncoder[A],
      decoder: JObjectDecoder[A]
  ): JObjectCodec[A] =
    new JObjectCodec(encoder, decoder)
