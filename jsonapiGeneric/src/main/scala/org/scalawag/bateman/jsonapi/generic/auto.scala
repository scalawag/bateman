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

package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.generic.{Config, TraitDeriverParams}
import org.scalawag.bateman.json.JObjectDecoder
import org.scalawag.bateman.jsonapi.ResourceCodec
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.jsonapi.generic.decoding.{CaseClassResourceDecoderFactory, TraitResourceDecoderFactory}
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator.UUIDLidGenerator
import org.scalawag.bateman.jsonapi.generic.encoding.{
  CaseClassResourceEncoderFactory,
  LidGenerator,
  TraitResourceEncoderFactory
}
import shapeless.Lazy

import scala.reflect.ClassTag

object auto extends AutoLowP {
  implicit def autoDeriveResourceCodecForCaseClass[A: ClassTag](implicit
      config: Config = Config.default,
      lidGenerator: LidGenerator = UUIDLidGenerator,
      decoderFactory: Lazy[CaseClassResourceDecoderFactory[A]],
      encoderFactory: Lazy[CaseClassResourceEncoderFactory[A]],
  ): ResourceCodec[A] = {
    import org.scalawag.bateman.jsonapi.generic.encoding.HListResourceEncoderFactory.Params
    val decoder = decoderFactory.value.apply(config, None)
    val encoder = encoderFactory.value.apply(Params(None, config, lidGenerator))
    new ResourceCodec(encoder, decoder)
  }

  implicit def autoDeriveResourceCodecForTrait[A: ClassTag](implicit
      config: Config = Config.default,
      decoderFactory: Lazy[TraitResourceDecoderFactory[A]],
      encoderFactory: Lazy[TraitResourceEncoderFactory[A]],
  ): ResourceCodec[A] = {
//    val decoder = decoderFactory.value.apply(TraitDeriverParams(config, "type", ))
//    val encoder = encoderFactory.value.apply(Params(None, config, lidGenerator))
//    new ResourceCodec(encoder, decoder)
    semiauto.unchecked.deriveResourceCodecForTrait[A](config = config)
  }
}

trait AutoLowP {
  implicit def autoDeriveResourceDecoderForCaseClass[A: ClassTag](implicit
      config: Config = Config.default,
      decoderFactory: Lazy[CaseClassResourceDecoderFactory[A]],
  ): JObjectDecoder[A] =
    semiauto.unchecked.deriveResourceDecoderForCaseClass[A](config = config)

  implicit def autoDeriveResourceDecoderForTrait[A: ClassTag](implicit
      config: Config = Config.default,
      decoderFactory: Lazy[TraitResourceDecoderFactory[A]],
  ): JObjectDecoder[A] =
    semiauto.unchecked.deriveResourceDecoderForTrait[A](config = config)

  implicit def autoDeriveResourceEncoderForCaseClass[A: ClassTag](implicit
      config: Config = Config.default,
      lidGenerator: LidGenerator = UUIDLidGenerator,
      decoderFactory: Lazy[CaseClassResourceEncoderFactory[A]],
  ): ResourceEncoder[A] =
    semiauto.unchecked.deriveResourceEncoderForCaseClass[A](config = config)

  implicit def autoDeriveResourceEncoderForTrait[A: ClassTag](implicit
      config: Config = Config.default,
      decoderFactory: Lazy[TraitResourceEncoderFactory[A]],
  ): ResourceEncoder[A] =
    semiauto.unchecked.deriveResourceEncoderForTrait[A](config = config)
}
