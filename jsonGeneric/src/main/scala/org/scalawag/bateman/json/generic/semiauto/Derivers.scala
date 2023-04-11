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

package org.scalawag.bateman.json.generic.semiauto

import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.generic.decoding.TraitDecoderFactory
import org.scalawag.bateman.json.generic.encoding.{CaseClassEncoderFactory, TraitEncoderFactory}
import org.scalawag.bateman.json.{JAny, JObject, JObjectCodec, JObjectDecoder, JObjectEncoder}
import org.scalawag.bateman.json.generic.{Config, Discriminators, TraitDeriverParams}
import org.scalawag.bateman.json.generic.Discriminators.{Discriminator, SimpleClassNameDiscriminator}
import org.scalawag.bateman.json.generic.decoding.CaseClassDecoderFactory
import shapeless.Lazy

import scala.reflect.{ClassTag, classTag}

object Derivers {

  class TraitEncoderDeriver[A] {
    def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectEncoder] = SimpleClassNameDiscriminator[JObjectEncoder],
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[TraitEncoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectEncoder[A] =
      encoderFactory.value(
        TraitDeriverParams[JObjectEncoder](config(defaultConfig), discriminatorLens, discriminator)
      )
  }

  class CaseClassEncoderDeriver[A] {
    def apply(
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[CaseClassEncoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectEncoder[A] =
      encoderFactory.value(config(defaultConfig))
  }

  class TraitDecoderDeriver[A] {
    def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectDecoder] = SimpleClassNameDiscriminator[JObjectDecoder],
        config: DeriverConfigMagnet = Defer
    )(implicit
        decoderFactory: Lazy[TraitDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectDecoder[A] =
      decoderFactory.value(
        TraitDeriverParams[JObjectDecoder](config(defaultConfig), discriminatorLens, discriminator)
      )
  }

  class CaseClassDecoderDeriver[A] {
    def apply(
        config: DeriverConfigMagnet = Defer
    )(implicit
        decoderFactory: Lazy[CaseClassDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectDecoder[A] =
      decoderFactory.value(config(defaultConfig))
  }

  class TraitCodecDeriver[A] {
    def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectCodec] = SimpleClassNameDiscriminator[JObjectCodec],
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[TraitEncoderFactory[A]],
        decoderFactory: Lazy[TraitDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectCodec[A] =
      new JObjectCodec(
        encoderFactory.value(
          TraitDeriverParams[JObjectEncoder](
            config(defaultConfig),
            discriminatorLens,
            new CodecToEncoderDiscriminator(discriminator)
          )
        ),
        decoderFactory.value(
          TraitDeriverParams[JObjectDecoder](
            config(defaultConfig),
            discriminatorLens,
            new CodecToDecoderDiscriminator(discriminator)
          )
        )
      )

    private class CodecToEncoderDiscriminator(in: Discriminator[JObjectCodec]) extends Discriminator[JObjectEncoder] {
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden
      override def apply[B: ClassTag](implicit
          config: Config,
          default: JObjectEncoder[B]
      ): Discriminators.DiscriminatorMapping[JObjectEncoder, B] = {
        implicit val codec: JObjectCodec[B] = new JObjectCodec(default, null) // This would blow up if it were used.
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.encoder))
      }
    }

    private class CodecToDecoderDiscriminator(in: Discriminator[JObjectCodec]) extends Discriminator[JObjectDecoder] {
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden
      override def apply[B: ClassTag](implicit
          config: Config,
          default: JObjectDecoder[B]
      ): Discriminators.DiscriminatorMapping[JObjectDecoder, B] = {
        implicit val codec: JObjectCodec[B] = new JObjectCodec(null, default) // This would blow up if it were used.
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.decoder))
      }
    }
  }

  class CaseClassCodecDeriver[A] {
    def apply(
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[CaseClassEncoderFactory[A]],
        decoderFactory: Lazy[CaseClassDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectCodec[A] =
      new JObjectCodec(
        encoderFactory.value(config(defaultConfig)),
        decoderFactory.value(config(defaultConfig))
      )
  }
}
