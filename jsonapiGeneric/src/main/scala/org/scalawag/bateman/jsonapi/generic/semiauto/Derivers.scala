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

package org.scalawag.bateman.jsonapi.generic.semiauto

import org.scalawag.bateman.json.lens.CreatableJLens
import org.scalawag.bateman.json.{JAny, JObject, JObjectDecoder, OptionLike}
import org.scalawag.bateman.json.generic.{Config, Discriminators, TraitDeriverParams}
import org.scalawag.bateman.json.generic.semiauto.{Defer, DeriverConfigMagnet}
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.generic.Discriminators.{Discriminator, SimpleClassNameDiscriminator}
import org.scalawag.bateman.jsonapi.generic.decoding.{CaseClassResourceDecoderFactory, TraitResourceDecoderFactory}
import org.scalawag.bateman.jsonapi.ResourceCodec
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator.UUIDLidGenerator
import org.scalawag.bateman.jsonapi.generic.encoding.HListResourceEncoderFactory.Params
import org.scalawag.bateman.jsonapi.generic.encoding.{
  CaseClassResourceEncoderFactory,
  LidGenerator,
  TraitResourceEncoderFactory
}
import shapeless.Lazy

import scala.reflect.ClassTag

trait Derivers {
  class TraitResourceEncoderDeriver[A] {
    def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[ResourceEncoder] = SimpleClassNameDiscriminator[ResourceEncoder],
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[TraitResourceEncoderFactory[A]],
        defaultConfig: Config = Config.default
    ): ResourceEncoder[A] =
      encoderFactory.value.apply(
        TraitDeriverParams[ResourceEncoder](config(defaultConfig), discriminatorLens, discriminator)
      )
  }

  class CaseClassResourceEncoderDeriver[A] {
    def apply(
        resourceTypeOverride: OptionLike[String] = None,
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[CaseClassResourceEncoderFactory[A]],
        defaultConfig: Config = Config.default,
        lidGenerator: LidGenerator = UUIDLidGenerator
    ): ResourceEncoder[A] =
      encoderFactory.value(Params(resourceTypeOverride.value, config(defaultConfig), lidGenerator))
  }

  class TraitResourceDecoderDeriver[A] {
    def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectDecoder] = SimpleClassNameDiscriminator[JObjectDecoder],
        config: DeriverConfigMagnet = Defer
    )(implicit
        decoderFactory: Lazy[TraitResourceDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectDecoder[A] =
      decoderFactory.value(
        TraitDeriverParams[JObjectDecoder](config(defaultConfig), discriminatorLens, discriminator)
      )
  }

  class CaseClassResourceDecoderDeriver[A] {
    def apply(
        resourceType: OptionLike[String] = None,
        config: DeriverConfigMagnet = Defer
    )(implicit
        decoderFactory: Lazy[CaseClassResourceDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): JObjectDecoder[A] =
      decoderFactory.value(config(defaultConfig), resourceType.value)
  }

  class TraitResourceCodecDeriver[A] {
    def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[ResourceCodec] = SimpleClassNameDiscriminator[ResourceCodec],
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[TraitResourceEncoderFactory[A]],
        decoderFactory: Lazy[TraitResourceDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): ResourceCodec[A] =
      new ResourceCodec(
        encoderFactory.value(
          TraitDeriverParams[ResourceEncoder](
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

    private class CodecToEncoderDiscriminator(in: Discriminator[ResourceCodec]) extends Discriminator[ResourceEncoder] {
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden

      override def apply[B: ClassTag](implicit
          config: Config,
          default: ResourceEncoder[B]
      ): Discriminators.DiscriminatorMapping[ResourceEncoder, B] = {
        implicit val codec: ResourceCodec[B] = new ResourceCodec(default, null) // This would blow up if it were used.
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.encoder))
      }
    }

    private class CodecToDecoderDiscriminator(in: Discriminator[ResourceCodec]) extends Discriminator[JObjectDecoder] {
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden

      override def apply[B: ClassTag](implicit
          config: Config,
          default: JObjectDecoder[B]
      ): Discriminators.DiscriminatorMapping[JObjectDecoder, B] = {
        implicit val codec: ResourceCodec[B] = new ResourceCodec(null, default) // This would blow up if it were used.
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.decoder))
      }
    }
  }

  class CaseClassResourceCodecDeriver[A] {
    def apply(
        resourceTypeOverride: OptionLike[String] = None,
        config: DeriverConfigMagnet = Defer
    )(implicit
        encoderFactory: Lazy[CaseClassResourceEncoderFactory[A]],
        decoderFactory: Lazy[CaseClassResourceDecoderFactory[A]],
        defaultConfig: Config = Config.default
    ): ResourceCodec[A] =
      new ResourceCodec(
        new CaseClassResourceEncoderDeriver[A].apply(resourceTypeOverride, config(defaultConfig)),
        new CaseClassResourceDecoderDeriver[A].apply(resourceTypeOverride, config(defaultConfig)),
      )
  }
}
