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

import org.scalawag.bateman.json.lens.{CreatableJLens, stringToLens}
import org.scalawag.bateman.json.{JAny, JObject, JObjectDecoder, OptionLike}
import org.scalawag.bateman.json.generic.{Config, TraitDeriverParams}
import org.scalawag.bateman.json.generic.Discriminators.{Discriminator, SimpleClassNameDiscriminator}
import org.scalawag.bateman.json.generic.{DeriverConfigMagnet, Defer}
import org.scalawag.bateman.jsonapi.ResourceCodec
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.jsonapi.generic.decoding.{CaseClassResourceDecoderFactory, TraitResourceDecoderFactory}
import org.scalawag.bateman.jsonapi.generic.encoding.CaseClassResourceEncoderFactory
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator.UUIDLidGenerator
import org.scalawag.bateman.jsonapi.generic.encoding.TraitResourceEncoderFactory

import scala.deriving.Mirror
import scala.reflect.ClassTag

object semiauto:
  // Kept for backward compatibility with existing imports of `semiauto._`
  val unchecked: semiauto.type = semiauto

  class TraitResourceEncoderDeriver[A]:
    inline def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[ResourceEncoder] = SimpleClassNameDiscriminator[ResourceEncoder],
        config: DeriverConfigMagnet = Defer
    )(using
        m: Mirror.SumOf[A],
        defaultConfig: Config = Config.default
    ): ResourceEncoder[A] =
      val encoderFactory = TraitResourceEncoderFactory.derived[A]
      encoderFactory(
        TraitDeriverParams[ResourceEncoder](config(defaultConfig), discriminatorLens, discriminator)
      )

  class CaseClassResourceEncoderDeriver[A]:
    def apply(
        resourceTypeOverride: OptionLike[String] = None,
        config: DeriverConfigMagnet = Defer
    )(using
        encoderFactory: CaseClassResourceEncoderFactory[A],
        defaultConfig: Config = Config.default,
        lidGenerator: LidGenerator = UUIDLidGenerator
    ): ResourceEncoder[A] =
      encoderFactory(CaseClassResourceEncoderFactory.Params(resourceTypeOverride.value, config(defaultConfig), lidGenerator))

  class TraitResourceDecoderDeriver[A]:
    inline def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectDecoder] = SimpleClassNameDiscriminator[JObjectDecoder],
        config: DeriverConfigMagnet = Defer
    )(using
        m: Mirror.SumOf[A],
        defaultConfig: Config = Config.default
    ): JObjectDecoder[A] =
      val decoderFactory = TraitResourceDecoderFactory.derived[A]
      decoderFactory(
        TraitDeriverParams[JObjectDecoder](config(defaultConfig), discriminatorLens, discriminator)
      )

  class CaseClassResourceDecoderDeriver[A]:
    def apply(
        resourceType: OptionLike[String] = None,
        config: DeriverConfigMagnet = Defer
    )(using
        decoderFactory: CaseClassResourceDecoderFactory[A],
        defaultConfig: Config = Config.default
    ): JObjectDecoder[A] =
      decoderFactory(config(defaultConfig), resourceType.value)

  class TraitResourceCodecDeriver[A]:
    inline def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[ResourceCodec] = SimpleClassNameDiscriminator[ResourceCodec],
        config: DeriverConfigMagnet = Defer
    )(using
        m: Mirror.SumOf[A],
        defaultConfig: Config = Config.default
    ): ResourceCodec[A] =
      val encoderFactory = TraitResourceEncoderFactory.derived[A]
      val decoderFactory = TraitResourceDecoderFactory.derived[A]
      new ResourceCodec(
        encoderFactory(
          TraitDeriverParams[ResourceEncoder](
            config(defaultConfig),
            discriminatorLens,
            new CodecToEncoderDiscriminator(discriminator)
          )
        ),
        decoderFactory(
          TraitDeriverParams[JObjectDecoder](
            config(defaultConfig),
            discriminatorLens,
            new CodecToDecoderDiscriminator(discriminator)
          )
        )
      )

    private class CodecToEncoderDiscriminator(in: Discriminator[ResourceCodec]) extends Discriminator[ResourceEncoder]:
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden
      override def apply[B: ClassTag](using
          config: Config,
          default: ResourceEncoder[B]
      ): org.scalawag.bateman.json.generic.Discriminators.DiscriminatorMapping[ResourceEncoder, B] =
        given ResourceCodec[B] = new ResourceCodec(default, null)
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.encoder))

    private class CodecToDecoderDiscriminator(in: Discriminator[ResourceCodec]) extends Discriminator[JObjectDecoder]:
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden
      override def apply[B: ClassTag](using
          config: Config,
          default: JObjectDecoder[B]
      ): org.scalawag.bateman.json.generic.Discriminators.DiscriminatorMapping[JObjectDecoder, B] =
        given ResourceCodec[B] = new ResourceCodec(null, default)
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.decoder))

  class CaseClassResourceCodecDeriver[A]:
    def apply(
        resourceTypeOverride: OptionLike[String] = None,
        config: DeriverConfigMagnet = Defer
    )(using
        encoderFactory: CaseClassResourceEncoderFactory[A],
        decoderFactory: CaseClassResourceDecoderFactory[A],
        defaultConfig: Config = Config.default,
        lidGenerator: LidGenerator = UUIDLidGenerator
    ): ResourceCodec[A] =
      new ResourceCodec(
        new CaseClassResourceEncoderDeriver[A].apply(resourceTypeOverride, config),
        new CaseClassResourceDecoderDeriver[A].apply(resourceTypeOverride, config),
      )

  def deriveResourceEncoderForTrait[A]: TraitResourceEncoderDeriver[A] =
    new TraitResourceEncoderDeriver[A]

  def deriveResourceEncoderForCaseClass[A]: CaseClassResourceEncoderDeriver[A] =
    new CaseClassResourceEncoderDeriver[A]

  def deriveResourceDecoderForTrait[A]: TraitResourceDecoderDeriver[A] =
    new TraitResourceDecoderDeriver[A]

  def deriveResourceDecoderForCaseClass[A]: CaseClassResourceDecoderDeriver[A] =
    new CaseClassResourceDecoderDeriver[A]

  def deriveResourceCodecForTrait[A]: TraitResourceCodecDeriver[A] =
    new TraitResourceCodecDeriver[A]

  def deriveResourceCodecForCaseClass[A]: CaseClassResourceCodecDeriver[A] =
    new CaseClassResourceCodecDeriver[A]
