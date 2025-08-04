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
import org.scalawag.bateman.json.generic.Discriminators.{Discriminator, SimpleClassNameDiscriminator}
import org.scalawag.bateman.json.generic.decoding.{CaseClassDecoderFactory, TraitDecoderFactory}
import org.scalawag.bateman.json.generic.encoding.{CaseClassEncoderFactory, TraitEncoderFactory}
import org.scalawag.bateman.json.lens.{CreatableJLens, stringToLens}
import scala.deriving.Mirror
import scala.reflect.ClassTag

object semiauto:

  // Unified derivers: case class versions (with defaults)

  inline def deriveEncoder[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectEncoder[A] =
    val factory = summon[CaseClassEncoderFactory[A]]
    factory(config)

  inline def deriveDecoder[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectDecoder[A] =
    val factory = summon[CaseClassDecoderFactory[A]]
    factory(config)

  inline def deriveCodec[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectCodec[A] =
    new JObjectCodec(deriveEncoder[A], deriveDecoder[A])

  // Unified derivers: trait versions (no defaults, to avoid overloading restriction)

  inline def deriveEncoder[A](
      discriminatorLens: CreatableJLens[JObject, JAny],
      discriminator: Discriminator[JObjectEncoder]
  )(using m: Mirror.SumOf[A], config: Config): JObjectEncoder[A] =
    val factory = summon[TraitEncoderFactory[A]]
    factory(TraitDeriverParams(config, discriminatorLens, discriminator))

  inline def deriveDecoder[A](
      discriminatorLens: CreatableJLens[JObject, JAny],
      discriminator: Discriminator[JObjectDecoder]
  )(using m: Mirror.SumOf[A], config: Config): JObjectDecoder[A] =
    val factory = summon[TraitDecoderFactory[A]]
    factory(TraitDeriverParams(config, discriminatorLens, discriminator))

  inline def deriveCodec[A](
      discriminatorLens: CreatableJLens[JObject, JAny],
      discriminator: Discriminator[JObjectCodec]
  )(using m: Mirror.SumOf[A], config: Config): JObjectCodec[A] =
    new JObjectCodec(
      deriveEncoder[A](discriminatorLens, codecToEncoderDiscriminator(discriminator)),
      deriveDecoder[A](discriminatorLens, codecToDecoderDiscriminator(discriminator))
    )

  // Convenience aliases: case class

  inline def deriveEncoderForCaseClass[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectEncoder[A] =
    deriveEncoder[A]

  inline def deriveDecoderForCaseClass[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectDecoder[A] =
    deriveDecoder[A]

  inline def deriveCodecForCaseClass[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectCodec[A] =
    new JObjectCodec(deriveEncoder[A], deriveDecoder[A])

  // Convenience aliases: trait (with defaults)

  inline def deriveEncoderForTrait[A](
      discriminatorLens: CreatableJLens[JObject, JAny] = "type",
      discriminator: Discriminator[JObjectEncoder] = SimpleClassNameDiscriminator[JObjectEncoder]
  )(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectEncoder[A] =
    deriveEncoder[A](discriminatorLens, discriminator)

  inline def deriveDecoderForTrait[A](
      discriminatorLens: CreatableJLens[JObject, JAny] = "type",
      discriminator: Discriminator[JObjectDecoder] = SimpleClassNameDiscriminator[JObjectDecoder]
  )(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectDecoder[A] =
    deriveDecoder[A](discriminatorLens, discriminator)

  inline def deriveCodecForTrait[A](
      discriminatorLens: CreatableJLens[JObject, JAny] = "type",
      discriminator: Discriminator[JObjectCodec] = SimpleClassNameDiscriminator[JObjectCodec]
  )(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectCodec[A] =
    deriveCodec[A](discriminatorLens, discriminator)

  private def codecToEncoderDiscriminator(in: Discriminator[JObjectCodec]): Discriminator[JObjectEncoder] =
    new Discriminator[JObjectEncoder]:
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden
      override def apply[B: ClassTag](using config: Config, default: JObjectEncoder[B]) =
        given JObjectCodec[B] = new JObjectCodec(default, null)
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.encoder))

  private def codecToDecoderDiscriminator(in: Discriminator[JObjectCodec]): Discriminator[JObjectDecoder] =
    new Discriminator[JObjectDecoder]:
      override def duplicateValuesForbidden: Boolean = in.duplicateValuesForbidden
      override def apply[B: ClassTag](using config: Config, default: JObjectDecoder[B]) =
        given JObjectCodec[B] = new JObjectCodec(null, default)
        val d = in[B]
        d.copy(explicit = d.explicit.map(_.decoder))

  // Kept for backward compatibility with existing imports of `semiauto._`.
  // Cannot delegate to top-level methods because `inline` methods cannot reference their enclosing package by name.
  @deprecated("Just use the semiauto package without unchecked. There is no longer a checked version.")
  object unchecked:
    inline def deriveEncoder[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectEncoder[A] =
      val factory = summon[CaseClassEncoderFactory[A]]
      factory(config)
    inline def deriveDecoder[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectDecoder[A] =
      val factory = summon[CaseClassDecoderFactory[A]]
      factory(config)
    inline def deriveCodec[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectCodec[A] =
      new JObjectCodec(deriveEncoder[A], deriveDecoder[A])
    inline def deriveEncoder[A](discriminatorLens: CreatableJLens[JObject, JAny], discriminator: Discriminator[JObjectEncoder])(using m: Mirror.SumOf[A], config: Config): JObjectEncoder[A] =
      val factory = summon[TraitEncoderFactory[A]]
      factory(TraitDeriverParams(config, discriminatorLens, discriminator))
    inline def deriveDecoder[A](discriminatorLens: CreatableJLens[JObject, JAny], discriminator: Discriminator[JObjectDecoder])(using m: Mirror.SumOf[A], config: Config): JObjectDecoder[A] =
      val factory = summon[TraitDecoderFactory[A]]
      factory(TraitDeriverParams(config, discriminatorLens, discriminator))
    inline def deriveCodec[A](discriminatorLens: CreatableJLens[JObject, JAny], discriminator: Discriminator[JObjectCodec])(using m: Mirror.SumOf[A], config: Config): JObjectCodec[A] =
      new JObjectCodec(
        deriveEncoder[A](discriminatorLens, codecToEncoderDiscriminator(discriminator)),
        deriveDecoder[A](discriminatorLens, codecToDecoderDiscriminator(discriminator))
      )
    inline def deriveEncoderForCaseClass[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectEncoder[A] =
      deriveEncoder[A]
    inline def deriveDecoderForCaseClass[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectDecoder[A] =
      deriveDecoder[A]
    inline def deriveCodecForCaseClass[A](using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectCodec[A] =
      new JObjectCodec(deriveEncoder[A], deriveDecoder[A])
    inline def deriveEncoderForTrait[A](discriminatorLens: CreatableJLens[JObject, JAny] = "type", discriminator: Discriminator[JObjectEncoder] = SimpleClassNameDiscriminator[JObjectEncoder])(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectEncoder[A] =
      deriveEncoder[A](discriminatorLens, discriminator)
    inline def deriveDecoderForTrait[A](discriminatorLens: CreatableJLens[JObject, JAny] = "type", discriminator: Discriminator[JObjectDecoder] = SimpleClassNameDiscriminator[JObjectDecoder])(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectDecoder[A] =
      deriveDecoder[A](discriminatorLens, discriminator)
    inline def deriveCodecForTrait[A](discriminatorLens: CreatableJLens[JObject, JAny] = "type", discriminator: Discriminator[JObjectCodec] = SimpleClassNameDiscriminator[JObjectCodec])(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectCodec[A] =
      deriveCodec[A](discriminatorLens, discriminator)
