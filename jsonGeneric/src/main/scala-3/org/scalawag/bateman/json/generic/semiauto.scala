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
import scala.compiletime.summonFrom
import scala.deriving.Mirror
import scala.reflect.ClassTag

object semiauto:

  // Case class deriver classes

  class CaseClassEncoderDeriver[A]:
    inline def apply()(using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectEncoder[A] =
      val factory = summon[CaseClassEncoderFactory[A]]
      factory(config)

  class CaseClassDecoderDeriver[A]:
    inline def apply()(using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectDecoder[A] =
      val factory = summon[CaseClassDecoderFactory[A]]
      factory(config)

  class CaseClassCodecDeriver[A]:
    inline def apply()(using m: Mirror.ProductOf[A], config: Config = Config.default): JObjectCodec[A] =
      new JObjectCodec(
        new CaseClassEncoderDeriver[A].apply(),
        new CaseClassDecoderDeriver[A].apply()
      )

  // Trait deriver classes

  class TraitEncoderDeriver[A]:
    inline def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectEncoder] = SimpleClassNameDiscriminator[JObjectEncoder]
    )(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectEncoder[A] =
      val factory = summon[TraitEncoderFactory[A]]
      factory(TraitDeriverParams(config, discriminatorLens, discriminator))

  class TraitDecoderDeriver[A]:
    inline def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectDecoder] = SimpleClassNameDiscriminator[JObjectDecoder]
    )(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectDecoder[A] =
      val factory = summon[TraitDecoderFactory[A]]
      factory(TraitDeriverParams(config, discriminatorLens, discriminator))

  class TraitCodecDeriver[A]:
    inline def apply(
        discriminatorLens: CreatableJLens[JObject, JAny] = "type",
        discriminator: Discriminator[JObjectCodec] = SimpleClassNameDiscriminator[JObjectCodec]
    )(using m: Mirror.SumOf[A], config: Config = Config.default): JObjectCodec[A] =
      new JObjectCodec(
        new TraitEncoderDeriver[A].apply(discriminatorLens, codecToEncoderDiscriminator(discriminator)),
        new TraitDecoderDeriver[A].apply(discriminatorLens, codecToDecoderDiscriminator(discriminator))
      )

  // Entry points

  transparent inline def deriveEncoder[A] =
    summonFrom {
      case _: Mirror.ProductOf[A] => new CaseClassEncoderDeriver[A]
      case _: Mirror.SumOf[A] => new TraitEncoderDeriver[A]
    }

  transparent inline def deriveDecoder[A] =
    summonFrom {
      case _: Mirror.ProductOf[A] => new CaseClassDecoderDeriver[A]
      case _: Mirror.SumOf[A] => new TraitDecoderDeriver[A]
    }

  transparent inline def deriveCodec[A] =
    summonFrom {
      case _: Mirror.ProductOf[A] => new CaseClassCodecDeriver[A]
      case _: Mirror.SumOf[A] => new TraitCodecDeriver[A]
    }

  def deriveEncoderForCaseClass[A]: CaseClassEncoderDeriver[A] = new CaseClassEncoderDeriver[A]
  def deriveDecoderForCaseClass[A]: CaseClassDecoderDeriver[A] = new CaseClassDecoderDeriver[A]
  def deriveCodecForCaseClass[A]: CaseClassCodecDeriver[A] = new CaseClassCodecDeriver[A]

  def deriveEncoderForTrait[A]: TraitEncoderDeriver[A] = new TraitEncoderDeriver[A]
  def deriveDecoderForTrait[A]: TraitDecoderDeriver[A] = new TraitDecoderDeriver[A]
  def deriveCodecForTrait[A]: TraitCodecDeriver[A] = new TraitCodecDeriver[A]

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
