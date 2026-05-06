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

package org.scalawag.bateman.json.generic.decoding

import cats.data.NonEmptyChain
import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.generic.{DiscriminatorCollision, TraitDeriverParams, TraitUtils}
import org.scalawag.bateman.json.generic.Discriminators
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait TraitDecoderFactory[To]:
  def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[To]

object TraitDecoderFactory:
  inline given derived[T](using m: Mirror.SumOf[T]): TraitDecoderFactory[T] =
    // A concrete type that extends multiple sealed traits (each themselves a direct or transitive
    // child of T) will be reached more than once by the flatten. Dedupe by runtime class — they're
    // the same type and resolve to the same decoder and discriminator value.
    val leaves = TraitUtils.summonConcretes[m.MirroredElemTypes, JObjectDecoder]
    val decoders = leaves.map(_._1)
    val classTags = leaves.map(_._2)

    class TraitDecoderFactoryImpl(
        decoders: List[JObjectDecoder[?]],
        classTags: List[ClassTag[?]]
    ) extends TraitDecoderFactory[T]:
      def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[T] =
        // Pre-compute discriminator mappings for each leaf type
        val mappings: List[Discriminators.DiscriminatorMapping[JObjectDecoder, Any]] =
          classTags.zip(decoders).map { case (ct, dec) =>
            params.discriminator(using ct.asInstanceOf[ClassTag[Any]], params.config, dec.asInstanceOf[JObjectDecoder[Any]])
          }

        if params.discriminator.duplicateValuesForbidden then
          DiscriminatorCollision.detect(mappings)

        class JObjectDecoderImpl(
            decoders: List[JObjectDecoder[?]],
            mappings: List[Discriminators.DiscriminatorMapping[JObjectDecoder, Any]],
            params: TraitDeriverParams[JObjectDecoder]
        ) extends JObjectDecoder[T]:
          def decode(jobjFocus: JFocus[JObject], discriminatorFieldFocuses: Set[JFocus[JAny]]): JResult[T] =
            jobjFocus(params.discriminatorLens).flatMap { disc =>
              val updatedDiscriminatorFields = discriminatorFieldFocuses + disc
              val discriminatorValue = disc.value.stripLocation

              mappings.zip(decoders)
                .find(_._1.value.stripLocation == discriminatorValue)
                .map { (mapping, decoder) =>
                  val effectiveDecoder = mapping.explicit.getOrElse(decoder).asInstanceOf[JObjectDecoder[T]]
                  effectiveDecoder.decode(jobjFocus, updatedDiscriminatorFields)
                } match
                case Some(result) => result
                case None =>
                  val valids = mappings.map(_.value: JAny).toSet
                  Left(NonEmptyChain.one(InvalidDiscriminator(disc, valids)))
            }

        new JObjectDecoderImpl(decoders, mappings, params)

    new TraitDecoderFactoryImpl(decoders, classTags)
