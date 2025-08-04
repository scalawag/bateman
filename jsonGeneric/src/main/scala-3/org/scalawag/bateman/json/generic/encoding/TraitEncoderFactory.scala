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

package org.scalawag.bateman.json.generic.encoding

import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.generic.{DiscriminatorCollision, TraitDeriverParams}
import org.scalawag.bateman.json.generic.Discriminators
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait TraitEncoderFactory[From]:
  def apply(params: TraitDeriverParams[JObjectEncoder]): JObjectEncoder[From]

object TraitEncoderFactory:
  inline def summonEncoders[T <: Tuple]: List[JObjectEncoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JObjectEncoder[t]] :: summonEncoders[ts]

  inline def summonClassTags[T <: Tuple]: List[ClassTag[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[ClassTag[t]] :: summonClassTags[ts]

  inline given derived[T](using m: Mirror.SumOf[T]): TraitEncoderFactory[T] =
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val encoders = summonEncoders[m.MirroredElemTypes]
    val classTags = summonClassTags[m.MirroredElemTypes]

    class TraitEncoderFactoryImpl(
        labels: List[String],
        encoders: List[JObjectEncoder[?]],
        classTags: List[ClassTag[?]],
        ordinal: T => Int
    ) extends TraitEncoderFactory[T]:
      def apply(params: TraitDeriverParams[JObjectEncoder]): JObjectEncoder[T] =
        // Pre-compute discriminator mappings for each concrete type
        val mappings = classTags.zip(encoders).map { case (ct, enc) =>
          params.discriminator(using ct.asInstanceOf[ClassTag[Any]], params.config, enc.asInstanceOf[JObjectEncoder[Any]])
        }

        // Check for duplicate discriminator values if required
        if params.discriminator.duplicateValuesForbidden then
          val discriminatorValues = mappings.zip(classTags).map { case (m, ct) =>
            m.value -> List(ct)
          }.groupMapReduce(_._1)(_._2)(_ ++ _)
          DiscriminatorCollision.detect(discriminatorValues)

        class JObjectEncoderImpl(
            encoders: List[JObjectEncoder[?]],
            mappings: List[Discriminators.DiscriminatorMapping[JObjectEncoder, Any]],
            ordinal: T => Int,
            params: TraitDeriverParams[JObjectEncoder]
        ) extends JObjectEncoder[T]:
          def encode(value: T, discriminators: JObject): JObject =
            val ord = ordinal(value)
            val defaultEncoder = encoders(ord).asInstanceOf[JObjectEncoder[Any]]
            val mapping = mappings(ord)

            // Use explicit encoder if provided (e.g., for layered discriminators)
            val effectiveEncoder = mapping.explicit.getOrElse(defaultEncoder).asInstanceOf[JObjectEncoder[Any]]

            // Accumulate this level's discriminator into the discriminators JObject.
            val newDiscriminators = params.addDiscriminator(discriminators, mapping.value)
            effectiveEncoder.encode(value, newDiscriminators)

        new JObjectEncoderImpl(encoders, mappings.asInstanceOf[List[Discriminators.DiscriminatorMapping[JObjectEncoder, Any]]], ordinal, params)

    new TraitEncoderFactoryImpl(labels, encoders, classTags, m.ordinal)
