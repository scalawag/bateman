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

package org.scalawag.bateman.jsonapi.generic.encoding

import org.scalawag.bateman.json.JAny
import org.scalawag.bateman.json.generic.TraitDeriverParams
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait TraitResourceEncoderFactory[In]:
  def apply(params: TraitDeriverParams[ResourceEncoder]): ResourceEncoder[In]

object TraitResourceEncoderFactory:
  inline def summonEncoders[T <: Tuple]: List[ResourceEncoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[ResourceEncoder[t]] :: summonEncoders[ts]

  inline def summonClassTags[T <: Tuple]: List[ClassTag[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[ClassTag[t]] :: summonClassTags[ts]

  inline given derived[T](using m: Mirror.SumOf[T]): TraitResourceEncoderFactory[T] =
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val encoders = summonEncoders[m.MirroredElemTypes]
    val classTags = summonClassTags[m.MirroredElemTypes]

    // Use a named class to prevent duplication at each inline call site.
    class TraitResourceEncoderFactoryImpl(
        labels: List[String],
        encoders: List[ResourceEncoder[?]],
        classTags: List[ClassTag[?]],
        ordinal: T => Int
    ) extends TraitResourceEncoderFactory[T]:
      def apply(params: TraitDeriverParams[ResourceEncoder]): ResourceEncoder[T] =
        import params.implicitConfig

        // Build discriminator mappings for each variant
        val discriminatorMappings = labels.zip(encoders).zip(classTags).map {
          case ((label, encoder), ct) =>
            given ClassTag[Any] = ct.asInstanceOf[ClassTag[Any]]
            given ResourceEncoder[Any] = encoder.asInstanceOf[ResourceEncoder[Any]]
            val disc = params.discriminator[Any]
            (disc.value, disc.explicit, encoder, ct)
        }

        // Use a named class to prevent duplication when the factory's apply is called at multiple sites.
        class ResourceEncoderImpl(
            ordinal: T => Int,
            discriminatorMappings: List[(org.scalawag.bateman.json.JAny, Option[ResourceEncoder[Any]], ResourceEncoder[?], ClassTag[?])],
            params: TraitDeriverParams[ResourceEncoder]
        ) extends ResourceEncoder[T]:
          def encodeResource(
              in: T,
              includeSpec: org.scalawag.bateman.jsonapi.encoding.IncludeSpec,
              fieldsSpec: org.scalawag.bateman.jsonapi.encoding.FieldsSpec,
              discriminators: org.scalawag.bateman.json.JObject
          ): org.scalawag.bateman.jsonapi.encoding.EncodeResult[ResourceEncoder.Encoded] =
            val ord = ordinal(in)
            val (discValue, explicitEncoder, defaultEncoder, ct) = discriminatorMappings(ord)
            val effectiveEncoder = explicitEncoder.getOrElse(defaultEncoder).asInstanceOf[ResourceEncoder[Any]]
            val enrichedDiscriminators = params.addDiscriminator(discriminators, discValue)
            effectiveEncoder.encodeResource(in, includeSpec, fieldsSpec, enrichedDiscriminators)

        new ResourceEncoderImpl(ordinal, discriminatorMappings.map {
          case (v, exp, enc, ct) => (v, exp.map(_.asInstanceOf[ResourceEncoder[Any]]), enc, ct)
        }, params)

    new TraitResourceEncoderFactoryImpl(labels, encoders, classTags, m.ordinal)
