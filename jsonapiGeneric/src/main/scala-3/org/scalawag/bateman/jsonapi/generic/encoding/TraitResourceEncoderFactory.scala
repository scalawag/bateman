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

import org.scalawag.bateman.json.JObject
import org.scalawag.bateman.json.generic.{DiscriminatorCollision, TraitDeriverParams, TraitUtils}
import org.scalawag.bateman.json.generic.Discriminators
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait TraitResourceEncoderFactory[In]:
  def apply(params: TraitDeriverParams[ResourceEncoder]): ResourceEncoder[In]

object TraitResourceEncoderFactory:
  inline given derived[T](using m: Mirror.SumOf[T]): TraitResourceEncoderFactory[T] =
    // A concrete type that extends multiple sealed traits (each themselves a direct or transitive
    // child of T) will be reached more than once by the flatten. Dedupe by runtime class — they're
    // the same type and resolve to the same encoder and discriminator value.
    val leaves = TraitUtils.summonConcretes[m.MirroredElemTypes, ResourceEncoder]
    val encoders = leaves.map(_._1)
    val classTags = leaves.map(_._2)

    // Use a named class to prevent duplication at each inline call site.
    class TraitResourceEncoderFactoryImpl(
        encoders: List[ResourceEncoder[?]],
        classTags: List[ClassTag[?]]
    ) extends TraitResourceEncoderFactory[T]:
      def apply(params: TraitDeriverParams[ResourceEncoder]): ResourceEncoder[T] =
        import params.implicitConfig

        // Pre-compute discriminator mappings for each leaf type
        val mappings: List[Discriminators.DiscriminatorMapping[ResourceEncoder, Any]] =
          classTags.zip(encoders).map { case (ct, enc) =>
            given ClassTag[Any] = ct.asInstanceOf[ClassTag[Any]]
            given ResourceEncoder[Any] = enc.asInstanceOf[ResourceEncoder[Any]]
            params.discriminator[Any]
          }

        if params.discriminator.duplicateValuesForbidden then
          DiscriminatorCollision.detect(mappings)

        // Build a class-keyed dispatch table. We dispatch by `value.getClass` rather than by
        // `Mirror.SumOf.ordinal` because the mirror only sees direct subtypes, while the table
        // needs to cover transitive leaves.
        val byClass: Map[Class[?], (ResourceEncoder[Any], Discriminators.DiscriminatorMapping[ResourceEncoder, Any])] =
          encoders.zip(classTags).zip(mappings).map { case ((enc, ct), mapping) =>
            ct.runtimeClass -> (enc.asInstanceOf[ResourceEncoder[Any]], mapping)
          }.toMap

        // Use a named class to prevent duplication when the factory's apply is called at multiple sites.
        class ResourceEncoderImpl(
            byClass: Map[Class[?], (ResourceEncoder[Any], Discriminators.DiscriminatorMapping[ResourceEncoder, Any])],
            params: TraitDeriverParams[ResourceEncoder]
        ) extends ResourceEncoder[T]:
          def encodeResource(
              in: T,
              includeSpec: org.scalawag.bateman.jsonapi.encoding.IncludeSpec,
              fieldsSpec: org.scalawag.bateman.jsonapi.encoding.FieldsSpec,
              discriminators: JObject
          ): org.scalawag.bateman.jsonapi.encoding.EncodeResult[ResourceEncoder.Encoded] =
            val (defaultEncoder, mapping) = byClass(in.getClass)
            val effectiveEncoder = mapping.explicit.getOrElse(defaultEncoder)
            val enrichedDiscriminators = params.addDiscriminator(discriminators, mapping.value)
            effectiveEncoder.encodeResource(in, includeSpec, fieldsSpec, enrichedDiscriminators)

        new ResourceEncoderImpl(byClass, params)

    new TraitResourceEncoderFactoryImpl(encoders, classTags)