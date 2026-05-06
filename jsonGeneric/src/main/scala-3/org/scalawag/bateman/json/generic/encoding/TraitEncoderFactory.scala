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
import org.scalawag.bateman.json.generic.{DiscriminatorCollision, TraitDeriverParams, TraitUtils}
import org.scalawag.bateman.json.generic.Discriminators
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait TraitEncoderFactory[From]:
  def apply(params: TraitDeriverParams[JObjectEncoder]): JObjectEncoder[From]

object TraitEncoderFactory:
  inline given derived[T](using m: Mirror.SumOf[T]): TraitEncoderFactory[T] =
    // A concrete type that extends multiple sealed traits (each themselves a direct or transitive
    // child of T) will be reached more than once by the flatten. Dedupe by runtime class — they're
    // the same type and resolve to the same encoder and discriminator value.
    val leaves = TraitUtils.summonConcretes[m.MirroredElemTypes, JObjectEncoder]
    val encoders = leaves.map(_._1)
    val classTags = leaves.map(_._2)

    // Use a named class to prevent duplication at each inline call site.
    class TraitEncoderFactoryImpl(
        encoders: List[JObjectEncoder[?]],
        classTags: List[ClassTag[?]]
    ) extends TraitEncoderFactory[T]:
      def apply(params: TraitDeriverParams[JObjectEncoder]): JObjectEncoder[T] =
        // Pre-compute discriminator mappings for each leaf type
        val mappings: List[Discriminators.DiscriminatorMapping[JObjectEncoder, Any]] =
          classTags.zip(encoders).map { case (ct, enc) =>
            params.discriminator(using ct.asInstanceOf[ClassTag[Any]], params.config, enc.asInstanceOf[JObjectEncoder[Any]])
          }

        if params.discriminator.duplicateValuesForbidden then
          DiscriminatorCollision.detect(mappings)

        // Build a class-keyed dispatch table. We dispatch by `value.getClass` rather than by
        // `Mirror.SumOf.ordinal` because the mirror only sees direct subtypes, while the table
        // needs to cover transitive leaves.
        val byClass: Map[Class[?], (JObjectEncoder[Any], Discriminators.DiscriminatorMapping[JObjectEncoder, Any])] =
          encoders.zip(classTags).zip(mappings).map { case ((enc, ct), mapping) =>
            ct.runtimeClass -> (enc.asInstanceOf[JObjectEncoder[Any]], mapping)
          }.toMap

        // Use a named class to prevent duplication when the factory's apply is called at multiple sites.
        class JObjectEncoderImpl(
            byClass: Map[Class[?], (JObjectEncoder[Any], Discriminators.DiscriminatorMapping[JObjectEncoder, Any])],
            params: TraitDeriverParams[JObjectEncoder]
        ) extends JObjectEncoder[T]:
          def encode(value: T, discriminators: JObject): JObject =
            val (defaultEncoder, mapping) = byClass(value.getClass)
            val effectiveEncoder = mapping.explicit.getOrElse(defaultEncoder)
            val newDiscriminators = params.addDiscriminator(discriminators, mapping.value)
            effectiveEncoder.encode(value, newDiscriminators)

        new JObjectEncoderImpl(byClass, params)

    new TraitEncoderFactoryImpl(encoders, classTags)