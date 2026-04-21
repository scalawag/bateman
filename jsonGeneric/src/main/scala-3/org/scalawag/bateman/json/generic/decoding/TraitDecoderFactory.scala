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
import org.scalawag.bateman.json.generic.{DiscriminatorCollision, TraitDeriverParams}
import org.scalawag.bateman.json.generic.Discriminators
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait TraitDecoderFactory[To]:
  def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[To]

object TraitDecoderFactory:
  /** Recursively flatten a tuple of subtypes, expanding any element that is itself a sealed trait
    * into its (transitive) leaf subtypes. The encoder writes the leaf type's discriminator value
    * at encode time (via overwrite semantics in `addDiscriminator`), so the decoder must be able
    * to match against those leaf values — hence the flattening at derivation time.
    *
    * Decoders and ClassTags are summoned together in a single recursive pass to keep the generated
    * tree shallow enough for the JS compiler's default stack (which is smaller than on JVM).
    */
  inline def summonFlattenedLeaves[T <: Tuple]: List[(JObjectDecoder[?], ClassTag[?])] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val tail = summonFlattenedLeaves[ts]
        summonFrom {
          case innerM: Mirror.SumOf[`t`] =>
            summonFlattenedLeaves[innerM.MirroredElemTypes] ::: tail
          case _ =>
            (summonInline[JObjectDecoder[t]], summonInline[ClassTag[t]]) :: tail
        }

  inline given derived[T](using m: Mirror.SumOf[T]): TraitDecoderFactory[T] =
    val leaves = summonFlattenedLeaves[m.MirroredElemTypes]
    val decoders = leaves.map(_._1)
    val classTags = leaves.map(_._2)

    class TraitDecoderFactoryImpl(
        decoders: List[JObjectDecoder[?]],
        classTags: List[ClassTag[?]]
    ) extends TraitDecoderFactory[T]:
      def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[T] =
        // Pre-compute discriminator mappings for each leaf type
        val mappings = classTags.zip(decoders).map { case (ct, dec) =>
          params.discriminator(using ct.asInstanceOf[ClassTag[Any]], params.config, dec.asInstanceOf[JObjectDecoder[Any]])
        }

        // Check for duplicate discriminator values if required. Because the hierarchy is flattened,
        // multiple leaves may share a value legitimately when `CustomDiscriminator` routes them
        // through a common explicit decoder (e.g., `forType[IntermediateTrait]("value")` matching
        // all of its leaves via isAssignableFrom). Treat those as a single logical mapping.
        if params.discriminator.duplicateValuesForbidden then
          val conflicts = mappings.zip(classTags)
            .groupBy(_._1.value)
            .filter { (_, entries) =>
              entries.size > 1 && {
                val explicits = entries.map(_._1.explicit)
                explicits.exists(_.isEmpty) || explicits.map(System.identityHashCode).distinct.size > 1
              }
            }
            .map { (value, entries) => value -> entries.map(_._2) }
          if conflicts.nonEmpty then throw DiscriminatorCollision(conflicts)

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

        new JObjectDecoderImpl(decoders, mappings.asInstanceOf[List[Discriminators.DiscriminatorMapping[JObjectDecoder, Any]]], params)

    new TraitDecoderFactoryImpl(decoders, classTags)
