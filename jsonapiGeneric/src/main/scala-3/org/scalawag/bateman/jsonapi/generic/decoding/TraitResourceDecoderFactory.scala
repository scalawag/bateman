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

package org.scalawag.bateman.jsonapi.generic.decoding

import cats.syntax.either.*
import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.generic.{DiscriminatorCollision, TraitDeriverParams}
import org.scalawag.bateman.json.generic.decoding.InvalidDiscriminator
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait TraitResourceDecoderFactory[To]:
  def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[To]

object TraitResourceDecoderFactory:
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

  inline given derived[T](using m: Mirror.SumOf[T]): TraitResourceDecoderFactory[T] =
    // A concrete type that extends multiple sealed traits (each themselves a direct or transitive
    // child of T) will be reached more than once by the flatten. Dedupe by runtime class — they're
    // the same type and resolve to the same decoder and discriminator value.
    val leaves = summonFlattenedLeaves[m.MirroredElemTypes].distinctBy(_._2.runtimeClass)
    val decoders = leaves.map(_._1)
    val classTags = leaves.map(_._2)

    // Use a named class to prevent duplication at each inline call site.
    class TraitResourceDecoderFactoryImpl(
        decoders: List[JObjectDecoder[?]],
        classTags: List[ClassTag[?]]
    ) extends TraitResourceDecoderFactory[T]:
      def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[T] =
        import params.implicitConfig

        // Build discriminator mappings for each leaf type. Retain the `explicit` marker so the
        // duplicate-value check below can distinguish legitimate shared routing (layered
        // discriminators via `forType[IntermediateTrait]`) from actual collisions.
        val mappings: List[(JAny, JObjectDecoder[Any], Option[JObjectDecoder[Any]])] =
          decoders.zip(classTags).map { case (decoder, ct) =>
            given ClassTag[Any] = ct.asInstanceOf[ClassTag[Any]]
            given JObjectDecoder[Any] = decoder.asInstanceOf[JObjectDecoder[Any]]
            val disc = params.discriminator[Any]
            val effectiveDecoder = disc.explicit.getOrElse(decoder).asInstanceOf[JObjectDecoder[Any]]
            (disc.value, effectiveDecoder, disc.explicit.map(_.asInstanceOf[JObjectDecoder[Any]]))
          }

        // Check for duplicate discriminator values if required. Because the hierarchy is flattened,
        // multiple leaves may share a value legitimately when `CustomDiscriminator` routes them
        // through a common explicit decoder (e.g., `forType[IntermediateTrait]("value")` matching
        // all of its leaves via isAssignableFrom). Treat those as a single logical mapping.
        if params.discriminator.duplicateValuesForbidden then
          val conflicts = mappings.zip(classTags)
            .groupBy(_._1._1)
            .filter { (_, entries) =>
              entries.size > 1 && {
                val explicits = entries.map(_._1._3)
                explicits.exists(_.isEmpty) || explicits.map(System.identityHashCode).distinct.size > 1
              }
            }
            .map { (value, entries) => value -> entries.map(_._2) }
          if conflicts.nonEmpty then throw DiscriminatorCollision(conflicts)

        val discriminatorMappings: List[(JAny, JObjectDecoder[Any])] = mappings.map { case (v, d, _) => (v, d) }

        // Use a named class to prevent duplication when the factory's apply is called at multiple sites.
        class JObjectDecoderImpl(
            discriminatorMappings: List[(JAny, JObjectDecoder[Any])],
            params: TraitDeriverParams[JObjectDecoder]
        ) extends JObjectDecoder[T]:
          def decode(jobjFocus: JFocus[JObject], discriminatorFieldFocuses: Set[JFocus[JAny]]): JResult[T] =
            jobjFocus(params.discriminatorLens).flatMap { cursor =>
              val disc = cursor
              val discValue = disc.value.stripLocation
              val consumedFields = discriminatorFieldFocuses + disc

              discriminatorMappings.find(_._1 == discValue) match
                case Some((_, decoder)) =>
                  decoder.decode(jobjFocus, consumedFields.asInstanceOf[Set[JFocus[JAny]]]).map(_.asInstanceOf[T])
                case None =>
                  val valids = discriminatorMappings.map(_._1: JAny).toSet
                  InvalidDiscriminator(disc, valids).leftNec
            }

        new JObjectDecoderImpl(discriminatorMappings, params)

    new TraitResourceDecoderFactoryImpl(decoders, classTags)