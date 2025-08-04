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
  inline def summonDecoders[T <: Tuple]: List[JObjectDecoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[JObjectDecoder[t]] :: summonDecoders[ts]

  inline def summonClassTags[T <: Tuple]: List[ClassTag[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[ClassTag[t]] :: summonClassTags[ts]

  inline given derived[T](using m: Mirror.SumOf[T]): TraitResourceDecoderFactory[T] =
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val decoders = summonDecoders[m.MirroredElemTypes]
    val classTags = summonClassTags[m.MirroredElemTypes]

    // Use a named class to prevent duplication at each inline call site.
    class TraitResourceDecoderFactoryImpl(
        labels: List[String],
        decoders: List[JObjectDecoder[?]],
        classTags: List[ClassTag[?]]
    ) extends TraitResourceDecoderFactory[T]:
      def apply(params: TraitDeriverParams[JObjectDecoder]): JObjectDecoder[T] =
        import params.implicitConfig

        // Build discriminator mappings for each variant
        val discriminatorMappings: List[(JAny, JObjectDecoder[Any])] = labels.zip(decoders).zip(classTags).map {
          case ((label, decoder), ct) =>
            given ClassTag[Any] = ct.asInstanceOf[ClassTag[Any]]
            given JObjectDecoder[Any] = decoder.asInstanceOf[JObjectDecoder[Any]]
            val disc = params.discriminator[Any]
            val effectiveDecoder = disc.explicit.getOrElse(decoder).asInstanceOf[JObjectDecoder[Any]]
            (disc.value, effectiveDecoder)
        }

        // Check for duplicate discriminator values
        val discriminatorValues: Map[JAny, List[ClassTag[?]]] =
          discriminatorMappings.zip(classTags).map { case ((v, _), ct) => (v, List(ct)) }
            .groupMapReduce(_._1)(_._2)(_ ++ _)

        if params.discriminator.duplicateValuesForbidden then
          DiscriminatorCollision.detect(discriminatorValues)

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

    new TraitResourceDecoderFactoryImpl(labels, decoders, classTags)
