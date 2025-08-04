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
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import org.scalawag.bateman.json.generic.defaults.DefaultsMacro
import shapeless3.deriving.AllAnnotations
import scala.compiletime.*
import scala.deriving.Mirror

trait CaseClassEncoderFactory[CaseClass]:
  def apply(config: Config): JObjectEncoder[CaseClass]

object CaseClassEncoderFactory:
  case class FieldEncoderInfo[T](encoder: JAnyEncoder[T], isOptional: Boolean, isSource: Boolean)

  inline def summonFieldEncoder[T]: FieldEncoderInfo[?] =
    inline erasedValue[T] match
      case _: Option[t] =>
        val baseEncoder = summonInline[JAnyEncoder[t]]
        class OptionEncoder(baseEncoder: JAnyEncoder[t]) extends JAnyEncoder[Option[t]]:
          def encode(value: Option[t]): JAny =
            value.map(baseEncoder.encode).getOrElse(JNull)
        FieldEncoderInfo(new OptionEncoder(baseEncoder).asInstanceOf[JAnyEncoder[Any]], true, false)
      case _ =>
        val encoder = summonInline[JAnyEncoder[T]]
        FieldEncoderInfo(encoder.asInstanceOf[JAnyEncoder[Any]], false, false)

  // A no-op encoder used as a placeholder for @Source fields (never actually called)
  private val sourceEncoder: JAnyEncoder[Any] = (_: Any) => JNull

  inline def summonEncoders[Elems <: Tuple, Annots <: Tuple]: List[FieldEncoderInfo[?]] =
    inline erasedValue[(Elems, Annots)] match
      case _: (EmptyTuple, EmptyTuple) => Nil
      case _: (h *: ts, (Source *: _) *: as) =>
        FieldEncoderInfo(sourceEncoder, false, true) :: summonEncoders[ts, as]
      case _: (h *: ts, _ *: as) =>
        summonFieldEncoder[h] :: summonEncoders[ts, as]

  inline given derived[T](using m: Mirror.ProductOf[T], aa: AllAnnotations[T]): CaseClassEncoderFactory[T] =
    val encoders = summonEncoders[m.MirroredElemTypes, aa.Out]
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val defaults = DefaultsMacro.extractDefaults[T]

    // Use a named class instead of an anonymous class to prevent duplication at each inline call site.
    // Without this, the Scala 3 compiler would duplicate the entire anonymous class definition at every
    // location where this inline given is expanded, leading to code bloat.
    class CaseClassEncoderFactoryImpl(
        encoders: List[FieldEncoderInfo[?]],
        labels: List[String],
        defaults: Product
    ) extends CaseClassEncoderFactory[T]:
      def apply(config: Config): JObjectEncoder[T] =
        val info = CaseClassInfo(defaults, labels)

        // Use a named class for the same reason - to prevent duplication when the factory's apply
        // method is called at multiple inline sites.
        class JObjectEncoderImpl(
            encoders: List[FieldEncoderInfo[?]],
            labels: List[String],
            info: CaseClassInfo,
            config: Config
        ) extends JObjectEncoder[T]:
          def encode(value: T, discriminators: JObject): JObject =
            val elements = value.asInstanceOf[Product].productIterator.toList
            val fields = labels.zip(elements).zip(encoders).flatMap { case ((label, elem), fieldInfo) =>
              // Skip @Source fields entirely
              if fieldInfo.isSource then
                None
              else
                val fieldName = config.fieldNameMapping(label)
                val defaultOpt = info.fieldByName(label).flatMap(_._2)

                val encoded = fieldInfo.encoder.asInstanceOf[JAnyEncoder[Any]].encode(elem)

                // Skip if it's a default value and we're not encoding defaults.
                // Compare raw values (not encoded) to avoid conflating None and Some(JNull).
                if !config.encodeDefaultValues && defaultOpt.contains(elem) then
                  None
                // Skip if it's an optional field set to None
                else if fieldInfo.isOptional && elem == None then
                  None
                else
                  Some((fieldName, encoded))
            }

            val baseEncoded = JObject(fields.toSeq*)
            DiscriminatorMerge.mergeDiscriminators(discriminators, baseEncoded)

        new JObjectEncoderImpl(encoders, labels, info, config)

    new CaseClassEncoderFactoryImpl(encoders, labels, defaults)
