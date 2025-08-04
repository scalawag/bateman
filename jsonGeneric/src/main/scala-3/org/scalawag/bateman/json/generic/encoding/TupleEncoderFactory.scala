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
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config}
import scala.compiletime.*

trait TupleEncoderFactory[T]:
  def apply(info: CaseClassInfo, config: Config): TupleEncoder[T]

trait TupleEncoder[T]:
  def encode(value: T, discriminators: JObject): JObject

object TupleEncoderFactory:
  inline def summonEncoders[T <: Tuple]: List[JAnyEncoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JAnyEncoder[t]] :: summonEncoders[ts]

  inline given derived[T <: Tuple]: TupleEncoderFactory[T] =
    val encoders = summonEncoders[T]

    // Use a named class instead of an anonymous class to prevent duplication at each inline call site.
    // Without this, the Scala 3 compiler would duplicate the entire anonymous class definition at every
    // location where this inline given is expanded, leading to code bloat.
    class TupleEncoderFactoryImpl(encoders: List[JAnyEncoder[?]]) extends TupleEncoderFactory[T]:
      def apply(info: CaseClassInfo, config: Config): TupleEncoder[T] =
        val labels = info.fieldNames

        // Use a named class for the same reason - to prevent duplication when the factory's apply
        // method is called at multiple inline sites.
        class TupleEncoderImpl(
            encoders: List[JAnyEncoder[?]],
            labels: List[String],
            info: CaseClassInfo,
            config: Config
        ) extends TupleEncoder[T]:
          def encode(value: T, discriminators: JObject): JObject =
            val elements = value.toList
            val fields = labels.zip(elements).zip(encoders).zip(info.defaults).flatMap {
              case (((label, elem), encoder), defaultOpt) =>
                val fieldName = config.fieldNameMapping(label)

                if !config.encodeDefaultValues && defaultOpt.contains(elem) then
                  None
                else
                  val encoded = encoder.asInstanceOf[JAnyEncoder[Any]].encode(elem)
                  Some((fieldName, encoded))
            }

            val baseEncoded = JObject(fields.toSeq: _*)
            DiscriminatorMerge.mergeDiscriminators(discriminators, baseEncoded)

        new TupleEncoderImpl(encoders, labels, info, config)

    new TupleEncoderFactoryImpl(encoders)