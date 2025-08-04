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
import org.scalawag.bateman.json.generic.Config
import scala.compiletime.*
import scala.deriving.Mirror

object CaseClassEncoder:
  inline def summonEncoders[T <: Tuple]: List[JAnyEncoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JAnyEncoder[t]] :: summonEncoders[ts]

  inline def extractDefaults[T]: List[Option[Any]] =
    inline erasedValue[T] match
      case _: Product =>
        val mirror = summonInline[Mirror.ProductOf[T]]
        extractDefaultsForFields[T, mirror.MirroredElemTypes, mirror.MirroredElemLabels](0)

  inline def extractDefaultsForFields[T, Elems <: Tuple, Labels <: Tuple](idx: Int): List[Option[Any]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Nil
      case _: (elem *: elems) =>
        extractDefault[T](idx) :: extractDefaultsForFields[T, elems, Labels](idx + 1)

  inline def extractDefault[T](idx: Int): Option[Any] =
    ${org.scalawag.bateman.json.generic.encoding.MacroImpl.extractDefaultImpl[T]('idx)}

  private def makeEncoder[T](encoders: List[JAnyEncoder[?]], labels: List[String], defaults: List[Option[Any]], config: Config): JObjectEncoder[T] =
    (value, discriminators) =>
      val elements = value.asInstanceOf[Product].productIterator.toList
      val fields = labels.zip(elements).zip(encoders).zip(defaults).flatMap {
        case (((label, elem), encoder), defaultOpt) =>
          val fieldName = config.fieldNameMapping(label)

          if !config.encodeDefaultValues && defaultOpt.contains(elem) then
            None
          else
            val encoded = encoder.asInstanceOf[JAnyEncoder[Any]].encode(elem)
            Some((fieldName, encoded))
      }

      JObject((fields ++ discriminators.fieldList.map(f => (f.name.value, f.value))).toSeq: _*)

  inline def derived[T](using m: Mirror.ProductOf[T], config: Config): JObjectEncoder[T] =
    val encoders = summonEncoders[m.MirroredElemTypes]
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val defaults = extractDefaults[T]
    makeEncoder[T](encoders, labels, defaults, config)
