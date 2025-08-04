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

import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import org.scalawag.bateman.json.generic.defaults.DefaultsMacro
import shapeless3.deriving.AllAnnotations
import scala.compiletime.*
import scala.deriving.Mirror

trait CaseClassDecoderFactory[To]:
  def apply(config: Config): JObjectDecoder[To]

object CaseClassDecoderFactory:

  /** Detect field roles from annotation and element type tuples at compile time. */
  inline def detectFieldRoles[Elems <: Tuple, Annots <: Tuple]: List[FieldRole] =
    inline erasedValue[(Elems, Annots)] match
      case _: (EmptyTuple, EmptyTuple) => Nil
      case _: (JSource *: ts, (Source *: _) *: as) =>
        FieldRole.SourceDirect :: detectFieldRoles[ts, as]
      case _: (Option[JSource] *: ts, (Source *: _) *: as) =>
        FieldRole.SourceOption :: detectFieldRoles[ts, as]
      case _: (h *: ts, _ *: as) =>
        FieldRole.Regular :: detectFieldRoles[ts, as]

  class CaseClassDecoderImpl[T](
      tupleDecoder: TupleDecoder[?],
      labels: List[String],
      fieldRoles: List[FieldRole],
      fromProduct: Product => T
  ) extends JObjectDecoder[T]:
    private val hasSourceFields = fieldRoles.exists(_ != FieldRole.Regular)

    def decode(jobjFocus: JFocus[JObject], discriminatorFields: Set[JFocus[JAny]]): JResult[T] =
      tupleDecoder.asInstanceOf[TupleDecoder[Tuple]].decode(
        jobjFocus, labels, Set.empty, discriminatorFields, Map.empty, fieldRoles
      ) match
        case Right(result) =>
          if hasSourceFields then
            // Post-process: inject JSource into @Source field positions
            val source = JSource(jobjFocus, result.fieldSources)
            val arr = result.value.toArray
            fieldRoles.zipWithIndex.foreach {
              case (FieldRole.SourceDirect, idx) => arr(idx) = source
              case (FieldRole.SourceOption, idx) => arr(idx) = Some(source)
              case _ => // no-op
            }
            Right(fromProduct(Tuple.fromArray(arr)))
          else
            Right(fromProduct(result.value))
        case Left(errs) =>
          Left(errs)

  class CaseClassDecoderFactoryImpl[T](
      labels: List[String],
      defaults: Product,
      fieldRoles: List[FieldRole],
      tupleDecoderFactory: TupleDecoderFactory[?],
      fromProduct: Product => T
  ) extends CaseClassDecoderFactory[T]:
    def apply(config: Config): JObjectDecoder[T] =
      val info = CaseClassInfo(defaults, labels)
      val tupleDecoder = tupleDecoderFactory.asInstanceOf[TupleDecoderFactory[Tuple]](info, config)
      new CaseClassDecoderImpl[T](tupleDecoder, labels, fieldRoles, fromProduct)

  inline given derived[T](using m: Mirror.ProductOf[T], aa: AllAnnotations[T]): CaseClassDecoderFactory[T] =
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val defaults = DefaultsMacro.extractDefaults[T]
    val tupleDecoderFactory = summonInline[TupleDecoderFactory[m.MirroredElemTypes]]
    val fieldRoles = detectFieldRoles[m.MirroredElemTypes, aa.Out]
    // Use named classes (CaseClassDecoderFactoryImpl and CaseClassDecoderImpl) instead of anonymous classes
    // to prevent duplication at each inline call site. Without this, the Scala 3 compiler would duplicate
    // the entire anonymous class definition at every location where this inline given is expanded, leading
    // to code bloat.
    new CaseClassDecoderFactoryImpl[T](labels, defaults, fieldRoles, tupleDecoderFactory, m.fromProduct)
