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
import org.scalawag.bateman.json.focus.{JFocus, JFieldFocus}
import org.scalawag.bateman.json.focus.JFocus._
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config}
import scala.compiletime.*

/** Describes the role of a field with respect to @Source annotation handling. */
enum FieldRole:
  case Regular
  case SourceDirect   // @Source field of type JSource
  case SourceOption   // @Source field of type Option[JSource]

trait TupleDecoderFactory[T]:
  def apply(info: CaseClassInfo, config: Config): TupleDecoder[T]

/** Result of tuple decoding includes consumed fields and field source tracking. */
case class TupleDecoderResult[T](
    value: T,
    consumedFields: Set[String],
    fieldSources: Map[String, JFocus[JAny]]
)

trait TupleDecoder[T]:
  def decode(
      jobjFocus: JFocus[JObject],
      fieldNames: List[String],
      consumedFields: Set[String],
      discriminatorFields: Set[JFocus[JAny]],
      fieldSources: Map[String, JFocus[JAny]],
      fieldRoles: List[FieldRole]
  ): JResult[TupleDecoderResult[T]]

object TupleDecoderFactory:

  // Decoder for EmptyTuple - this is where we check for unknown fields
  given emptyTupleDecoder: TupleDecoderFactory[EmptyTuple] = new TupleDecoderFactory[EmptyTuple]:
    def apply(info: CaseClassInfo, config: Config): TupleDecoder[EmptyTuple] =
      new TupleDecoder[EmptyTuple]:
        def decode(
            jobjFocus: JFocus[JObject],
            fieldNames: List[String],
            consumedFields: Set[String],
            discriminatorFields: Set[JFocus[JAny]],
            fieldSources: Map[String, JFocus[JAny]],
            fieldRoles: List[FieldRole]
        ): JResult[TupleDecoderResult[EmptyTuple]] =
          if config.allowUnknownFields then
            Right(TupleDecoderResult(EmptyTuple, consumedFields, fieldSources))
          else
            val jobj = jobjFocus.value
            // Check for unknown fields
            // discriminatorFields are field focuses pointing to field values that should be ignored
            val discriminatorFieldNames = discriminatorFields.collect {
              case fieldFocus: JFieldFocus[_, _] if fieldFocus.parent == jobjFocus =>
                fieldFocus.name.value
            }
            val actualFieldNames = jobj.fieldList.map(_.name.value).toSet
            val unknownFields = actualFieldNames -- consumedFields -- discriminatorFieldNames

            if unknownFields.isEmpty then
              Right(TupleDecoderResult(EmptyTuple, consumedFields, fieldSources))
            else
              val errors = unknownFields.flatMap { fieldName =>
                // Manually create the field focus
                jobj.fieldList.zipWithIndex.collectFirst {
                  case (field, idx) if field.name.value == fieldName =>
                    UnexpectedValue(JFieldFocus(field.value, field.name, idx, jobjFocus.asInstanceOf[JFocus[JObject]]))
                }
              }
              Left(NonEmptyChain.fromSeq(errors.toSeq).get)

  // Helper to determine if type is Optional
  private inline def isOptionalType[T]: Boolean = inline erasedValue[T] match
    case _: Option[?] => true
    case _ => false

  // Placeholder decoder for @Source fields - never actually invoked at runtime because
  // @Source fields produce null placeholders that are replaced by CaseClassDecoderFactory.
  private val sourceFieldPlaceholder: JAnyDecoder[Any] = (focus: JFocus[JAny]) =>
    Left(NonEmptyChain.one(InvalidValue(focus, "JSource fields should not be decoded from JSON")))

  // Helper to create decoder with Option handling.
  // JSource and Option[JSource] cases produce placeholders to avoid requiring a JAnyDecoder[JSource] given.
  private inline def createDecoder[H]: JAnyDecoder[H] =
    inline erasedValue[H] match
      case _: Option[JSource] =>
        sourceFieldPlaceholder.asInstanceOf[JAnyDecoder[H]]
      case _: JSource =>
        sourceFieldPlaceholder.asInstanceOf[JAnyDecoder[H]]
      case _: Option[t] =>
        val baseDecoder = summonInline[JAnyDecoder[t]]
        class OptionDecoder(baseDecoder: JAnyDecoder[t]) extends JAnyDecoder[Option[t]]:
          def decode(focus: JFocus[JAny]): JResult[Option[t]] =
            focus.value match
              case JNull => Right(None)
              case _ => baseDecoder.decode(focus).map(Some(_))
        new OptionDecoder(baseDecoder).asInstanceOf[JAnyDecoder[H]]
      case _ =>
        summonInline[JAnyDecoder[H]]

  // Decoder for non-empty tuples
  inline given consDecoder[H, T <: Tuple]: TupleDecoderFactory[H *: T] =
    val headDecoder = createDecoder[H]
    val tailFactory = summonInline[TupleDecoderFactory[T]]
    val isOptional = isOptionalType[H]

    // Use a named class instead of an anonymous class to prevent duplication at each inline call site.
    // Without this, the Scala 3 compiler would duplicate the entire anonymous class definition at every
    // location where this inline given is expanded, leading to code bloat.
    class TupleDecoderFactoryImpl(
        headDecoder: JAnyDecoder[H],
        tailFactory: TupleDecoderFactory[T],
        isOptional: Boolean
    ) extends TupleDecoderFactory[H *: T]:
      def apply(info: CaseClassInfo, config: Config): TupleDecoder[H *: T] =
        val tailDecoder = tailFactory(info, config)

        // Use a named class for the same reason - to prevent duplication when the factory's apply
        // method is called at multiple inline sites.
        class TupleDecoderImpl(
            headDecoder: JAnyDecoder[H],
            tailDecoder: TupleDecoder[T],
            isOptional: Boolean,
            info: CaseClassInfo,
            config: Config
        ) extends TupleDecoder[H *: T]:
          def decode(
              jobjFocus: JFocus[JObject],
              fieldNames: List[String],
              consumedFields: Set[String],
              discriminatorFields: Set[JFocus[JAny]],
              fieldSources: Map[String, JFocus[JAny]],
              fieldRoles: List[FieldRole]
          ): JResult[TupleDecoderResult[H *: T]] =
            val jobj = jobjFocus.value
            fieldNames match
              case Nil =>
                // Should not happen - fieldNames should match tuple structure
                Left(NonEmptyChain.one(InvalidValue(jobjFocus, "Field count mismatch")))
              case fieldName :: restNames =>
                val role = fieldRoles.headOption.getOrElse(FieldRole.Regular)
                val restRoles = if fieldRoles.nonEmpty then fieldRoles.tail else Nil

                role match
                  case FieldRole.SourceDirect | FieldRole.SourceOption =>
                    // @Source field: produce a null placeholder (will be replaced in CaseClassDecoderFactory)
                    // Don't consume any JSON fields, don't add to consumedFields
                    tailDecoder.decode(jobjFocus, restNames, consumedFields, discriminatorFields, fieldSources, restRoles) match
                      case Right(tailResult) =>
                        Right(TupleDecoderResult(
                          null.asInstanceOf[H] *: tailResult.value,
                          tailResult.consumedFields,
                          tailResult.fieldSources
                        ))
                      case Left(errors) =>
                        Left(errors)

                  case FieldRole.Regular =>
                    val mappedFieldName = config.fieldNameMapping(fieldName)
                    val fieldOpt = jobj.fieldList.find(_.name.value == mappedFieldName)
                    val defaultOpt = info.fieldByName(fieldName).flatMap(_._2)

                    val headResult: JResult[(H, Option[JFocus[JAny]])] = fieldOpt match
                      case Some(field) =>
                        // Find the field index in the object
                        val fieldIndex = jobj.fieldList.indexWhere(_.name.value == mappedFieldName)
                        val fieldFocus = JFieldFocus(field.value, field.name, fieldIndex, jobjFocus.asInstanceOf[JFocus[JObject]])
                        headDecoder.decode(fieldFocus).map(v => (v, Some(fieldFocus)))
                      case None =>
                        // Handle Option types and defaults
                        if isOptional then
                          Right((None.asInstanceOf[H], None))
                        else if config.useDefaultsForMissingFields && defaultOpt.isDefined then
                          Right((defaultOpt.get.asInstanceOf[H], None))
                        else
                          Left(NonEmptyChain.one(MissingField(jobjFocus, mappedFieldName)))

                    headResult match
                      case Right((head, focusOpt)) =>
                        val newConsumedFields = if fieldOpt.isDefined then consumedFields + mappedFieldName else consumedFields
                        // Track field source using the Scala field name (not the mapped JSON name)
                        val newFieldSources = focusOpt.fold(fieldSources)(f => fieldSources + (fieldName -> f))
                        tailDecoder.decode(jobjFocus, restNames, newConsumedFields, discriminatorFields, newFieldSources, restRoles) match
                          case Right(tailResult) =>
                            Right(TupleDecoderResult(
                              head *: tailResult.value,
                              tailResult.consumedFields,
                              tailResult.fieldSources
                            ))
                          case Left(errors) =>
                            Left(errors)
                      case Left(errors) =>
                        // Still try to decode the tail to accumulate all errors
                        tailDecoder.decode(jobjFocus, restNames, consumedFields, discriminatorFields, fieldSources, restRoles) match
                          case Right(_) =>
                            Left(errors)
                          case Left(tailErrors) =>
                            Left(errors ++ tailErrors)

        new TupleDecoderImpl(headDecoder, tailDecoder, isOptional, info, config)

    new TupleDecoderFactoryImpl(headDecoder, tailFactory, isOptional)
