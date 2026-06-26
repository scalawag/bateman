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
import cats.syntax.parallel.*
import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.lens.*
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.defaults.DefaultsMacro
import org.scalawag.bateman.jsonapi.{JsonApiTypeMismatch, MissingIncludedResourceObject, lens as japiLens}
import org.scalawag.bateman.jsonapi.generic.Annotations.*
import shapeless3.deriving.AllAnnotations
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

trait CaseClassResourceDecoderFactory[B]:
  def apply(config: Config, resourceTypeOverride: Option[String]): JObjectDecoder[B]

object CaseClassResourceDecoderFactory:
  enum FieldRole:
    case IdField, AttributeField, MetaField, RelationshipField, IncludedRelationshipField, TypeField, SourceField

  sealed trait FieldDecoderInfo
  case class IdDec(dec: JStringDecoder[Any]) extends FieldDecoderInfo
  case class AttrDec(dec: JAnyDecoder[Any]) extends FieldDecoderInfo
  case class MetaDec(dec: JAnyDecoder[Any]) extends FieldDecoderInfo
  case class RelDec(dec: JObjectDecoder[Any]) extends FieldDecoderInfo
  case class InclRelDec(dec: JObjectDecoder[Any]) extends FieldDecoderInfo
  case class TypeDec(dec: JStringDecoder[Any]) extends FieldDecoderInfo
  case object SourceDec extends FieldDecoderInfo
  case object IgnoredDec extends FieldDecoderInfo

  // Whether field type is Optional
  private inline def isOptional[H]: Boolean =
    inline erasedValue[H] match
      case _: Option[?] => true
      case _            => false

  private inline def summonStringDecoder[H]: JStringDecoder[Any] =
    inline erasedValue[H] match
      case _: Option[h] => summonInline[JStringDecoder[h]].asInstanceOf[JStringDecoder[Any]]
      case _            => summonInline[JStringDecoder[H]].asInstanceOf[JStringDecoder[Any]]

  private inline def summonAnyDecoder[H]: JAnyDecoder[Any] =
    inline erasedValue[H] match
      case _: Option[h] => summonInline[JAnyDecoder[h]].asInstanceOf[JAnyDecoder[Any]]
      case _            => summonInline[JAnyDecoder[H]].asInstanceOf[JAnyDecoder[Any]]

  // For relationships - unwrap Option, List, Nullable to get to the base decoder
  private inline def summonRelDecoder[H]: JObjectDecoder[Any] =
    inline erasedValue[H] match
      case _: Option[h]   => summonRelDecoder[h].asInstanceOf[JObjectDecoder[Any]]
      case _: List[h]     => summonInline[JObjectDecoder[h]].asInstanceOf[JObjectDecoder[Any]]
      case _: Nullable[h] => summonInline[JObjectDecoder[h]].asInstanceOf[JObjectDecoder[Any]]
      case _              => summonInline[JObjectDecoder[H]].asInstanceOf[JObjectDecoder[Any]]

  // Determine the cardinality shape at compile time
  enum CardinalityShape:
    case Direct, OptionDirect, OptionList, OptionNullable, DirectList, DirectNullable

  private inline def getCardinalityShape[H]: CardinalityShape =
    inline erasedValue[H] match
      case _: Option[List[?]]     => CardinalityShape.OptionList
      case _: Option[Nullable[?]] => CardinalityShape.OptionNullable
      case _: Option[?]          => CardinalityShape.OptionDirect
      case _: List[?]            => CardinalityShape.DirectList
      case _: Nullable[?]        => CardinalityShape.DirectNullable
      case _                     => CardinalityShape.Direct

  case class FieldInfo(
      decoder: FieldDecoderInfo,
      isOpt: Boolean,
      cardinalityShape: CardinalityShape
  )

  inline def summonDecoders[Elems <: Tuple, Annots <: Tuple]: List[FieldInfo] =
    inline erasedValue[(Elems, Annots)] match
      case _: (EmptyTuple, EmptyTuple) => Nil
      case _: (h *: ts, (Id *: _) *: as) =>
        FieldInfo(IdDec(summonStringDecoder[h]), isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]
      case _: (h *: ts, (Attribute *: _) *: as) =>
        FieldInfo(AttrDec(summonAnyDecoder[h]), isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]
      case _: (h *: ts, (Meta *: _) *: as) =>
        FieldInfo(MetaDec(summonAnyDecoder[h]), isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]
      case _: (h *: ts, (Relationship *: _) *: as) =>
        FieldInfo(RelDec(summonRelDecoder[h]), isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]
      case _: (h *: ts, (IncludedRelationship *: _) *: as) =>
        FieldInfo(InclRelDec(summonRelDecoder[h]), isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]
      case _: (h *: ts, (Type *: _) *: as) =>
        FieldInfo(TypeDec(summonStringDecoder[h]), isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]
      case _: (h *: ts, (Source *: _) *: as) =>
        FieldInfo(SourceDec, isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]
      case _: (h *: ts, EmptyTuple *: as) =>
        FieldInfo(SourceDec, isOptional[h], getCardinalityShape[h]) :: summonDecoders[ts, as]

  inline given derived[T](using m: Mirror.ProductOf[T], ct: ClassTag[T], aa: AllAnnotations[T]): CaseClassResourceDecoderFactory[T] =
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val defaults = DefaultsMacro.extractDefaults[T]
    val decoders = summonDecoders[m.MirroredElemTypes, aa.Out]

    // Use a named class to prevent duplication at each inline call site.
    class CaseClassResourceDecoderFactoryImpl(
        labels: List[String],
        defaults: Product,
        decoders: List[FieldInfo],
        ct: ClassTag[T]
    ) extends CaseClassResourceDecoderFactory[T]:
      def apply(config: Config, resourceTypeOverride: Option[String]): JObjectDecoder[T] =
        val info = CaseClassInfo(defaults, labels)
        val resType = resourceTypeOverride.getOrElse(
          config.classNameMapping(ct.runtimeClass.getSimpleName)
        )

        // Use a named class to prevent duplication when the factory's apply is called at multiple sites.
        class JObjectDecoderImpl(
            info: CaseClassInfo,
            decoders: List[FieldInfo],
            resType: String,
            config: Config
        ) extends JObjectDecoder[T]:
          def decode(jobjFocus: JFocus[JObject], discriminatorFieldFocuses: Set[JFocus[JAny]]): JResult[T] =
            val jobj = jobjFocus.value

            // Type check
            val typeCheck: JResult[Unit] =
              jobjFocus(japiLens.resourceType).flatMap { cursor =>
                val inType = cursor
                if inType.value.value != resType then
                  JsonApiTypeMismatch(inType, resType).leftNec
                else
                  ().rightNec
              }

            // Decode all fields
            val fieldSources = scala.collection.mutable.Map.empty[String, JFocus[JAny]]
            val fieldResults: List[JResult[Any]] = labels.zip(decoders).map { case (scalaFieldName, fieldInfo) =>
              val jsonFieldName = config.fieldNameMapping(scalaFieldName)
              val defaultOpt = info.fieldByName(scalaFieldName).flatMap(_._2)

              fieldInfo.decoder match
                case TypeDec(dec) =>
                  decodeTypeLens(jobjFocus, japiLens.resourceType, dec, fieldInfo, defaultOpt, config, scalaFieldName, fieldSources)

                case IdDec(dec) =>
                  decodeTypeLens(jobjFocus, japiLens.id, dec, fieldInfo, defaultOpt, config, scalaFieldName, fieldSources)

                case AttrDec(dec) =>
                  decodeFieldLens(jobjFocus, japiLens.attribute(jsonFieldName), dec, fieldInfo, defaultOpt, config, scalaFieldName, fieldSources)

                case MetaDec(dec) =>
                  decodeFieldLens(jobjFocus, japiLens.meta(jsonFieldName), dec, fieldInfo, defaultOpt, config, scalaFieldName, fieldSources)

                case RelDec(dec) =>
                  decodeRelationship(jobjFocus, jsonFieldName, dec, fieldInfo, defaultOpt, config, scalaFieldName, fieldSources)

                case InclRelDec(dec) =>
                  decodeIncludedRelationship(jobjFocus, jsonFieldName, dec, fieldInfo, defaultOpt, config, scalaFieldName, fieldSources)

                case SourceDec =>
                  val src = JSource(jobjFocus, fieldSources.toMap)
                  if fieldInfo.isOpt then
                    Right(Some(src))
                  else
                    Right(src)

                case IgnoredDec =>
                  Right(null)
            }

            // Combine type check with field results
            val combinedResult = fieldResults.foldLeft(typeCheck.map(_ => List.empty[Any])) { (acc, fieldResult) =>
              (acc, fieldResult).parMapN { (list, value) => list :+ value }
            }

            val unknownFieldCheck: JResult[Unit] =
              if config.allowUnknownFields then
                ().rightNec
              else
                val consumed = fieldSources.values.toSet

                val topLevelWhiteList = Set("type", "meta", "links", "attributes", "relationships")
                val unknownTopLevelFoci = jobjFocus.value.fieldList.map(_.name.value).zip(jobjFocus.fields)
                  .filter { (name, focus) =>
                    !topLevelWhiteList.contains(name) &&
                      !discriminatorFieldFocuses.contains(focus) &&
                      !consumed.contains(focus)
                  }
                  .map(_(1))

                List(
                  jobjFocus(japiLens.meta.? ~> **).map(_.foci),
                  jobjFocus(japiLens.attributes.? ~> **).map(_.foci),
                  jobjFocus(japiLens.relationships.? ~> **).map(_.foci),
                ).parFlatSequence.flatMap { containerValues =>
                  val unconsumedInContainers = containerValues.filterNot(consumed)
                  rightIfEmpty((unknownTopLevelFoci ++ unconsumedInContainers).map(UnexpectedValue(_)), ())
                }

            (combinedResult, unknownFieldCheck).parMapN { (values, _) =>
              val tuple = Tuple.fromArray(values.toArray)
              m.fromTuple(tuple.asInstanceOf[m.MirroredElemTypes])
            }.leftMap(_.distinct)

        new JObjectDecoderImpl(info, decoders, resType, config)

    new CaseClassResourceDecoderFactoryImpl(labels, defaults, decoders, ct)

  private def decodeTypeLens[J <: JAny](
      jobjFocus: JFocus[JObject],
      lens: CreatableJLens[JObject, J],
      dec: Decoder[J, Any],
      fieldInfo: FieldInfo,
      defaultOpt: Option[Any],
      config: Config,
      scalaFieldName: String,
      fieldSources: scala.collection.mutable.Map[String, JFocus[JAny]]
  ): JResult[Any] =
    jobjFocus(lens.?).flatMap { optFocus =>
      optFocus.foci match
        case Some(focus) =>
          fieldSources(scalaFieldName) = focus.asInstanceOf[JFocus[JAny]]
          if fieldInfo.isOpt then
            focus.decode(dec).map(Some(_))
          else
            focus.decode(dec)
        case None =>
          if fieldInfo.isOpt then None.rightNec
          else if config.useDefaultsForMissingFields && defaultOpt.isDefined then defaultOpt.get.rightNec
          else jobjFocus(lens).flatMap(_.decode(dec)) // Will produce a proper error
    }

  private def decodeFieldLens(
      jobjFocus: JFocus[JObject],
      lens: CreatableJLens[JObject, JAny],
      dec: JAnyDecoder[Any],
      fieldInfo: FieldInfo,
      defaultOpt: Option[Any],
      config: Config,
      scalaFieldName: String,
      fieldSources: scala.collection.mutable.Map[String, JFocus[JAny]]
  ): JResult[Any] =
    jobjFocus(lens.?).flatMap { optFocus =>
      optFocus.foci match
        case Some(focus) =>
          fieldSources(scalaFieldName) = focus
          if fieldInfo.isOpt then
            focus.decode(dec).map(Some(_))
          else
            focus.decode(dec)
        case None =>
          if fieldInfo.isOpt then None.rightNec
          else if config.useDefaultsForMissingFields && defaultOpt.isDefined then defaultOpt.get.rightNec
          else jobjFocus(lens).flatMap(_.decode(dec)) // Will produce a proper error
    }

  private def decodeRelationship(
      jobjFocus: JFocus[JObject],
      jsonFieldName: String,
      dec: JObjectDecoder[Any],
      fieldInfo: FieldInfo,
      defaultOpt: Option[Any],
      config: Config,
      scalaFieldName: String,
      fieldSources: scala.collection.mutable.Map[String, JFocus[JAny]]
  ): JResult[Any] =
    val relLens = japiLens.relationship(jsonFieldName)
    jobjFocus(relLens.?).flatMap { optFocus =>
      optFocus.foci match
        case Some(relFocus) =>
          fieldSources(scalaFieldName) = relFocus.asInstanceOf[JFocus[JAny]]
          fieldInfo.cardinalityShape match
            case CardinalityShape.Direct =>
              relFocus(japiLens.data ~> narrowTo[JObject]).flatMap(_.decode(dec))
            case CardinalityShape.OptionDirect =>
              relFocus(japiLens.data ~> narrowTo[JObject]).flatMap(_.decode(dec)).map(Some(_))
            case CardinalityShape.DirectNullable =>
              val nullableDec: JAnyDecoder[Nullable[Any]] = Decoder.nullableDecoder(Decoder.widenJObjectDecoder(dec))
              relFocus(japiLens.data).flatMap(_.decode(nullableDec))
            case CardinalityShape.OptionNullable =>
              val nullableDec: JAnyDecoder[Nullable[Any]] = Decoder.nullableDecoder(Decoder.widenJObjectDecoder(dec))
              relFocus(japiLens.data).flatMap(_.decode(nullableDec)).map(Some(_))
            case CardinalityShape.DirectList =>
              relFocus(japiLens.data ~> * ~> narrowTo[JObject]).flatMap(_.decode(dec))
            case CardinalityShape.OptionList =>
              relFocus(japiLens.data ~> * ~> narrowTo[JObject]).flatMap(_.decode(dec)).map(Some(_))
        case None =>
          if fieldInfo.isOpt then None.rightNec
          else if config.useDefaultsForMissingFields && defaultOpt.isDefined then defaultOpt.get.rightNec
          else jobjFocus(relLens).flatMap(_ => MissingField(jobjFocus, jsonFieldName).leftNec)
    }

  private def decodeIncludedRelationship(
      jobjFocus: JFocus[JObject],
      jsonFieldName: String,
      dec: JObjectDecoder[Any],
      fieldInfo: FieldInfo,
      defaultOpt: Option[Any],
      config: Config,
      scalaFieldName: String,
      fieldSources: scala.collection.mutable.Map[String, JFocus[JAny]]
  ): JResult[Any] =
    val relLens = japiLens.relationship(jsonFieldName)
    jobjFocus(relLens.?).flatMap { optFocus =>
      optFocus.foci match
        case Some(relFocus) =>
          fieldSources(scalaFieldName) = relFocus.asInstanceOf[JFocus[JAny]]
          fieldInfo.cardinalityShape match
            case CardinalityShape.Direct =>
              relFocus(japiLens.data ~> narrowTo[JObject]).flatMap(includedOrStub(_, dec))
            case CardinalityShape.OptionDirect =>
              relFocus(japiLens.data ~> narrowTo[JObject]).flatMap(includedOrStub(_, dec)).map(Some(_))
            case CardinalityShape.DirectNullable =>
              nullableIncludedOrStub(relFocus, dec)
            case CardinalityShape.OptionNullable =>
              nullableIncludedOrStub(relFocus, dec).map(Some(_))
            case CardinalityShape.DirectList =>
              relFocus(japiLens.data ~> * ~> japiLens.includedRef).flatMap(_.decode(dec))
            case CardinalityShape.OptionList =>
              relFocus(japiLens.data ~> * ~> japiLens.includedRef).flatMap(_.decode(dec)).map(Some(_))
        case None =>
          if fieldInfo.isOpt then None.rightNec
          else if config.useDefaultsForMissingFields && defaultOpt.isDefined then defaultOpt.get.rightNec
          else jobjFocus(relLens).flatMap(_ => MissingField(jobjFocus, jsonFieldName).leftNec)
    }

  /** Resolves the resource object referred to by a relationship datum (a resource identifier) and decodes it. If the
    * referenced object is absent from the document's `included` array, it may still be unnecessary: a related type
    * that requires no fields of its own can be reconstructed from the resource identifier alone. In that case we
    * decode directly from the identifier. Only if a required field is genuinely missing below the identifier do we
    * report the original [[MissingIncludedResourceObject]] error. (Restores the behavior of commit dfe1496d, which
    * was lost in the lens-based rewrite.)
    */
  private def includedOrStub(datum: JFocus[JObject], decoder: JObjectDecoder[Any]): JResult[Any] =
    datum(japiLens.includedRef).flatMap(_.decode(decoder)) match
      case Left(errors) if errors.forall(_.isInstanceOf[MissingIncludedResourceObject]) =>
        datum.decode(decoder) match
          case Left(stubErrors) if stubErrors.forall(missingValueBelow(datum)) => errors.asLeft
          case decoded                                                         => decoded
      case decoded => decoded

  /** Same as [[includedOrStub]] but yields a [[Nullable]], deferring to `nullableIncludedRef` (so a null datum
    * decodes to `Null`) and only applying the stub fallback when the datum is an object.
    */
  private def nullableIncludedOrStub(relFocus: JFocus[JObject], decoder: JObjectDecoder[Any]): JResult[Nullable[Any]] =
    relFocus(japiLens.data ~> narrowTo[JObject]) match
      case Right(datum) => includedOrStub(datum, decoder).map(h => NotNull(h): Nullable[Any])
      case Left(_) =>
        val nullableDec: JAnyDecoder[Nullable[Any]] = Decoder.nullableDecoder(Decoder.widenJObjectDecoder(decoder))
        relFocus(japiLens.data ~> japiLens.nullableIncludedRef).flatMap(_.decode(nullableDec))

  /** True if the error indicates a value that was simply absent (rather than malformed) at or below the given focus. */
  private def missingValueBelow(here: JFocus[JObject])(error: JError): Boolean =
    error.isInstanceOf[MissingValue] && error.pointer.tokens.startsWith(here.pointer.tokens)
