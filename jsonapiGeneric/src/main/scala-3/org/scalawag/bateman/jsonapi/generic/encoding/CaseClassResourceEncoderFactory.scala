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

package org.scalawag.bateman.jsonapi.generic.encoding

import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.syntax.*
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields.Explicit
import org.scalawag.bateman.jsonapi.encoding.{EncodeResult, IncludeSpec, InvalidFieldName, InvalidIncludePath, ResourceEncoder, UnavailableIncludePath}
import org.scalawag.bateman.jsonapi.generic.Annotations.*
import org.scalawag.bateman.json.generic.defaults.DefaultsMacro
import shapeless3.deriving.AllAnnotations
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.{ClassTag, classTag}

trait CaseClassResourceEncoderFactory[In]:
  def apply(params: CaseClassResourceEncoderFactory.Params): ResourceEncoder[In]

object CaseClassResourceEncoderFactory:
  final case class Params(resourceTypeOverride: Option[String], config: Config, lidGenerator: LidGenerator):
    def resourceTypeFor[A: ClassTag]: String =
      resourceTypeOverride.getOrElse(config.classNameMapping(classTag[A].runtimeClass.getSimpleName))

  /** Represents the state of encoding as it iterates through the individual field encoders. This class keeps track
    * of both the partial resource being built and the field names that have been handled so that error checking can
    * be done after all fields have been processed. If [[includeSpec]] or [[fieldsSpec]] references a field that was
    * not present in either [[EncodeState.attributesHandled]] or [[EncodeState.relationshipsHandled]], an error is
    * raised.
    */
  final case class EncodeState(
      partial: PartialResource,
      attributesHandled: Set[String] = Set.empty,
      relationshipsHandled: Set[String] = Set.empty
  ):
    def fieldsHandled: Set[String] = attributesHandled ++ relationshipsHandled
    def withAttributeHandled(name: String): EncodeState = copy(attributesHandled = attributesHandled + name)
    def withRelationshipHandled(name: String): EncodeState = copy(relationshipsHandled = relationshipsHandled + name)

  enum FieldRole:
    case IdField, AttributeField, MetaField, RelationshipField, IncludedRelationshipField, TypeField, SourceField

  // Sealed trait to hold different encoder types alongside their role
  sealed trait FieldEncoderInfo
  case class IdEnc(enc: Encoder[Any, JString]) extends FieldEncoderInfo
  case class AttrEnc(enc: JAnyEncoder[Any]) extends FieldEncoderInfo
  case class MetaEnc(enc: JAnyEncoder[Any]) extends FieldEncoderInfo
  case class RelEnc(enc: JObjectEncoder[Any]) extends FieldEncoderInfo
  case class InclRelEnc(enc: ResourceEncoder[Any]) extends FieldEncoderInfo
  case object IgnoredEnc extends FieldEncoderInfo

  // Unwrap Option for summoning the base encoder type
  private inline def summonIdEncoder[H]: Encoder[Any, JString] =
    inline erasedValue[H] match
      case _: Option[h] => summonInline[Encoder[h, JString]].asInstanceOf[Encoder[Any, JString]]
      case _            => summonInline[Encoder[H, JString]].asInstanceOf[Encoder[Any, JString]]

  private inline def summonAnyEncoder[H]: JAnyEncoder[Any] =
    inline erasedValue[H] match
      case _: Option[h] => summonInline[JAnyEncoder[h]].asInstanceOf[JAnyEncoder[Any]]
      case _            => summonInline[JAnyEncoder[H]].asInstanceOf[JAnyEncoder[Any]]

  // Unwrap Option, List, Nullable layers for relationship encoder summoning
  private inline def summonRelEncoder[H]: JObjectEncoder[Any] =
    inline erasedValue[H] match
      case _: Option[h]   => summonRelEncoder[h].asInstanceOf[JObjectEncoder[Any]]
      case _: List[h]     => summonInline[JObjectEncoder[h]].asInstanceOf[JObjectEncoder[Any]]
      case _: Nullable[h] => summonInline[JObjectEncoder[h]].asInstanceOf[JObjectEncoder[Any]]
      case _              => summonInline[JObjectEncoder[H]].asInstanceOf[JObjectEncoder[Any]]

  private inline def summonResEncoder[H]: ResourceEncoder[Any] =
    inline erasedValue[H] match
      case _: Option[h]   => summonResEncoder[h].asInstanceOf[ResourceEncoder[Any]]
      case _: List[h]     => summonInline[ResourceEncoder[h]].asInstanceOf[ResourceEncoder[Any]]
      case _: Nullable[h] => summonInline[ResourceEncoder[h]].asInstanceOf[ResourceEncoder[Any]]
      case _              => summonInline[ResourceEncoder[H]].asInstanceOf[ResourceEncoder[Any]]

  // Summon encoder for each field based on its annotation type
  inline def summonEncoders[Elems <: Tuple, Annots <: Tuple]: List[FieldEncoderInfo] =
    inline erasedValue[(Elems, Annots)] match
      case _: (EmptyTuple, EmptyTuple) => Nil
      case _: (h *: ts, (Id *: _) *: as) =>
        IdEnc(summonIdEncoder[h]) :: summonEncoders[ts, as]
      case _: (h *: ts, (Attribute *: _) *: as) =>
        AttrEnc(summonAnyEncoder[h]) :: summonEncoders[ts, as]
      case _: (h *: ts, (Meta *: _) *: as) =>
        MetaEnc(summonAnyEncoder[h]) :: summonEncoders[ts, as]
      case _: (h *: ts, (Relationship *: _) *: as) =>
        RelEnc(summonRelEncoder[h]) :: summonEncoders[ts, as]
      case _: (h *: ts, (IncludedRelationship *: _) *: as) =>
        InclRelEnc(summonResEncoder[h]) :: summonEncoders[ts, as]
      case _: (h *: ts, (Type *: _) *: as) =>
        IgnoredEnc :: summonEncoders[ts, as]
      case _: (h *: ts, (Source *: _) *: as) =>
        IgnoredEnc :: summonEncoders[ts, as]
      case _: (h *: ts, EmptyTuple *: as) =>
        IgnoredEnc :: summonEncoders[ts, as]

  inline given derived[T](using m: Mirror.ProductOf[T], ct: ClassTag[T], aa: AllAnnotations[T]): CaseClassResourceEncoderFactory[T] =
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    val defaults = DefaultsMacro.extractDefaults[T]
    val encoders = summonEncoders[m.MirroredElemTypes, aa.Out]

    // Use a named class to prevent duplication at each inline call site.
    class CaseClassResourceEncoderFactoryImpl(
        labels: List[String],
        defaults: Product,
        encoders: List[FieldEncoderInfo],
        ct: ClassTag[T]
    ) extends CaseClassResourceEncoderFactory[T]:
      def apply(params: Params): ResourceEncoder[T] =
        val info = CaseClassInfo(defaults, labels)
        val resourceType = params.resourceTypeFor[T](using ct)

        // Use a named class to prevent duplication when the factory's apply is called at multiple sites.
        class ResourceEncoderImpl(
            info: CaseClassInfo,
            encoders: List[FieldEncoderInfo],
            resourceType: String,
            params: Params
        ) extends ResourceEncoder[T]:
          def encodeResource(
              in: T,
              includeSpec: IncludeSpec,
              fieldsSpec: FieldsSpec,
              discriminators: JObject
          ): EncodeResult[ResourceEncoder.Encoded] =
            val product = in.asInstanceOf[Product]

            val state =
              labels.indices.reverse.foldLeft(EncodeState(PartialResource(resourceType))) { (state, i) =>
                val scalaFieldName = labels(i)
                val jsonFieldName = params.config.fieldNameMapping(scalaFieldName)
                val fieldValue = product.productElement(i)
                val defaultOpt = info.fieldByName(scalaFieldName).flatMap(_._2)
                val encoderInfo = encoders(i)

                encoderInfo match
                  case IdEnc(enc) =>
                    // @Id field - always encode (mustBeEncoded = true)
                    val updatedPartial = unwrapOption(fieldValue).fold(state.partial) { v =>
                      val encoded = enc.encode(v)
                      state.partial.copy(id = Some(encoded.value))
                    }
                    state.copy(partial = updatedPartial)

                  case AttrEnc(enc) =>
                    val encodedOpt = unwrapOption(fieldValue).map(enc.encode)
                    val encodedDefaultOpt = defaultOpt.map(d => unwrapOption(d).map(enc.encode))
                    val include = shouldIncludeField(
                      fieldsSpec.forResourceType(resourceType),
                      jsonFieldName,
                      canBeExcluded = true,
                      params.config.encodeDefaultValues,
                      encodedDefaultOpt,
                      encodedOpt
                    )
                    val updatedPartial = if include then
                      encodedOpt.fold(state.partial)(v => state.partial.addAttribute(jsonFieldName, v))
                    else state.partial
                    state.copy(partial = updatedPartial).withAttributeHandled(jsonFieldName)

                  case MetaEnc(enc) =>
                    val encodedOpt = unwrapOption(fieldValue).map(enc.encode)
                    val encodedDefaultOpt = defaultOpt.map(d => unwrapOption(d).map(enc.encode))
                    val include = shouldIncludeField(
                      fieldsSpec.forResourceType(resourceType),
                      jsonFieldName,
                      canBeExcluded = false,
                      params.config.encodeDefaultValues,
                      encodedDefaultOpt,
                      encodedOpt
                    )
                    val updatedPartial = if include then
                      encodedOpt.fold(state.partial)(v => state.partial.addMeta(jsonFieldName, v))
                    else state.partial
                    state.copy(partial = updatedPartial)

                  case RelEnc(enc) =>
                    val updatedPartial = encodeRelationship(
                      state.partial, fieldValue, defaultOpt, enc, jsonFieldName,
                      resourceType, includeSpec, fieldsSpec, params
                    )
                    state.copy(partial = updatedPartial).withRelationshipHandled(jsonFieldName)

                  case InclRelEnc(enc) =>
                    val updatedPartial = encodeIncludedRelationship(
                      state.partial, fieldValue, defaultOpt, enc, jsonFieldName,
                      resourceType, includeSpec, fieldsSpec, params
                    )
                    state.copy(partial = updatedPartial).withRelationshipHandled(jsonFieldName)

                  case IgnoredEnc => state // @Type or @Source - skip
              }

            // Check for invalid include paths
            val invalidIncludePathErrors =
              includeSpec.explicitChildren.filterNot(state.relationshipsHandled).map(includeSpec.descend).flatMap {
                case always: IncludeSpec.Always => Some(InvalidIncludePath(always.path))
                case _ => None
              }

            // Check for invalid field names
            val invalidFieldErrors =
              fieldsSpec.forResourceType(resourceType) match
                case Explicit(names) => (names -- state.fieldsHandled).map(InvalidFieldName(resourceType, _))
                case _               => Set.empty

            val finalPartial = (invalidFieldErrors ++ invalidIncludePathErrors).foldLeft(state.partial)(_.addError(_))

            val finalPartialWithDisc =
              if (discriminators.fieldList.nonEmpty) finalPartial.withDiscriminators(discriminators)
              else finalPartial
            finalPartialWithDisc.toEncoded

        new ResourceEncoderImpl(info, encoders, resourceType, params)

    new CaseClassResourceEncoderFactoryImpl(labels, defaults, encoders, ct)

  // Helper: unwrap Option layer, returning Some(inner) or None
  private def unwrapOption(value: Any): Option[Any] = value match
    case None        => None
    case Some(inner) => Some(inner)
    case other       => Some(other)

  private def shouldIncludeField[A](
      fields: Fields,
      fieldName: String,
      canBeExcluded: Boolean,
      encodeDefaults: Boolean,
      encodedDefault: Option[A],
      encodedHead: A
  ): Boolean =
    fields match
      case Explicit(fieldSet) if canBeExcluded =>
        fieldSet(fieldName)
      case fields =>
        fields(fieldName) && (encodeDefaults || !encodedDefault.contains(encodedHead))

  // Check if a cardinality-wrapped value is "empty" (for default comparison filtering).
  // Only empty defaults should be used for relationship default comparison.
  private def isEmptyCardinality(value: Any): Boolean = value match
    case None           => true
    case Null           => true
    case list: List[_]  => list.isEmpty
    case _              => false

  private def relationshipData(fa: Any): JAny = fa match
    case Null                          => JNull
    case NotNull(ri: JObject)          => ri
    case ri: JObject                   => ri
    case ris: List[JObject @unchecked] => JArray(ris*)

  private def encodeRelationship(
      partial: PartialResource,
      fieldValue: Any,
      defaultOpt: Option[Any],
      enc: JObjectEncoder[Any],
      jsonFieldName: String,
      resourceType: String,
      includeSpec: IncludeSpec,
      fieldsSpec: FieldsSpec,
      params: Params
  ): PartialResource =
    includeSpec.descend(jsonFieldName) match
      case child: IncludeSpec.Always =>
        partial.addError(UnavailableIncludePath(child.path))
      case _ =>
        val optValue = unwrapOption(fieldValue)
        val encodedHead = optValue.map(encodeCardinality(_, enc))
        // The only default that can be excluded from encoding here is an empty value.
        // Throw away any non-empty default (matching Scala 2 behavior).
        val encodedDefault = defaultOpt.map(d => unwrapOption(d).map(encodeCardinality(_, enc)))
          .filter(_.exists(isEmptyCardinality))

        val include = shouldIncludeField(
          fieldsSpec.forResourceType(resourceType),
          jsonFieldName,
          canBeExcluded = true,
          params.config.encodeDefaultValues,
          encodedDefault,
          encodedHead
        )

        if include then
          partial.addRelationship(
            jsonFieldName,
            encodedHead.map(relationshipData(_)).map(v => JObject("data" -> v))
          )
        else
          partial

  // Encode a value that might be wrapped in a cardinality type (Single, Nullable, List)
  private def encodeCardinality(value: Any, enc: JObjectEncoder[Any]): Any = value match
    case Null            => Null
    case NotNull(inner)  => NotNull(enc.encode(inner, JObject.Empty))
    case list: List[_]   => list.map(item => enc.encode(item, JObject.Empty))
    case single          => enc.encode(single, JObject.Empty)

  private def encodeIncludedRelationship(
      partial: PartialResource,
      fieldValue: Any,
      defaultOpt: Option[Any],
      enc: ResourceEncoder[Any],
      jsonFieldName: String,
      resourceType: String,
      includeSpec: IncludeSpec,
      fieldsSpec: FieldsSpec,
      params: Params
  ): PartialResource =
    val optValue = unwrapOption(fieldValue)

    includeSpec.descend(jsonFieldName) match
      case IncludeSpec.Never if !fieldsSpec.forResourceType(resourceType)(jsonFieldName) =>
        // Field and path both excluded
        partial

      case IncludeSpec.Never =>
        // Path excluded but field included - minimally encode to get identifiers
        val encodedHead = optValue.map(encodeCardinalityMinimally(_, enc))
        // The only default that can be excluded from encoding here is an empty value.
        val encodedDefault = defaultOpt.map(d => unwrapOption(d).map(encodeCardinalityMinimally(_, enc)))
          .filter(_.exists(isEmptyCardinality))

        val include = shouldIncludeField(
          fieldsSpec.forResourceType(resourceType),
          jsonFieldName,
          canBeExcluded = true,
          params.config.encodeDefaultValues,
          encodedDefault,
          encodedHead
        )

        if include then
          partial.addRelationship(
            jsonFieldName,
            encodedHead.map(relationshipData(_)).map(v => JObject("data" -> v))
          )
        else
          partial

      case childIncludeSpec =>
        // Path is included - fully encode with includes
        optValue match
          case None =>
            partial

          case Some(cardinalityValue) =>
            encodeIncludedItems(
              partial, cardinalityValue, defaultOpt, enc, jsonFieldName,
              resourceType, childIncludeSpec, fieldsSpec, params
            )

  private def encodeCardinalityMinimally(value: Any, enc: ResourceEncoder[Any]): Any = value match
    case Null            => Null
    case NotNull(inner)  => NotNull(enc.encodeMinimally(inner))
    case list: List[_]   => list.map(item => enc.encodeMinimally(item))
    case single          => enc.encodeMinimally(single)

  private def encodeIncludedItems(
      partial: PartialResource,
      cardinalityValue: Any,
      defaultOpt: Option[Any],
      enc: ResourceEncoder[Any],
      jsonFieldName: String,
      resourceType: String,
      childIncludeSpec: IncludeSpec,
      fieldsSpec: FieldsSpec,
      params: Params
  ): PartialResource =
    // Encode all items in the cardinality wrapper. Child resources start with empty discriminators
    // since they are independent resources, not part of this resource's discriminator hierarchy.
    val encodedResults: EncodeResult[Any] = cardinalityValue match
      case Null =>
        Right(Null)
      case NotNull(inner) =>
        enc.encodeResource(inner, childIncludeSpec, fieldsSpec, JObject.Empty)
          .map(encoded => NotNull(encoded))
      case list: List[_] =>
        import cats.syntax.traverse.*
        list.map(item => enc.encodeResource(item, childIncludeSpec, fieldsSpec, JObject.Empty))
          .sequence
      case single =>
        enc.encodeResource(single, childIncludeSpec, fieldsSpec, JObject.Empty)

    partial.whenValid(encodedResults) { encodedItems =>
      // The only default that can be excluded from encoding here is an empty value.
      val encodedDefault = defaultOpt.map(d => unwrapOption(d).map(encodeCardinalityMinimally(_, enc)))
        .filter(_.exists(isEmptyCardinality))

      val include = shouldIncludeField(
        fieldsSpec.forResourceType(resourceType),
        jsonFieldName,
        canBeExcluded = true,
        params.config.encodeDefaultValues,
        encodedDefault,
        Some(encodedItems)
      )

      if include then
        encodedItems match
          case Null =>
            partial.addRelationship(jsonFieldName, JObject("data" -> JNull))
          case _ =>
            processEncodedIncludes(partial, encodedItems, jsonFieldName, params)
      else
        partial
    }

  private def processEncodedIncludes(
      partial: PartialResource,
      encodedItems: Any,
      jsonFieldName: String,
      params: Params
  ): PartialResource =
    import org.scalawag.bateman.json.lens.stringToLens
    import org.scalawag.bateman.json.RichJResult
    import cats.syntax.either.*

    def processEncoded(encoded: ResourceEncoder.Encoded): JResult[(JObject, ResourceEncoder.Encoded)] =
      val ro = encoded.resourceObject
      ro.id match
        case Some(idVal) if !ro.localId =>
          val ri = JObject("type" -> ro.resourceType.toJAny, "id" -> idVal.toJAny)
          (ri -> encoded).rightNec
        case _ =>
          val lid = params.lidGenerator()
          val ri = JObject("type" -> ro.resourceType.toJAny, "lid" -> lid.toJAny)
          val f = encoded.root.asRootFocus
          val updatedRoot = f.asObject.flatMap(_.encodeTo("lid", lid, overwrite = true)).getOrThrow.value
          val updatedRo = ro.copy(id = Some(lid), localId = true)
          (ri -> encoded.copy(root = updatedRoot, resourceObject = updatedRo)).rightNec

    import cats.syntax.traverse.*

    // Process encoded items and build relationship data preserving cardinality structure.
    encodedItems match
      case NotNull(e: ResourceEncoder.Encoded) =>
        processEncoded(e).map { case (ri, enc) =>
          partial
            .addRelationship(jsonFieldName, JObject("data" -> ri))
            .addInclusions(List(enc))
        }.getOrThrow
      case list: List[ResourceEncoder.Encoded @unchecked] =>
        list.map(processEncoded).sequence
          .map(_.unzip)
          .map { case (ids, encodedObjects) =>
            partial
              .addRelationship(jsonFieldName, JObject("data" -> JArray(ids*)))
              .addInclusions(encodedObjects)
          }
          .getOrThrow
      case e: ResourceEncoder.Encoded =>
        processEncoded(e).map { case (ri, enc) =>
          partial
            .addRelationship(jsonFieldName, JObject("data" -> ri))
            .addInclusions(List(enc))
        }.getOrThrow
      case _ =>
        partial
