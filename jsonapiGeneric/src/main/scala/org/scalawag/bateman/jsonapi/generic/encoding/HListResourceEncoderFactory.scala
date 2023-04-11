// bateman -- Copyright 2021-2023 -- Justin Patterson
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

import cats.Traverse
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.syntax.foldable._
import org.scalawag.bateman.json.focus.Single
import org.scalawag.bateman.json.{
  Encoder,
  JAny,
  JArray,
  JNull,
  JObject,
  JObjectEncoder,
  JString,
  NotNull,
  Null,
  RichJResult
}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.generic.{Cardinality, CaseClassInfo, Config, Source}
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields.Explicit
import org.scalawag.bateman.jsonapi.encoding.{FieldsSpec, ResourceEncoder}
import org.scalawag.bateman.jsonapi.encoding.{IncludeSpec, InvalidFieldName, InvalidIncludePath, UnavailableIncludePath}
import org.scalawag.bateman.jsonapi.generic.Annotations.{Attribute, Id, IncludedRelationship, Meta, Relationship, Type}
import org.scalawag.bateman.jsonapi.generic.encoding.HListResourceEncoderFactory.Params
import shapeless.{::, HList, HNil, Lazy}
import scala.reflect.{ClassTag, classTag}

trait HListResourceEncoderFactory[In <: HList, Defaults <: HList, Annot <: HList] {
  def apply(typeInfo: CaseClassInfo[Defaults], params: Params): HListResourceEncoder[In, Defaults, Annot]
}

/** Generally speaking, all of the instances in here are derived for HLists that have an Option[H] as the head and a
  * JAnyEncoder instance for H. The specific instance derived depends on the head of the annotation for that
  * particular field. HLists that have merely an H as the head (_not_ an Option[H]) are supported through the
  * [[somify]] conversion, which make any of the normal instances work for a non-Option head.
  *
  * The following type parameters have consistent meaning throughout the instances:
  *  - F = the cardinality of the field (Id, Option or List)
  *  - H = the head of the input HList
  *  - T = the tail of the input HList
  *  - DT = the tail of the default value HList
  *  - AH = the annotations on the head of the HList
  *  - AT = the annotations on the tail of the HList
  */

object HListResourceEncoderFactory {
  final case class Params(resourceTypeOverride: Option[String], config: Config, lidGenerator: LidGenerator) {
    def resourceTypeFor[A: ClassTag]: String =
      resourceTypeOverride.getOrElse(config.classNameMapping(classTag[A].runtimeClass.getSimpleName))
  }

  final case class Input[In <: HList](
      in: In,
      resourceType: String,
      includeSpec: IncludeSpec,
      fieldsSpec: FieldsSpec,
      attributesHandled: Set[String] = Set.empty,
      relationshipsHandled: Set[String] = Set.empty
  ) {
    def fieldsHandled: Set[String] = attributesHandled ++ relationshipsHandled
    def withAttributeHandled(name: String): Input[In] = this.copy(attributesHandled = attributesHandled + name)
    def withRelationshipHandled(name: String): Input[In] = this.copy(relationshipsHandled = relationshipsHandled + name)
  }

  object Input {
    implicit class InputOps[H, T <: HList](input: Input[H :: T]) {
      def tail: Input[T] = input.copy(in = input.in.tail)
      def somifyHead: Input[Option[H] :: T] = input.copy(in = Some(input.in.head) :: input.in.tail)
    }
  }

  implicit val forHNil: HListResourceEncoderFactory[HNil, HNil, HNil] = { (_, _) => (input, _) =>
    import input._
    // If we got any paths that don't have corresponding relationships, that's an error.
    val invalidIncludePathErrors =
      includeSpec.explicitChildren.filterNot(relationshipsHandled).map(includeSpec.descend) collect {
        case always: IncludeSpec.Always => InvalidIncludePath(always.path)
      }

    // If we have any fields specified that aren't actually fields on this resource type, that's an error.
    val invalidFieldErrors =
      input.fieldsSpec.forResourceType(input.resourceType) match {
        case Explicit(names) => (names -- input.fieldsHandled).map(InvalidFieldName(input.resourceType, _))
        case _               => Set.empty
      }

    val errors = invalidFieldErrors ++ invalidIncludePathErrors

    PartialResource(resourceType, errors = errors.toSet)
  }

  implicit def forIdHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[Encoder[H, JString]],
      tailEncoder: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, (Id :: HNil) :: AT] =
    encodeSimpleHCons[H, T, DT, Id, AT, JString](true, false) { (henc, partial, _) =>
      partial.copy(id = Some(henc.value))
    }

  implicit def forAttributeHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[Encoder[H, JAny]],
      tailEncoderFactoryFactory: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, (Attribute :: HNil) :: AT] =
    encodeSimpleHCons[H, T, DT, Attribute, AT, JAny](false, true) { (henc, partial, name) =>
      partial.addAttribute(name, henc)
    }

  implicit def forMetaHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[Encoder[H, JAny]],
      tailEncoderFactoryFactory: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, (Meta :: HNil) :: AT] =
    encodeSimpleHCons[H, T, DT, Meta, AT, JAny](false, false) { (henc, partial, name) =>
      partial.addMeta(name, henc)
    }

  /** This supports the common behavior of all fields that are simply encoded into the resource object.
    * That basically means everything except relationships, which are a little trickier, and source, which is
    * not even this tricky.
    */
  private def encodeSimpleHCons[H, T <: HList, DT <: HList, AH, AT <: HList, J <: JAny](
      mustBeEncoded: Boolean,
      isAttribute: Boolean,
  )(encodeHead: (J, PartialResource, String) => PartialResource)(implicit
      headEncoder: Lazy[Encoder[H, J]],
      tailEncoderFactory: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, (AH :: HNil) :: AT] =
    (caseClassInfo, params) => {
      val scalaFieldName = caseClassInfo.fieldNames.head
      lazy val encodedDefault = caseClassInfo.defaults.head.map(_.map(headEncoder.value.encode))
      val jsonFieldName = params.config.fieldNameMapping(scalaFieldName)
      val tailEncoder = tailEncoderFactory(caseClassInfo.tail, params)

      (input, discriminators) => {
        val inputWithFieldHandled = if (isAttribute) input.withAttributeHandled(jsonFieldName) else input
        val encodedTail = tailEncoder.encode(inputWithFieldHandled.tail, discriminators)
        lazy val encodedHead = input.in.head.map(headEncoder.value.encode)

        // Before we call the common inclusion logic, there are a few things we need to take into account here.
        // The caller of this function has used the annotation on the field to determine whether it can be excluded
        // by the fieldset. Only attributes can be excluded that way. (Well, relationships can, too, but they're
        // not handled by this code.) Metadata always passes this check (can't be excluded by the fieldset), but it
        // may be excluded by the next one...
        //
        // Some fields must be encoded even if they are set to their default value, regardless of the configuration:
        // id and lid, specifically. Excluding them can lead to an invalid JSON:API resource. Hopefully, they'll
        // never have a default value, but it could happen and we don't prevent it.
        //
        // This two paragraphs are captured in the two conditions prior to calling the common logic.

        val include = mustBeEncoded || shouldIncludeField(
          input.fieldsSpec.forResourceType(input.resourceType),
          jsonFieldName,
          isAttribute,
          params.config.encodeDefaultValues,
          encodedDefault,
          encodedHead
        )

        if (include)
          encodedHead.map(encodeHead(_, encodedTail, jsonFieldName)).getOrElse(encodedTail)
        else
          encodedTail
      }
    }

  implicit def forTypeHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      tailEncoder: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[H :: T, Option[H] :: DT, (Type :: HNil) :: AT] =
    ignoreHCons[H, T, DT, Type, AT]

  implicit def forSourceHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      tailEncoderFactoryFactory: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[H :: T, Option[H] :: DT, (Source :: HNil) :: AT] =
    ignoreHCons[H, T, DT, Source, AT]

  private def ignoreHCons[H, T <: HList, DT <: HList, AH, AT <: HList](implicit
      tailEncoderFactory: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[H :: T, Option[H] :: DT, (AH :: HNil) :: AT] =
    (caseClassInfo, params) => {
      val tailEncoder = tailEncoderFactory(caseClassInfo.tail, params)

      (input, discriminators) => {
        tailEncoder.encode(input.tail, discriminators)
      }
    }

  // Scala doesn't seem to be able to find the generic Cardinality instance (below) for Id. Give it a little push.
  // TODO: find a way to delete this if possible
  implicit def forRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[JObjectEncoder[H]],
      tailEncoder: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, (Relationship :: HNil) :: AT] =
    forRelationshipsHCons[Single, H, T, DT, AT]

  /**
    *  @param headEncoder Note that this does not need to be a ResourceEncoder because we never include anything but
    *                    the reference itself.
    */
  implicit def forRelationshipsHCons[F[_]: Cardinality, H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[JObjectEncoder[H]],
      tailEncoderFactory: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[Option[F[H]] :: T, Option[
    Option[F[H]]
  ] :: DT, (Relationship :: HNil) :: AT] =
    (caseClassInfo, params) => {
      implicit val traverse: Traverse[F] = Cardinality.traverseForCardinality[F]
      val scalaFieldName = caseClassInfo.fieldNames.head
      // The only default that can be excluded from encoding here is an empty value. Throw away any non-empty default.
      lazy val encodedDefault = caseClassInfo.defaults.head
        .map(_.map(_.map(headEncoder.value.encode)))
        .filter(_.exists(_.isEmpty))

      val jsonFieldName = params.config.fieldNameMapping(scalaFieldName)
      val tailEncoder = tailEncoderFactory(caseClassInfo.tail, params)

      (input, discriminators) => {
        val encodedTail = tailEncoder.encode(input.withRelationshipHandled(jsonFieldName).tail, discriminators)
        lazy val encodedHead = input.in.head.map(_.map(headEncoder.value.encode))

        // Figure out if we're supposed to include the related objects in the document based on IncludeSpec.
        input.includeSpec.descend(jsonFieldName) match {
          case child: IncludeSpec.Always =>
            // The caller asked for this relationship to be included, but we don't actually have the ability
            // to do that (based on the case class annotation), so just return an error.
            encodedTail.addError(UnavailableIncludePath(child.path))
          case _ =>
            // We don't need to include the related resource object, but should we even include the field?

            val include =
              shouldIncludeField(
                input.fieldsSpec.forResourceType(input.resourceType),
                jsonFieldName,
                canBeExcluded = true,
                params.config.encodeDefaultValues,
                encodedDefault,
                encodedHead
              )

            if (include)
              encodedTail.addRelationship(
                jsonFieldName,
                encodedHead.map(relationshipData(_)).map(v => JObject("data" -> v))
              )
            else
              encodedTail
        }
      }
    }

  // Scala doesn't seem to be able to find the generic Cardinality instance (below) for Id. Give it a little push.
  // TODO: find a way to delete this if possible
  implicit def forIncludedRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[H]],
      tailEncoder: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[Option[H] :: T, Option[
    Option[H]
  ] :: DT, (IncludedRelationship :: HNil) :: AT] =
    forIncludedRelationshipsHCons[Single, H, T, DT, AT]

  implicit def forIncludedRelationshipsHCons[F[_]: Cardinality, H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[H]],
      tailEncoderFactory: HListResourceEncoderFactory[T, DT, AT]
  ): HListResourceEncoderFactory[
    Option[F[H]] :: T,
    Option[Option[F[H]]] :: DT,
    (IncludedRelationship :: HNil) :: AT
  ] =
    (caseClassInfo, params) => {
      implicit val traverse: Traverse[F] = Cardinality.traverseForCardinality[F]
      val scalaFieldName = caseClassInfo.fieldNames.head
      val jsonFieldName = params.config.fieldNameMapping(scalaFieldName)
      val tailEncoder = tailEncoderFactory(caseClassInfo.tail, params)
      // The only default that can be excluded from encoding here is an empty value. Throw away any non-empty default.
      lazy val encodedDefault = caseClassInfo.defaults.head
        .map(_.map(_.map(headEncoder.value.encodeMinimally(_))))
        .filter(_.exists(_.isEmpty))

      (input, discriminators) => {
        val encodedTail = tailEncoder.encode(input.withRelationshipHandled(jsonFieldName).tail, discriminators)

        // For each relationship, we need a resource identifier and possibly a resource object. We'll automatically
        // derive the former from the latter as the latter is the only thing we have implicits to create. Whether we
        // actually need the resource object depends on whether this field is in the IncludeSpec (has a path). If it
        // doesn't, we'll possibly save a little work by minimally encoding the resource object. After all, we only
        // need the type and id, which are always included. If it _is_ to be included, then we need to use the
        // specified FieldsSpec and IncludeSpec to determine how to encode the resource object because we're going to
        // include it in the resulting Encoded.

        val head = input.in.head

        input.includeSpec.descend(jsonFieldName) match {
          case IncludeSpec.Never if !input.fieldsSpec.forResourceType(input.resourceType)(jsonFieldName) =>
            // This field is excluded and the path is excluded, nothing else to do.
            encodedTail

          case IncludeSpec.Never =>
            // The path is excluded (the resource object), but the field is included (the resource identifier).
            // Minimally encode the objects to get identifiers from them.
            val encodedHead =
              head.map(_.map(headEncoder.value.encodeInfallibly(_, IncludeSpec.Never, FieldsSpec.None)).map(_.root))

            val include = shouldIncludeField(
              input.fieldsSpec.forResourceType(input.resourceType),
              jsonFieldName,
              canBeExcluded = true,
              params.config.encodeDefaultValues,
              encodedDefault,
              encodedHead
            )

            if (include)
              encodedTail.addRelationship(
                jsonFieldName,
                encodedHead.map(relationshipData(_)).map(v => JObject("data" -> v))
              )
            else
              encodedTail

          case childIncludeSpec =>
            // The path is included, so it doesn't really matter what the fieldSpec says; it gets included regardless.
            // We need to encode the included objects the way the parameters say that we should. Then we'll extract
            // the identifiers from those for the field value.

            encodedTail
              .whenValid(
                head.traverse(
                  _.parTraverse(headEncoder.value.encodeResource(_, childIncludeSpec, input.fieldsSpec, discriminators))
                )
              ) {
                // This field is optional and absent, so there's nothing to encode.
                case None =>
                  encodedTail

                case Some(rawEncodedHeads) =>
                  val include = shouldIncludeField(
                    input.fieldsSpec.forResourceType(input.resourceType),
                    jsonFieldName,
                    canBeExcluded = true,
                    params.config.encodeDefaultValues,
                    encodedDefault,
                    Some(rawEncodedHeads) // Rewrap in Some for comparison purposes.
                  )

                  if (include)
                    // For any of the objects that don't have IDs, generate and insert one
                    rawEncodedHeads
                      .parTraverse { encodedHead =>
                        import org.scalawag.bateman.jsonapi.lens._
                        import org.scalawag.bateman.json.focus.weak._

                        val f = encodedHead.root.asRootFocus
                        // Decode the type and (optional) id from each encoded head. Generate an identifier for each.
                        (f(resourceType).decode[String], f(id.?).decode[String]).parMapN {
                          case (rt, Some(id)) =>
                            // If there's an ID, just include this object as-is. Use its ID to reference it.
                            val ri = JObject("type" -> rt.toJAny, "id" -> id.toJAny)
                            ri -> encodedHead
                          case (rt, None) =>
                            // No ID, add a lid that we can use to refer to the included object.
                            val lid = params.lidGenerator()
                            val ri = JObject("type" -> rt.toJAny, "lid" -> lid.toJAny)
                            ri -> encodedHead.copy(root =
                              f.asObject.map(_.overwriteTo("lid", lid.toJAny)).flatMap(_.asObject).getOrThrow.value
                            )
                        }
                      }
                      .map(_.unzip)
                      .map {
                        case (encodedHeadIds, encodedHeadObjects) =>
                          encodedTail
                            .addRelationship(jsonFieldName, JObject("data" -> relationshipData(encodedHeadIds)))
                            .addInclusions(encodedHeadObjects.map(_.root).toList)
                            .addInclusions(encodedHeadObjects.map(_.inclusions).combineAll)
                      }
                      .getOrThrow // Any errors detected here are programmer errors.
                  else
                    encodedTail
              }
        }
      }
    }

  /** The logic here seem complex and tricky enough that I thought it warranted centralizing it and documenting it.
    *
    * Whether or not to encode a field is a bit complicated. It depends on several things:
    *   1) What type of field is it (what annotation does it have)?
    *   2) Is it included in the FieldsSpec for this encoding? Explicitly or implicitly?
    *   3) Is it set to the default value?
    */
  private def shouldIncludeField[A](
      fields: Fields,
      fieldName: String,
      canBeExcluded: Boolean,
      encodeDefaults: Boolean,
      encodedDefault: Option[A],
      encodedHead: A
  ) = {
    fields match {
      case Explicit(fields) if canBeExcluded =>
        // If it's explicitly included or excluded by a fieldset, then heed that.
        fields(fieldName)
      case fields =>
        // If it's not excluded (implicitly) by the fieldset, then encode it as long as we're either configured to
        // encode default values OR the value we want to encode is different than the default value.
        fields(fieldName) && (encodeDefaults || !encodedDefault.contains(encodedHead))
    }
  }

  private def relationshipData[F[_]: Cardinality, A](fa: F[A]): JAny =
    fa match {
      case Null                 => JNull
      case NotNull(ri: JObject) => ri
      case ri: JObject          => ri
      case ris: List[JObject]   => JArray(ris: _*)
    }

  /** Turns any of the Option-headed instances here into non-Option-headed instances. */
  implicit def forSomifiedHCons[H, T <: HList, DT <: HList, A <: HList](implicit
      delegate: HListResourceEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, A]
  ): HListResourceEncoderFactory[H :: T, Option[H] :: DT, A] =
    (info, params) =>
      (input, discriminators) => delegate.apply(info.doubleSomeHead, params).encode(input.somifyHead, discriminators)
}
