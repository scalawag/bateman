// bateman -- Copyright 2021 -- Justin Patterson
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

import cats.{Id, Traverse}
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.functor._
import org.scalawag.bateman.json.{NotNull, Null, Nullable}
import org.scalawag.bateman.json.encoding.{Encoder, JAny, JString}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, SourceTag, Tag}
import org.scalawag.bateman.jsonapi.encoding
import org.scalawag.bateman.jsonapi.encoding.{
  DeferredEncoding,
  FieldsSpec,
  IncludeSpec,
  InvalidFieldName,
  InvalidIncludePath,
  NullData,
  Relationship,
  RelationshipData,
  ResourceEncoder,
  ResourceIdentifier
}
import org.scalawag.bateman.jsonapi.generic.encoding.HListResourceEncoderFactoryFactory.{Input, Params}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, MetaTag, RelationshipTag}
import shapeless.tag.@@
import shapeless.{::, HList, HNil, Lazy, tag}

import scala.reflect.{ClassTag, classTag}

trait HListResourceEncoder[In <: HList, Defaults <: HList] {
  // Instead of returning a result here, just keep the errors in the PartialResource. This makes it easier to gather
  // all of the errors while processing as much as possible. At the end, we still fail if there are any errors, but
  // we lose some of the fail-fast behavior that happens when there are a bunch of different encoders interacting.
  def encode(input: Input[In]): PartialResource
}

trait HListResourceEncoderFactory[In <: HList, Defaults <: HList] {
  def apply(params: Params): HListResourceEncoder[In, Defaults]
}

trait HListResourceEncoderFactoryFactory[In <: HList, Default <: HList] {
  def apply(typeInfo: CaseClassInfo[Default]): HListResourceEncoderFactory[In, Default]
}

object HListResourceEncoderFactoryFactory {
  final case class Params(config: Config, resourceTypeOverride: Option[String]) {
    def resourceTypeFor[A: ClassTag]: String =
      resourceTypeOverride.getOrElse(config.classNameMapping(classTag[A].runtimeClass.getSimpleName))
  }

  final case class Input[In <: HList](
      in: In,
      resourceType: String,
      includeSpec: IncludeSpec,
      fieldsSpec: FieldsSpec,
      fieldsHandled: Set[(String, Tag)] = Set.empty
  ) {
    def relationshipsHandled: Set[String] = fieldsHandled.collect { case (n, RelationshipTag) => n }
    def withFieldHandled(name: String, tag: Tag): Input[In] = this.copy(fieldsHandled = fieldsHandled + ((name, tag)))
  }

  object Input {
    implicit class InputOps[H, T <: HList](input: Input[H :: T]) {
      def tail: Input[T] = input.copy(in = input.in.tail)
    }
  }

  implicit val hnilEncoder: HListResourceEncoderFactoryFactory[HNil, HNil] = { _ => _ => input =>
    import input._
    // If we got any paths that don't have corresponding relationships, that's an error.
    val invalidIncludePathErrors =
      includeSpec.explicitChildren.filterNot(relationshipsHandled).map(includeSpec.descend) collect {
        case always: IncludeSpec.Always => InvalidIncludePath(always.path)
      }

    // If we have any fields specified that aren't actually fields on this resource type, that's an error.
    val invalidFieldErrors =
      input.fieldsSpec.explicitFields(input.resourceType).collect {
        case k if !input.fieldsHandled.map(_._1).contains(k) => InvalidFieldName(input.resourceType, k)
      }

    val errors = invalidFieldErrors ++ invalidIncludePathErrors

    PartialResource(errors = errors.toSet)
  }

  implicit def hconsIdEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[Encoder[InHead, JString]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (InHead @@ IdTag) :: InTail,
    Option[InHead @@ IdTag] :: DefTail
  ] =
    headEncoderFactoryFactory[InHead, IdTag, InTail, DefTail](IdTag, false, false) { (input, partial, _) =>
      partial.copy(id = Some(headEncoder.value.encode(input.in.head).value))
    }

  implicit def hconsIdOptionalEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[Encoder[InHead, JString]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[InHead] @@ IdTag) :: InTail,
    Option[Option[InHead] @@ IdTag] :: DefTail
  ] =
    headEncoderFactoryFactory[Option[InHead], IdTag, InTail, DefTail](IdTag, false, false) { (input, partial, _) =>
      partial.copy(id = input.in.head.map(headEncoder.value.encode).map(_.value))
    }

  implicit def hconsAttributeEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[Encoder[InHead, JAny]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (InHead @@ AttributeTag) :: InTail,
    Option[InHead @@ AttributeTag] :: DefTail
  ] =
    headEncoderFactoryFactory[InHead, AttributeTag, InTail, DefTail](AttributeTag, true, true) {
      (input, partial, name) =>
        partial.addAttribute(name, headEncoder.value.encode(input.in.head))
    }

  implicit def hconsOptionAttributeEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[Encoder[InHead, JAny]],
      tailEncoderFactoryFactory: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[InHead] @@ AttributeTag) :: InTail,
    Option[Option[InHead] @@ AttributeTag] :: DefTail
  ] =
    headEncoderFactoryFactory[Option[InHead], AttributeTag, InTail, DefTail](AttributeTag, true, true) {
      (input, partial, name) =>
        (input.in.head: Option[InHead]) match {
          case Some(h) => partial.addAttribute(name, headEncoder.value.encode(h))
          case None    => partial
        }
    }

  implicit def hconsMetaEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[Encoder[InHead, JAny]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (InHead @@ MetaTag) :: InTail,
    Option[InHead @@ MetaTag] :: DefTail
  ] =
    headEncoderFactoryFactory[InHead, MetaTag, InTail, DefTail](MetaTag, true, false) { (input, partial, name) =>
      partial.addMeta(name, headEncoder.value.encode(input.in.head))
    }

  implicit def hconsOptionMetaEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[Encoder[InHead, JAny]],
      tailEncoderFactoryFactory: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[InHead] @@ MetaTag) :: InTail,
    Option[Option[InHead] @@ MetaTag] :: DefTail
  ] =
    headEncoderFactoryFactory[Option[InHead], MetaTag, InTail, DefTail](MetaTag, true, true) { (input, partial, name) =>
      (input.in.head: Option[InHead]) match {
        case Some(h) => partial.addMeta(name, headEncoder.value.encode(h))
        case None    => partial
      }
    }

  implicit def hconsRelationshipRequiredIdentifierEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (InHead @@ RelationshipTag) :: InTail,
    Option[InHead @@ RelationshipTag] :: DefTail
  ] = hconsRelationshipIdentifierEncoder[Id, InHead, InTail, DefTail]

  implicit def hconsRelationshipOptionalIdentifierEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Nullable[InHead] @@ RelationshipTag) :: InTail,
    Option[Nullable[InHead] @@ RelationshipTag] :: DefTail
  ] = hconsRelationshipIdentifierEncoder[Nullable, InHead, InTail, DefTail]

  implicit def hconsRelationshipMultipleIdentifierEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (List[InHead] @@ RelationshipTag) :: InTail,
    Option[List[InHead] @@ RelationshipTag] :: DefTail
  ] = hconsRelationshipIdentifierEncoder[List, InHead, InTail, DefTail]

  def hconsRelationshipIdentifierEncoder[Card[_]: Traverse, InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Card[InHead] @@ RelationshipTag) :: InTail,
    Option[Card[InHead] @@ RelationshipTag] :: DefTail
  ] = { typeInfo => params =>
    headEncoderFactoryFactory[Card[InHead], RelationshipTag, InTail, DefTail](RelationshipTag, true, true) {
      (input, partial, name) =>
        // Figure out if we should include these relationships in the document. If so, we don't have the instances,
        // so just queue up deferred encoding jobs.

        val head: Card[InHead] = input.in.head
        val encodedHeads = head.map(headEncoder.value.encode)

        val deferrals = input.includeSpec.descend(name) match {
          case child: IncludeSpec.Always => encodedHeads.map(h => DeferredEncoding(h, child, input.fieldsSpec)).toList
          case _                         => Nil
        }

        if (input.fieldsSpec.includeField(input.resourceType, name))
          partial
            .addRelationship(name, Relationship(Some(relationshipData(encodedHeads))))
            .addDeferredEncodings(deferrals)
        else
          partial.addDeferredEncodings(deferrals)
    }.apply(typeInfo)(params)
  }

  implicit def hconsOptionRelationshipRequiredIdentifierEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[InHead] @@ RelationshipTag) :: InTail,
    Option[Option[InHead] @@ RelationshipTag] :: DefTail
  ] = hconsOptionRelationshipIdentifierEncoder[Id, InHead, InTail, DefTail]

  implicit def hconsOptionRelationshipOptionalIdentifierEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[Nullable[InHead]] @@ RelationshipTag) :: InTail,
    Option[Option[Nullable[InHead]] @@ RelationshipTag] :: DefTail
  ] = hconsOptionRelationshipIdentifierEncoder[Nullable, InHead, InTail, DefTail]

  implicit def hconsOptionRelationshipMultipleIdentifierEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[List[InHead]] @@ RelationshipTag) :: InTail,
    Option[Option[List[InHead]] @@ RelationshipTag] :: DefTail
  ] = hconsOptionRelationshipIdentifierEncoder[List, InHead, InTail, DefTail]

  def hconsOptionRelationshipIdentifierEncoder[Card[_]: Traverse, InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceIdentifier]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[Card[InHead]] @@ RelationshipTag) :: InTail,
    Option[Option[Card[InHead]] @@ RelationshipTag] :: DefTail
  ] = { typeInfo => params =>
    headEncoderFactoryFactory[Option[Card[InHead]], RelationshipTag, InTail, DefTail](RelationshipTag, true, true) {
      (input, partial, name) =>
        (input.in.head: Option[Card[InHead]]) match {
          case None       => partial
          case Some(head) =>
            // Figure out if we should include these relationships in the document. If so, we don't have the instances,
            // so just queue up deferred encoding jobs.

            val encodedHeads = head.map(headEncoder.value.encode)

            val deferrals = input.includeSpec.descend(name) match {
              case child: IncludeSpec.Always =>
                encodedHeads.map(h => DeferredEncoding(h, child, input.fieldsSpec)).toList
              case _ => Nil
            }

            if (input.fieldsSpec.includeField(input.resourceType, name))
              partial
                .addRelationship(name, Relationship(Some(relationshipData(encodedHeads))))
                .addDeferredEncodings(deferrals)
            else
              partial.addDeferredEncodings(deferrals)

        }

    }.apply(typeInfo)(params)
  }

  implicit def hconsRelationshipRequiredObjectEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (InHead @@ RelationshipTag) :: InTail,
    Option[InHead @@ RelationshipTag] :: DefTail
  ] = hconsRelationshipObjectEncoder[Id, InHead, InTail, DefTail]

  implicit def hconsRelationshipOptionalObjectEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Nullable[InHead] @@ RelationshipTag) :: InTail,
    Option[Nullable[InHead] @@ RelationshipTag] :: DefTail
  ] = hconsRelationshipObjectEncoder[Nullable, InHead, InTail, DefTail]

  implicit def hconsRelationshipMultipleObjectEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (List[InHead] @@ RelationshipTag) :: InTail,
    Option[List[InHead] @@ RelationshipTag] :: DefTail
  ] = hconsRelationshipObjectEncoder[List, InHead, InTail, DefTail]

  def hconsRelationshipObjectEncoder[Card[_]: Traverse, InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Card[InHead] @@ RelationshipTag) :: InTail,
    Option[Card[InHead] @@ RelationshipTag] :: DefTail
  ] = { typeInfo => params =>
    headEncoderFactoryFactory[Card[InHead], RelationshipTag, InTail, DefTail](RelationshipTag, true, true) {
      (input, partial, fieldName) =>
        // For each relationship, we need a ResourceIdentifier and possibly a ResourceObject. Whether we need a
        // ResourceObject depends on whether this field is in the IncludeSpec (has a path). If it doesn't,
        // we can just encode the ResourceObject (which is the only encoder we have access to) with no fields and
        // then extract the identifier from that. If it _is_ included, then we need to use the specified FieldsSpec
        // and IncludeSpec to determine how to encode it because we're going to use the resulting ResourceObject.

        val head: Card[InHead] = input.in.head

        input.includeSpec.descend(fieldName) match {
          case IncludeSpec.Never =>
            // If it's not included, all we possibly need is the RI, which is only needed if it's an included field.
            if (input.fieldsSpec.includeField(input.resourceType, fieldName))
              // Encode minimally, just to get an identifier.
              partial.whenValid(
                head.traverse(headEncoder.value.encodeResource(_, IncludeSpec.Never, FieldsSpec.None))
              ) { encodedHead =>
                val relationship = {
                  val ri = encodedHead.map(_.root.getResourceIdentifier)
                  Some(Relationship(Option(relationshipData(ri))))
                }

                partial.addRelationship(fieldName, relationship)
              }
            else
              // This field is excluded and the path is excluded, nothing else to do.
              partial

          case childIncludeSpec =>
            // It is included. We need to encode it the way the parameters say that we should.
            partial.whenValid(head.traverse(headEncoder.value.encodeResource(_, childIncludeSpec, input.fieldsSpec))) {
              encodedHead =>
                val relationship =
                  if (input.fieldsSpec.includeField(input.resourceType, fieldName)) {
                    val ri = encodedHead.map(_.root.getResourceIdentifier)
                    Some(Relationship(Option(relationshipData(ri))))
                  } else None

                partial
                  .addRelationship(fieldName, relationship)
                  .addInclusions(encodedHead.map(_.root).toList)
                  .addInclusions(encodedHead.map(_.inclusions).combineAll)
                  .addDeferredEncodings(encodedHead.toList.flatMap(_.deferrals))
            }
        }
    }.apply(typeInfo)(params)
  }

  implicit def hconsOptionRelationshipRequiredObjectEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[InHead] @@ RelationshipTag) :: InTail,
    Option[Option[InHead] @@ RelationshipTag] :: DefTail
  ] = hconsOptionRelationshipObjectEncoder[Id, InHead, InTail, DefTail]

  implicit def hconsOptionRelationshipOptionalObjectEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[Nullable[InHead]] @@ RelationshipTag) :: InTail,
    Option[Option[Nullable[InHead]] @@ RelationshipTag] :: DefTail
  ] = hconsOptionRelationshipObjectEncoder[Nullable, InHead, InTail, DefTail]

  implicit def hconsOptionRelationshipMultipleObjectEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[List[InHead]] @@ RelationshipTag) :: InTail,
    Option[Option[List[InHead]] @@ RelationshipTag] :: DefTail
  ] = hconsOptionRelationshipObjectEncoder[List, InHead, InTail, DefTail]

  def hconsOptionRelationshipObjectEncoder[Card[_]: Traverse, InHead, InTail <: HList, DefTail <: HList](implicit
      headEncoder: Lazy[ResourceEncoder[InHead, encoding.ResourceObject]],
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (Option[Card[InHead]] @@ RelationshipTag) :: InTail,
    Option[Option[Card[InHead]] @@ RelationshipTag] :: DefTail
  ] = { typeInfo => params =>
    headEncoderFactoryFactory[Option[Card[InHead]], RelationshipTag, InTail, DefTail](RelationshipTag, true, true) {
      (input, partial, fieldName) =>
        (input.in.head: Option[Card[InHead]]) match {
          case None       => partial
          case Some(head) =>
            // For each relationship, we need a ResourceIdentifier and possibly a ResourceObject. Whether we need a
            // ResourceObject depends on whether this field is in the IncludeSpec (has a path). If it doesn't,
            // we can just encode the ResourceObject (which is the only encoder we have access to) with no fields and
            // then extract the identifier from that. If it _is_ included, then we need to use the specified FieldsSpec
            // and IncludeSpec to determine how to encode it because we're going to use the resulting ResourceObject.

            input.includeSpec.descend(fieldName) match {
              case IncludeSpec.Never =>
                // If it's not included, all we possibly need is the RI, which is only needed if it's an included field.
                if (input.fieldsSpec.includeField(input.resourceType, fieldName))
                  // Encode minimally, just to get an identifier.
                  partial.whenValid(
                    head.traverse(headEncoder.value.encodeResource(_, IncludeSpec.Never, FieldsSpec.None))
                  ) { encodedHead =>
                    val relationship = {
                      val ri = encodedHead.map(_.root.getResourceIdentifier)
                      Some(Relationship(Option(relationshipData(ri))))
                    }

                    partial.addRelationship(fieldName, relationship)
                  }
                else
                  // This field is excluded and the path is excluded, nothing else to do.
                  partial

              case childIncludeSpec =>
                // It is included. We need to encode it the way the parameters say that we should.
                partial.whenValid(
                  head.traverse(headEncoder.value.encodeResource(_, childIncludeSpec, input.fieldsSpec))
                ) { encodedHead =>
                  val relationship =
                    if (input.fieldsSpec.includeField(input.resourceType, fieldName)) {
                      val ri = encodedHead.map(_.root.getResourceIdentifier)
                      Some(Relationship(Option(relationshipData(ri))))
                    } else None

                  partial
                    .addRelationship(fieldName, relationship)
                    .addInclusions(encodedHead.map(_.root).toList)
                    .addInclusions(encodedHead.map(_.inclusions).combineAll)
                    .addDeferredEncodings(encodedHead.toList.flatMap(_.deferrals))
                }
            }
        }
    }.apply(typeInfo)(params)
  }

  private def relationshipData[F[_]: Traverse, A](fa: F[A]): RelationshipData =
    fa match {
      case _: Null                         => NullData
      case NotNull(ri: ResourceIdentifier) => RelationshipData.fromResourceIdentifier(ri)
      case ri: ResourceIdentifier          => RelationshipData.fromResourceIdentifier(ri)
      case ris: List[ResourceIdentifier]   => RelationshipData.fromResourceIdentifiers(ris)
    }

  implicit def hconsSourceEncoder[InHead, InTail <: HList, DefTail <: HList](implicit
      tailEncoder: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[
    (InHead @@ SourceTag) :: InTail,
    Option[InHead @@ SourceTag] :: DefTail
  ] =
    headEncoderFactoryFactory[InHead, SourceTag, InTail, DefTail](SourceTag, false, false) { (_, partial, _) =>
      // This is essentially a no-op. We can't encode source fields.
      partial
    }

  def headEncoderFactoryFactory[InHead, Tg <: Tag, InTail <: HList, DefTail <: HList](
      tag: Tg,
      canBeExcluded: Boolean,
      fieldsCanExclude: Boolean,
  )(
      encodeHead: (Input[(InHead @@ Tg) :: InTail], PartialResource, String) => PartialResource
  )(implicit
      tailEncoderFactoryFactory: HListResourceEncoderFactoryFactory[InTail, DefTail]
  ): HListResourceEncoderFactoryFactory[(InHead @@ Tg) :: InTail, Option[InHead @@ Tg] :: DefTail] =
    caseClassInfo => {
      val scalaFieldName = caseClassInfo.fieldNames.head
      val tailEncoderFactory = tailEncoderFactoryFactory(caseClassInfo.tail)

      params => {
        val jsonFieldName = params.config.fieldNameMapping(scalaFieldName)
        val tailEncoder = tailEncoderFactory(params)

        input => {
          val encodedTail = tailEncoder.encode(input.withFieldHandled(jsonFieldName, tag).tail)

          // Whether to encode this field is a bit complicated. It depends on several things.
          //   1) What type of field is it (what Tag does it have)?
          //   2) Is it set to the default value?
          //   3) Is it included in the FieldsSpec for this encoding?
          //
          // #1 needs to be determined by the caller of this function because we don't know the type of the field here.
          // This drives everything else. Since all of the context is available here, we actually make all the
          // decisions here, but allow the caller to specify through a few parameters whether we should heed #2 and #3.
          //
          // Some fields must be encoded even if they have a default value that they equal. IdTag is the only one that
          // comes to mind here, because excluding it can lead to an invalid ResourceIdentifierLike, which requires an
          // id.
          //
          // Only JSON:API fields (AttributeTag and RelationshipTag) can be excluded by the FieldsSpec. So, MetaTag
          // and IdTag are always encoded. SourceTag is never encoded because it doesn't make sense to.
          //
          // Here we go...

          val exclude =
            canBeExcluded && (
              (fieldsCanExclude && !input.fieldsSpec.includeField(input.resourceType, jsonFieldName)) ||
                (!params.config.encodeDefaultValues && caseClassInfo.defaults.head.contains(input.in.head))
            )

          if (exclude)
            encodedTail
          else
            encodeHead(input, encodedTail, jsonFieldName)
        }
      }
    }
}
