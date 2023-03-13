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

package org.scalawag.bateman.jsonapi.generic.decoding

import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{Id, Traverse}
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.json.decoding._
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, SourceTag, Tag}
import org.scalawag.bateman.json.{Nullable, validIfEmpty}
import org.scalawag.bateman.jsonapi.decoding._
import org.scalawag.bateman.jsonapi.generic.decoding.HListResourceDecoderFactoryFactory.{Input, Output, Params}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, MetaTag, RelationshipTag, decoding}
import org.scalawag.bateman.jsonapi.query.{attribute, multiple, nullable, relationship, required, _}
import org.w3c.dom.TypeInfo
import shapeless.tag.@@
import shapeless.{::, HList, HNil, Lazy, tag}

import scala.reflect.{ClassTag, classTag}

trait HListResourceDecoder[In <: ResourceLike, Out <: HList] {
  def decode(input: Input[In]): DecodeResult[Output[Out]]
}

trait HListResourceDecoderFactory[In <: ResourceLike, Out <: HList] {
  def apply(params: Params): HListResourceDecoder[In, Out]
}

trait HListResourceDecoderFactoryFactory[In <: ResourceLike, Out <: HList, Defaults <: HList] {
  def apply(typeInfo: CaseClassInfo[Defaults]): HListResourceDecoderFactory[In, Out]
}

object HListResourceDecoderFactoryFactory {
  final case class Params(config: Config, resourceTypeOverride: Option[String]) {
    def resourceTypeFor[A: ClassTag]: String =
      resourceTypeOverride.getOrElse(config.classNameMapping(classTag[A].runtimeClass.getSimpleName))
  }

  final case class Input[In <: ResourceLike](
      in: In,
      context: Document,
      fieldPointers: Map[String, JPointer] = Map.empty,
  ) {
    def withFieldPointer(name: String, tag: Tag, pointer: JPointer): Input[In] =
      this.copy(fieldPointers = this.fieldPointers + (name -> pointer))
  }

  final case class Output[Out <: HList](out: Out, fieldSources: Map[String, JPointer])

  implicit def hnilDecoder[In <: ResourceLike]: HListResourceDecoderFactoryFactory[In, HNil, HNil] =
    _ => {
      params => { input =>
        val output: Output[HNil] = Output[HNil](HNil, input.fieldPointers)

        {
          if (params.config.allowUnknownFields)
            output.validNec
          else {

            // Gather up all of the values present in the input so we can compare that with what's been handled.

            // TODO: Does this ID check belong here? There could be an ID in the JSON that doesn't have a place in
            //       the case class.

            val ids = input.in match {
              case r: ResourceIdentifierLike => r.optionalId.toList.map(JPointer.Root / "id" -> _)
              case _                         => Iterable.empty
            }

            val meta =
              input.in.meta.toList.flatMap(_.mappings).map {
                case (n, v) => JPointer.Root / "meta" / n.value -> v
              }

            val (attributes, relationships) = input.in match {
              case r: ResourceObjectLike =>
                (
                  r.attributes.toList.flatMap(_.mappings).map {
                    case (n, v) => JPointer.Root / "attributes" / n.value -> v
                  },
                  r.relationships.toList.flatMap(_.mappings).map {
                    case (n, v) => JPointer.Root / "relationships" / n.value -> v.src.root
                  }
                )
              case _ =>
                (
                  Iterable.empty,
                  Iterable.empty
                )
            }

            val presentValues = ids ++ meta ++ attributes ++ relationships
            val handledValues = input.fieldPointers.values.toSet
            val unhandledValues = presentValues.filterNot(x => handledValues(x._1)).map(_._2).map(UnexpectedValue)
            validIfEmpty(unhandledValues, output)
          }
        }
      }
    }

  implicit def hconsIdDecoder[In <: ResourceLike, OutHead, OutTail <: HList, DefaultTail <: HList](implicit
      headDecoder: Lazy[ContextualDecoder[JString, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (OutHead @@ IdTag) :: OutTail,
    Option[OutHead @@ IdTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, OutHead, IdTag, OutTail, DefaultTail](IdTag, JPointer.Root / _) { (input, _) =>
      implicit val hdec = headDecoder.value
      input.in.cquery(input.context)(_ ~> id ~> as[JString] ~> as[OutHead])
    }

  // We need special handling for optional IDs because JSON:API doesn't allow the ID to be set to JSON `null`. So, the
  // only way that we can set it to [[None]] is for it to be absent. This works out well with out restriction that the
  // ID decoder must go from [[JString]] and not [[JAny]], which would be required if `null` were a valid value.

  implicit def hconsOptionIdDecoder[In <: ResourceLike, OutHead, OutTail <: HList, DefaultTail <: HList](implicit
      headDecoder: Lazy[ContextualDecoder[JString, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[OutHead] @@ IdTag) :: OutTail,
    Option[Option[OutHead] @@ IdTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, Option[OutHead], IdTag, OutTail, DefaultTail](IdTag, JPointer.Root / _) {
      (input, _) =>
        implicit val hdec = headDecoder.value
        input.in.ctquery(input.context)(_ ~>? id ~> as[JString] ~> as[OutHead])
    }

  implicit def hconsAttributeDecoder[In <: ResourceLike, OutHead, OutTail <: HList, DefaultTail <: HList](implicit
      headDecoder: Lazy[ContextualDecoder[JAny, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (OutHead @@ AttributeTag) :: OutTail,
    Option[OutHead @@ AttributeTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, OutHead, AttributeTag, OutTail, DefaultTail](
      AttributeTag,
      JPointer.Root / "attributes" / _
    ) { (input, fieldName) =>
      implicit val hdec = headDecoder.value
      input.in match {
        case ro: ResourceObjectLike =>
          ro.cquery(input.context)(_ ~> attribute(fieldName) ~> as[OutHead])
        case ri: ResourceIdentifier =>
          UnspecifiedField(ri.src.root, JPointer.Root / "attributes" / fieldName).invalidNec
      }
    }

  implicit def hconsOptionAttributeDecoder[In <: ResourceLike, OutHead, OutTail <: HList, DefaultTail <: HList](implicit
      headDecoder: Lazy[ContextualDecoder[JAny, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[OutHead] @@ AttributeTag) :: OutTail,
    Option[Option[OutHead] @@ AttributeTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, Option[OutHead], AttributeTag, OutTail, DefaultTail](
      AttributeTag,
      JPointer.Root / "attributes" / _
    ) { (input, fieldName) =>
      implicit val hdec = headDecoder.value
      input.in match {
        case ro: ResourceObjectLike =>
          ro.ctquery(input.context)(_ ~>? attribute(fieldName) ~> as[OutHead])
        case ri: ResourceIdentifier =>
          UnspecifiedField(ri.src.root, JPointer.Root / "attributes" / fieldName).invalidNec
      }
    }

  implicit def hconsMetaDecoder[In <: ResourceLike, OutHead, OutTail <: HList, DefaultTail <: HList](implicit
      headDecoder: Lazy[ContextualDecoder[JAny, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (OutHead @@ MetaTag) :: OutTail,
    Option[OutHead @@ MetaTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, OutHead, MetaTag, OutTail, DefaultTail](MetaTag, JPointer.Root / "meta" / _) {
      (input, fieldName) =>
        implicit val hdec = headDecoder.value
        input.in.cquery(input.context)(_ ~> meta(fieldName) ~> as[OutHead])
    }

  implicit def hconsOptionMetaDecoder[In <: ResourceLike, OutHead, OutTail <: HList, DefaultTail <: HList](implicit
      headDecoder: Lazy[ContextualDecoder[JAny, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[OutHead] @@ MetaTag) :: OutTail,
    Option[Option[OutHead] @@ MetaTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, Option[OutHead], MetaTag, OutTail, DefaultTail](MetaTag, JPointer.Root / "meta" / _) {
      (input, fieldName) =>
        implicit val hdec = headDecoder.value
        input.in.ctquery(input.context)(_ ~>? meta(fieldName) ~> as[OutHead])
    }

  implicit def hconsRelationshipSingularIdentifierDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (OutHead @@ RelationshipTag) :: OutTail,
    Option[OutHead @@ RelationshipTag] :: DefaultTail
  ] =
    hconsRelationshipIdentifierDecoder[Id, In, OutHead, OutTail, DefaultTail](
      required.forRelationship(required).toTraverseQuery
    )

  implicit def hconsRelationshipNullableIdentifierDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Nullable[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Nullable[OutHead] @@ RelationshipTag] :: DefaultTail
  ] = hconsRelationshipIdentifierDecoder(nullable)

  implicit def hconsRelationshipMultipleIdentifierDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (List[OutHead] @@ RelationshipTag) :: OutTail,
    Option[List[OutHead] @@ RelationshipTag] :: DefaultTail
  ] = hconsRelationshipIdentifierDecoder(multiple)

  /** This is generic over the cardinality (Id, Option, List) because the remainder of the code is the same.
    * It's not implicit because `selector` has to be specified to make it work, so all permutations are
    * instantiated as their own implicits above.
    */
  def hconsRelationshipIdentifierDecoder[
      Card[_]: Traverse,
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](selector: TraverseQuery[Card, RelationshipData, ResourceIdentifier, Document])(implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Card[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Card[OutHead] @@ RelationshipTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, Card[OutHead], RelationshipTag, OutTail, DefaultTail](
      RelationshipTag,
      JPointer.Root / "relationships" / _
    ) { (input, fieldName) =>
      input.in match {
        case ro: ResourceObjectLike =>
          val ros =
            ro.ctquery[Card, ResourceIdentifier, Document](input.context)(
              _ ~> relationship(fieldName) ~> data ~> selector
            )
          val heads = ros.andThen(_.traverse(headDecoder.value.decode(_, input.context)))
          heads
        case ri: ResourceIdentifier =>
          UnspecifiedField(ri.src.root, JPointer.Root / "relationships" / fieldName).invalidNec
      }
    }

  implicit def hconsOptionRelationshipSingularIdentifierDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Option[OutHead] @@ RelationshipTag] :: DefaultTail
  ] =
    hconsOptionRelationshipIdentifierDecoder[Id, In, OutHead, OutTail, DefaultTail](
      required.forRelationship(required).toTraverseQuery
    )

  implicit def hconsOptionRelationshipNullableIdentifierDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[Nullable[OutHead]] @@ RelationshipTag) :: OutTail,
    Option[Option[Nullable[OutHead]] @@ RelationshipTag] :: DefaultTail
  ] = hconsOptionRelationshipIdentifierDecoder(nullable)

  implicit def hconsOptionRelationshipMultipleIdentifierDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[List[OutHead]] @@ RelationshipTag) :: OutTail,
    Option[Option[List[OutHead]] @@ RelationshipTag] :: DefaultTail
  ] = hconsOptionRelationshipIdentifierDecoder(multiple)

  /** This is generic over the cardinality (Id, Option, List) because the remainder of the code is the same.
    * It's not implicit because `selector` has to be specified to make it work, so all permutations are
    * instantiated as their own implicits above.
    */

  def hconsOptionRelationshipIdentifierDecoder[
      Card[_]: Traverse,
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](selector: TraverseQuery[Card, RelationshipData, ResourceIdentifier, Document])(implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[Card[OutHead]] @@ RelationshipTag) :: OutTail,
    Option[Option[Card[OutHead]] @@ RelationshipTag] :: DefaultTail
  ] =
    headOptionDecoderFactoryFactory[In, Card[OutHead], RelationshipTag, OutTail, DefaultTail](
      RelationshipTag,
      JPointer.Root / "relationships" / _
    ) { (input, fieldName) =>
      input.in match {
        case ro: ResourceObjectLike =>
          val ros =
            ro.ctquery[Card, ResourceIdentifier, Document](input.context)(
              _ ~> relationship(fieldName) ~> data ~> selector
            )
          val heads = ros.andThen(_.traverse(headDecoder.value.decode(_, input.context)))
          heads
        case ri: ResourceIdentifier =>
          UnspecifiedField(ri.src.root, JPointer.Root / "relationships" / fieldName).invalidNec
      }
    }

  implicit def hconsRelationshipSingularObjectDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (OutHead @@ RelationshipTag) :: OutTail,
    Option[OutHead @@ RelationshipTag] :: DefaultTail
  ] =
    hconsRelationshipObjectDecoder[Id, In, OutHead, OutTail, DefaultTail](
      required.forRelationship(required).toTraverseQuery
    )

  implicit def hconsRelationshipNullableObjectDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Nullable[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Nullable[OutHead] @@ RelationshipTag] :: DefaultTail
  ] = hconsRelationshipObjectDecoder(nullable)

  implicit def hconsRelationshipMultipleObjectDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (List[OutHead] @@ RelationshipTag) :: OutTail,
    Option[List[OutHead] @@ RelationshipTag] :: DefaultTail
  ] = hconsRelationshipObjectDecoder(multiple)

  /** This is generic over the cardinality (Id, Option, List) because the remainder of the code is the same.
    * It's not implicit because `selector` has to be specified to make it work, so all permutations are
    * instantiated as their own implicits above.
    */
  def hconsRelationshipObjectDecoder[
      Card[_]: Traverse,
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](selector: TraverseQuery[Card, RelationshipData, ResourceIdentifier, Document])(implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Card[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Card[OutHead] @@ RelationshipTag] :: DefaultTail
  ] =
    headDecoderFactoryFactory[In, Card[OutHead], RelationshipTag, OutTail, DefaultTail](
      RelationshipTag,
      JPointer.Root / "relationships" / _
    ) { (input, fieldName) =>
      input.in match {
        case ro: ResourceObjectLike =>
          val resourceIdentifiersResult =
            ro.ctquery[Card, ResourceIdentifier, Document](input.context)(
              _ ~> relationship(fieldName) ~> data ~> selector
            )

          // We got all the ids. Now, try to go back to the included to get the ResourceObject and decode each
          resourceIdentifiersResult.andThen { crid =>
            crid.traverse { rid =>
              implicit val hdec = headDecoder.value
              input.context.requiredIncluded(rid).andThen(_.cquery(input.context)(_ ~> as[OutHead]))
            }
          }

        case ri: ResourceIdentifier =>
          UnspecifiedField(ri.src.root, JPointer.Root / "relationships" / fieldName).invalidNec
      }
    }

  implicit def hconsOptionRelationshipSingularObjectDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Option[OutHead] @@ RelationshipTag] :: DefaultTail
  ] =
    hconsOptionRelationshipObjectDecoder[Id, In, OutHead, OutTail, DefaultTail](
      required.forRelationship(required).toTraverseQuery
    )

  implicit def hconsOptionRelationshipNullableObjectDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[Nullable[OutHead]] @@ RelationshipTag) :: OutTail,
    Option[Option[Nullable[OutHead]] @@ RelationshipTag] :: DefaultTail
  ] = hconsOptionRelationshipObjectDecoder(nullable)

  implicit def hconsOptionRelationshipMultipleObjectDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[List[OutHead]] @@ RelationshipTag) :: OutTail,
    Option[Option[List[OutHead]] @@ RelationshipTag] :: DefaultTail
  ] = hconsOptionRelationshipObjectDecoder(multiple)

  /** This is generic over the cardinality (Id, Option, List) because the remainder of the code is the same.
    * It's not implicit because `selector` has to be specified to make it work, so all permutations are
    * instantiated as their own implicits above.
    */
  def hconsOptionRelationshipObjectDecoder[
      Card[_]: Traverse,
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](selector: TraverseQuery[Card, RelationshipData, ResourceIdentifier, Document])(implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[Card[OutHead]] @@ RelationshipTag) :: OutTail,
    Option[Option[Card[OutHead]] @@ RelationshipTag] :: DefaultTail
  ] =
    headOptionDecoderFactoryFactory[In, Card[OutHead], RelationshipTag, OutTail, DefaultTail](
      RelationshipTag,
      JPointer.Root / "relationships" / _
    ) { (input, fieldName) =>
      input.in match {
        case ro: ResourceObjectLike =>
          val resourceIdentifiersResult =
            ro.ctquery[Card, ResourceIdentifier, Document](input.context)(
              _ ~> relationship(fieldName) ~> data ~> selector
            )

          // We got all the ids. Now, try to go back to the included to get the ResourceObject and decode each
          resourceIdentifiersResult.andThen { crid =>
            crid.traverse { rid =>
              implicit val hdec = headDecoder.value
              input.context.requiredIncluded(rid).andThen(_.cquery(input.context)(_ ~> as[OutHead]))
            }
          }

        case ri: ResourceIdentifier =>
          UnspecifiedField(ri.src.root, JPointer.Root / "relationships" / fieldName).invalidNec
      }
    }

  // I had to copy these from json-generic because otherwise the tailDecoder type doesn't match and nothing works.
  // Parameterizing the tail type seemed like it would add more complexity than it's worth. Luckily, there's no
  // real logic here, it's mostly just typing.

  implicit def hconsSourceDecoder[In <: ResourceLike, OutTail <: HList, DefaultTail <: HList](implicit
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (JSource @@ SourceTag) :: OutTail,
    Option[JSource @@ SourceTag] :: DefaultTail
  ] =
    headSourceDecoderFactoryFactory[In, JSource, OutTail, DefaultTail](identity)

  implicit def hconsSourceOptionDecoder[In <: ResourceLike, OutTail <: HList, DefaultTail <: HList](implicit
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[JSource] @@ SourceTag) :: OutTail,
    Option[Option[JSource] @@ SourceTag] :: DefaultTail
  ] =
    headSourceDecoderFactoryFactory[In, Option[JSource], OutTail, DefaultTail](Some(_))

  def headSourceDecoderFactoryFactory[In <: ResourceLike, OutHead, OutTail <: HList, DefaultTail <: HList](
      headDecoder: JSource => OutHead
  )(implicit
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (OutHead @@ SourceTag) :: OutTail,
    (
      Option[
        OutHead
          @@ SourceTag
      ]
    ) :: DefaultTail
  ] =
    typeInfo => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoderFactory = tailDecoderFactoryFactory(typeInfo.tail)

      params => {
        val tailDecoder = tailDecoderFactory(params)

        input => {
          val tailResult =
            tailDecoder.decode(input.withFieldPointer(scalaFieldName, SourceTag, JPointer.Root))

          tailResult.map { tailOutput =>
            tailOutput.copy(
              headDecoder(decoding.JSource(input.in, input.in.src.root, tailOutput.fieldSources)) :: tailOutput.out
            )
          }
        }
      }
    }

  def headDecoderFactoryFactory[In <: ResourceLike, OutHead, Tg <: Tag, OutTail <: HList, DefaultTail <: HList](
      tag: Tag,
      pointerFn: String => JPointer
  )(
      decodeHead: (Input[In], String) => DecodeResult[OutHead]
  )(implicit
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[In, (OutHead @@ Tg) :: OutTail, Option[OutHead @@ Tg] :: DefaultTail] =
    typeInfo => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoderFactory = tailDecoderFactoryFactory(typeInfo.tail)

      params => {
        val tailDecoder = tailDecoderFactory(params)
        val jsonFieldName = params.config.fieldNameMapping(typeInfo.fieldNames.head)
        val pointer = pointerFn(jsonFieldName)

        input => {
          val tailResult = tailDecoder.decode(input.withFieldPointer(scalaFieldName, tag, pointer))
          val headResult =
            (params.config.useDefaultsForMissingFields, typeInfo.defaults.head) match {
              case (true, Some(d)) =>
                pointer.navigateOption(input.in.src.root).andThen {
                  case Some(_) => decodeHead(input, jsonFieldName)
                  case None    => d.validNec
                }
              case _ =>
                decodeHead(input, jsonFieldName)
            }

          (headResult, tailResult).mapN { (h, t) => t.copy(out = shapeless.tag[Tg](h) :: t.out) }
        }
      }
    }

  def headOptionDecoderFactoryFactory[In <: ResourceLike, OutHead, Tg <: Tag, OutTail <: HList, DefaultTail <: HList](
      tag: Tag,
      pointerFn: String => JPointer
  )(
      decodeHead: (Input[In], String) => DecodeResult[OutHead]
  )(implicit
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[In, (Option[OutHead] @@ Tg) :: OutTail, Option[
    Option[OutHead] @@ Tg
  ] :: DefaultTail] =
    typeInfo => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoderFactory = tailDecoderFactoryFactory(typeInfo.tail)

      params => {
        val tailDecoder = tailDecoderFactory(params)
        val jsonFieldName = params.config.fieldNameMapping(typeInfo.fieldNames.head)
        val pointer = pointerFn(jsonFieldName)

        input => {
          val tailResult = tailDecoder.decode(input.withFieldPointer(scalaFieldName, tag, pointer))
          val headResult =
            (params.config.useDefaultsForMissingFields, typeInfo.defaults.head) match {
              case (true, Some(d)) =>
                pointer.navigateOption(input.in.src.root).andThen {
                  case Some(_) => decodeHead(input, jsonFieldName).map(Some(_))
                  case None    => d.validNec
                }
              case x =>
                pointer.navigateOption(input.in.src.root).andThen {
                  case Some(_) => decodeHead(input, jsonFieldName).map(Some(_))
                  case None    => None.validNec
                }
            }

          (headResult, tailResult).mapN { (h, t) => t.copy(out = shapeless.tag[Tg](h) :: t.out) }
        }
      }
    }

  // TODO: This is an attempt at replacing all of the optional handlers with a single method that handles all optional
  //       fields. It doesn't work, and I haven't yet figured out why.
//  def hconsOptionDecoder[In <: ResourceLike, Tg <: Tag, OutHead, OutTail <: HList, DefaultTail <: HList](implicit
//      headDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, (OutHead @@ Tg) :: OutTail, Option[
//        OutHead @@ Tg
//      ] :: DefaultTail]
//  ): HListResourceDecoderFactoryFactory[
//    In,
//    (Option[OutHead] @@ Tg) :: OutTail,
//    Option[Option[OutHead] @@ Tg] :: DefaultTail
//  ] =
//    typeInfo => {
//      val headDecoderFactory = headDecoderFactoryFactory(typeInfo.flattenHeadOrNone)
//
//      params => {
//        val headDecoder = headDecoderFactory(params)
//
//        input => {
//          println(s"G6: ${typeInfo.fieldNames.head} $input")
//          headDecoder.decode(input).map { output =>
//            output.copy(out = tag[Tg](Some(output.out.head: OutHead)) :: output.out.tail)
//          }
//        }
//      }
//    }
}
