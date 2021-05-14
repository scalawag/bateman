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
import org.scalawag.bateman.json.validIfEmpty
import org.scalawag.bateman.jsonapi.decoding._
import org.scalawag.bateman.jsonapi.generic.decoding.HListResourceDecoderFactoryFactory.{Input, Output, Params}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, MetaTag, RelationshipTag, decoding}
import org.scalawag.bateman.jsonapi.query.{attribute, multiple, optional, relationship, required, _}
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
      fieldPointers: Map[String, (Tag, JPointer)] = Map.empty,
  ) {
    def withFieldPointer(name: String, tag: Tag, pointer: JPointer): Input[In] =
      this.copy(fieldPointers = this.fieldPointers + (name -> (tag, pointer)))
    def idsHandled: Set[String] = fieldPointers.collect { case (name, (IdTag, _)) => name }.toSet
    def metasHandled: Set[String] = fieldPointers.collect { case (name, (MetaTag, _)) => name }.toSet
    def attributesHandled: Set[String] = fieldPointers.collect { case (name, (AttributeTag, _)) => name }.toSet
    def relationshipsHandled: Set[String] = fieldPointers.collect { case (name, (RelationshipTag, _)) => name }.toSet
  }

  final case class Output[Out <: HList](out: Out, fieldSources: Map[String, JPointer])

  implicit def hnilDecoder[In <: ResourceLike]: HListResourceDecoderFactoryFactory[In, HNil, HNil] =
    _ => {
      params => { input =>
        val output: Output[HNil] = Output[HNil](HNil, input.fieldPointers.mapValues(_._2))

        {
          if (params.config.allowUnknownFields)
            output.validNec
          else {

            val ids = input.in match {
              case r: ResourceIdentifierLike => r.optionalId.toIterable
              case _                         => Iterable.empty
            }

            val (attributes, relationships) = input.in match {
              case r: ResourceObjectLike =>
                (
                  r.attributes.toIterable.flatMap(_.mappings),
                  r.relationships.toIterable.flatMap(_.mappings)
                )
              case _ =>
                (
                  Iterable.empty,
                  Iterable.empty
                )
            }

            // TODO: Does this ID check belong here? There could be an ID in the JSON that doesn't have a place in
            //       the case class.

            val unknownIds = ids
              .filterNot(x => input.idsHandled(x.value))
              .map(UnexpectedValue)

            val unknownMetas = input.in.meta.toIterable
              .flatMap(_.mappings)
              .filterNot(x => input.metasHandled(x._1.value))
              .map(_._2)
              .map(UnexpectedValue)

            val unknownAttributes = attributes
              .filterNot(x => input.attributesHandled(x._1.value))
              .map(_._2)
              .map(UnexpectedValue)

            val unknownRelationships = relationships
              .filterNot(x => input.relationshipsHandled(x._1.value))
              .map(_._2.src.root)
              .map(UnexpectedValue)

            validIfEmpty(
              unknownIds ++ unknownMetas ++ unknownAttributes ++ unknownRelationships,
              output
            )
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
  // ID decoder must go from [[JString]] and noto [[JAny]], which would be required if `null` were a valid value.

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

  implicit def hconsRelationshipOptionalIdentifierDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceIdentifier, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Option[OutHead] @@ RelationshipTag] :: DefaultTail
  ] = hconsRelationshipIdentifierDecoder(optional)

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

  implicit def hconsRelationshipOptionalObjectDecoder[
      In <: ResourceLike,
      OutHead,
      OutTail <: HList,
      DefaultTail <: HList,
  ](implicit
      headDecoder: Lazy[ContextualDecoder[ResourceObject, OutHead, Document]],
      tailDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, OutTail, DefaultTail]
  ): HListResourceDecoderFactoryFactory[
    In,
    (Option[OutHead] @@ RelationshipTag) :: OutTail,
    Option[Option[OutHead] @@ RelationshipTag] :: DefaultTail
  ] = hconsRelationshipObjectDecoder(optional)

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
          val tailResult = tailDecoder.decode(input.withFieldPointer(scalaFieldName, SourceTag, JPointer.Root))

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
                  case Some(headJson) => decodeHead(input, jsonFieldName)
                  case None           => d.validNec
                }
              case _ =>
                decodeHead(input, jsonFieldName)
            }

          (headResult, tailResult).mapN { (h, t) => t.copy(out = shapeless.tag[Tg](h) :: t.out) }
        }
      }
    }

}
