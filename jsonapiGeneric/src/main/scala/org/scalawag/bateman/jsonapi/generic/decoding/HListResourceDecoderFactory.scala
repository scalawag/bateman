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

package org.scalawag.bateman.jsonapi.generic.decoding

import cats.syntax.parallel._
import cats.syntax.either._
import cats.Functor
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import org.scalawag.bateman.json.{
  Decoder,
  JAny,
  JAnyDecoder,
  JObject,
  JObjectDecoder,
  JResult,
  JString,
  JStringDecoder,
  Nullable,
  UnexpectedValue,
  rightIfEmpty
}
import Decoder.{nullableDecoder, widenJObjectDecoder}
import org.scalawag.bateman.json.focus.{JFocus, Single}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.jsonapi.generic.decoding.HListResourceDecoderFactory.Params
import org.scalawag.bateman.jsonapi.generic.Annotations._
import shapeless.{::, HList, HNil, Lazy}

import scala.reflect.{ClassTag, classTag}

trait HListResourceDecoderFactory[Out <: HList, Defaults <: HList, Annots <: HList] {
  def apply(typeInfo: CaseClassInfo[Defaults], params: Params): HListResourceDecoder[Out]
}

object HListResourceDecoderFactory {
  final case class Params(config: Config, resourceTypeOverride: Option[String]) {
    def resourceTypeFor[A: ClassTag]: String =
      resourceTypeOverride.getOrElse(config.classNameMapping(classTag[A].runtimeClass.getSimpleName))
  }

  final case class Output[Out <: HList](out: Out, fieldSources: Map[String, JFocus[JAny]])

  implicit def forHNil: HListResourceDecoderFactory[HNil, HNil, HNil] =
    (_, params) => {
      input =>
        val output: Output[HNil] = Output[HNil](HNil, input.fieldSources)

        if (params.config.allowUnknownFields)
          output.rightNec
        else
          // Gather up all of the values present in the input so we can compare that with what's been handled.
          List(
            input.in(resourceType.?).map(_.toList),
            input.in(id.?).map(_.toList),
            input.in(meta.? ~> *).map(_.foci),
            input.in(attributes.? ~> *).map(_.foci),
            input.in(relationships.? ~> *).map(_.foci),
          ).parFlatSequence.flatMap { presentValues =>
            val handled = input.fieldSources.values.toSet
            val unhandled = presentValues.filterNot(handled).map(UnexpectedValue)
            rightIfEmpty(unhandled, output)
          }

    }

  implicit def forTypeHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JStringDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[H :: T, Option[H] :: DT, (Type :: HNil) :: AT] =
    headDecoderFactory[Single, JString, H, T, DT, Type, AT](
      _ => resourceType,
      _.decode(headDecoder.value)
    )

  implicit def forOptionTypeHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JStringDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[Option[H] :: T, Option[Option[H]] :: DT, (Type :: HNil) :: AT] =
    optionHeadDecoderFactory[Single, JString, H, T, DT, Type, AT](
      _ => resourceType,
      _.decode(headDecoder.value)
    )

  implicit def forIdHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JStringDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[H :: T, Option[H] :: DT, (Id :: HNil) :: AT] =
    headDecoderFactory[Single, JString, H, T, DT, Id, AT](
      _ => id,
      _.decode(headDecoder.value)
    )

  implicit def forOptionIdHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JStringDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[(Option[H]) :: T, Option[Option[H]] :: DT, (Id :: HNil) :: AT] =
    optionHeadDecoderFactory[Single, JString, H, T, DT, Id, AT](
      _ => id,
      _.decode(headDecoder.value)
    )

  implicit def forAttributeHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JAnyDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    H :: T,
    Option[H] :: DT,
    (Attribute :: HNil) :: AT
  ] =
    headDecoderFactory[Single, JAny, H, T, DT, Attribute, AT](
      attribute,
      _.decode(headDecoder.value)
    )

  implicit def forOptionAttributeHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JAnyDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Option[H] :: T,
    Option[Option[H]] :: DT,
    (Attribute :: HNil) :: AT
  ] =
    optionHeadDecoderFactory[Single, JAny, H, T, DT, Attribute, AT](
      attribute,
      _.decode(headDecoder.value)
    )

  implicit def forMetaHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JAnyDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    H :: T,
    Option[H] :: DT,
    (Meta :: HNil) :: AT
  ] =
    headDecoderFactory[Single, JAny, H, T, DT, Meta, AT](
      meta(_),
      _.decode(headDecoder.value)
    )

  implicit def forOptionMetaHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JAnyDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Option[H] :: T,
    Option[Option[H]] :: DT,
    (Meta :: HNil) :: AT
  ] =
    optionHeadDecoderFactory[Single, JAny, H, T, DT, Meta, AT](
      meta(_),
      _.decode(headDecoder.value)
    )

  implicit def forRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    H :: T,
    Option[H] :: DT,
    (Relationship :: HNil) :: AT
  ] =
    headDecoderFactory[Single, JObject, H, T, DT, Relationship, AT](
      relationship,
      _(data ~> narrow[JObject]).flatMap(_.decode(headDecoder.value))
    )

  implicit def forOptionRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Option[H] :: T,
    Option[Option[H]] :: DT,
    (Relationship :: HNil) :: AT
  ] =
    optionHeadDecoderFactory[Single, JObject, H, T, DT, Relationship, AT](
      relationship,
      _(data ~> narrow[JObject]).flatMap(_.decode(headDecoder.value))
    )

  implicit def forNullableRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Nullable[H] :: T,
    Option[Nullable[H]] :: DT,
    (Relationship :: HNil) :: AT
  ] =
    headDecoderFactory[Single, JObject, Nullable[H], T, DT, Relationship, AT](
      relationship,
      // TODO: make these more composable somehow.
      { in =>
        val dec: JAnyDecoder[Nullable[H]] = Decoder.nullableDecoder(Decoder.widenJObjectDecoder(headDecoder.value))
        in(data).flatMap(_.decode(dec))
      }
    )

  implicit def forOptionNullableRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Option[Nullable[H]] :: T,
    Option[Option[Nullable[H]]] :: DT,
    (Relationship :: HNil) :: AT
  ] =
    optionHeadDecoderFactory[Single, JObject, Nullable[H], T, DT, Relationship, AT](
      relationship,
      // TODO: make these more composable somehow.
      { in =>
        val dec: JAnyDecoder[Nullable[H]] = Decoder.nullableDecoder(Decoder.widenJObjectDecoder(headDecoder.value))
        in(data).flatMap(_.decode(dec))
      }
    )

  implicit def forListRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    List[H] :: T,
    Option[List[H]] :: DT,
    (Relationship :: HNil) :: AT
  ] =
    headDecoderFactory[List, JObject, H, T, DT, Relationship, AT](
      relationship,
      _(data ~> * ~> narrow[JObject]).flatMap(_.decode(headDecoder.value))
    )

  implicit def forOptionListRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Option[List[H]] :: T,
    Option[Option[List[H]]] :: DT,
    (Relationship :: HNil) :: AT
  ] =
    optionHeadDecoderFactory[List, JObject, H, T, DT, Relationship, AT](
      relationship,
      _(data ~> * ~> narrow[JObject]).flatMap(_.decode(headDecoder.value))
    )

  implicit def forIncludedRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    H :: T,
    Option[H] :: DT,
    (IncludedRelationship :: HNil) :: AT
  ] =
    headDecoderFactory[Single, JObject, H, T, DT, IncludedRelationship, AT](
      relationship,
      _(data ~> includedRef).flatMap(_.decode(headDecoder.value))
    )

  implicit def forOptionIncludedRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Option[H] :: T,
    Option[Option[H]] :: DT,
    (IncludedRelationship :: HNil) :: AT
  ] =
    optionHeadDecoderFactory[Single, JObject, H, T, DT, IncludedRelationship, AT](
      relationship,
      _(data ~> includedRef).flatMap(_.decode(headDecoder.value))
    )

  implicit def forNullableIncludedRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Nullable[H] :: T,
    Option[Nullable[H]] :: DT,
    (IncludedRelationship :: HNil) :: AT
  ] =
    headDecoderFactory[Single, JObject, Nullable[H], T, DT, IncludedRelationship, AT](
      relationship,
      _(data ~> nullableIncludedRef).flatMap(_.decode(nullableDecoder(widenJObjectDecoder(headDecoder.value))))
    )

  implicit def forOptionNullableIncludedRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    Option[Nullable[H]] :: T,
    Option[Option[Nullable[H]]] :: DT,
    (IncludedRelationship :: HNil) :: AT
  ] =
    optionHeadDecoderFactory[Single, JObject, Nullable[H], T, DT, IncludedRelationship, AT](
      relationship,
      _(data ~> nullableIncludedRef).flatMap(_.decode(nullableDecoder(widenJObjectDecoder(headDecoder.value))))
    )

  implicit def forListIncludedRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[
    List[H] :: T,
    Option[List[H]] :: DT,
    (IncludedRelationship :: HNil) :: AT
  ] =
    headDecoderFactory[List, JObject, H, T, DT, IncludedRelationship, AT](
      relationship,
      _(data ~> * ~> includedRef).flatMap(_.decode(headDecoder.value))
    )

  implicit def forOptionListIncludedRelationshipHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[Option[List[H]] :: T, Option[
    Option[List[H]]
  ] :: DT, (IncludedRelationship :: HNil) :: AT] =
    optionHeadDecoderFactory[List, JObject, H, T, DT, IncludedRelationship, AT](
      relationship,
      _(data ~> * ~> includedRef).flatMap(_.decode(headDecoder.value))
    )

  // TODO: try to factor out the commonalities between these two functions.

  def headDecoderFactory[Card[_]: Functor, J <: JAny, H, T <: HList, DT <: HList, AH, AT <: HList](
      extractorFn: String => CreatableJLens[JObject, J],
      decodeFn: JFocus[J] => JResult[Card[H]]
  )(implicit
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[Card[H] :: T, Option[Card[H]] :: DT, (AH :: HNil) :: AT] =
    (typeInfo, params) => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoder = tailDecoderFactory(typeInfo.tail, params)
      val jsonFieldName = params.config.fieldNameMapping(typeInfo.fieldNames.head)
      val extractor = extractorFn(jsonFieldName)

      input => {
        input.in(extractor.?).map(_.foci) flatMap { headFocus =>
          val tailResult = tailDecoder.decode(input.withFieldFocus(scalaFieldName, headFocus))
          val headResult: JResult[Card[H]] =
            (params.config.useDefaultsForMissingFields, typeInfo.defaults.head) match {
              case (true, Some(d)) =>
                headFocus match {
                  case None      => d.rightNec
                  case Some(out) => decodeFn(out)
                }
              case _ =>
                // Attempt to extract again because that's the easiest way to get consistent error handling.
                input.in(extractor).map(_.foci).flatMap(decodeFn)
            }

          (headResult, tailResult).parMapN { (h, t) => t.copy(out = h :: t.out) }
        }
      }
    }

  def optionHeadDecoderFactory[Card[_]: Functor, J <: JAny, H, T <: HList, DT <: HList, AH, AT <: HList](
      extractorFn: String => CreatableJLens[JObject, J],
      decodeFn: JFocus[J] => JResult[Card[H]]
  )(implicit
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[Option[Card[H]] :: T, Option[Option[Card[H]]] :: DT, (AH :: HNil) :: AT] =
    (typeInfo, params) => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoder = tailDecoderFactory(typeInfo.tail, params)
      val jsonFieldName = params.config.fieldNameMapping(typeInfo.fieldNames.head)
      val extractor = extractorFn(jsonFieldName)

      input => {
        input.in(extractor.?).flatMap { headFocus =>
          val tailResult = tailDecoder.decode(input.withFieldFocus(scalaFieldName, headFocus))
          val headResult: JResult[Option[Card[H]]] =
            headFocus.foci match {
              case None      => None.rightNec
              case Some(out) => decodeFn(out).map(Some.apply)
            }

          (headResult, tailResult).parMapN { (h, t) => t.copy(out = h :: t.out) }
        }
      }
    }

  // TODO: Merge these two into one implementation to minimize code duplication.

  implicit def forSourceHCons[T <: HList, DT <: HList, AT <: HList](implicit
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[JSource :: T, Option[JSource] :: DT, (Source :: HNil) :: AT] =
    (typeInfo, params) =>
      input =>
        tailDecoderFactory(typeInfo.tail, params).decode(input).map { tail =>
          val jsource = JSource(input.in, input.fieldSources)
          tail.copy(out = jsource :: tail.out)
        }

  implicit def forOptionSourceHCons[T <: HList, DT <: HList, AT <: HList](implicit
      tailDecoderFactory: HListResourceDecoderFactory[T, DT, AT]
  ): HListResourceDecoderFactory[Option[JSource] :: T, Option[Option[JSource]] :: DT, (Source :: HNil) :: AT] =
    (typeInfo, params) =>
      input =>
        tailDecoderFactory(typeInfo.tail, params).decode(input).map { tail =>
          val jsource = JSource(input.in, input.fieldSources)
          tail.copy(out = Some(jsource) :: tail.out)
        }
}
