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

package org.scalawag.bateman.jsonapi.decoding

import cats.syntax.validated._
import cats.syntax.traverse._
import cats.syntax.apply._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.decoding.{
  DecodeResult,
  Decoder,
  JAny,
  JAnyDecoder,
  JObject,
  JPointer,
  JString,
  JsonTypeMismatch,
  UnexpectedValue,
  UnspecifiedField
}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.{SourceTag, semiauto}
import org.scalawag.bateman.json.{ProgrammerError, validIfEmpty}
import org.scalawag.bateman.jsonapi.encoding
import shapeless.tag.@@

sealed trait ResourceLike extends HasMeta {
  val src: JSource
  val `type`: JString
  val meta: Option[Meta]
  val optionalId: Option[JString]

  def validateType(expectedType: String): DecodeResult[Unit] =
    if (`type`.value != expectedType)
      JsonApiTypeMismatch(this, expectedType).invalidNec
    else
      ().validNec

  def optionalIdAs[A](implicit dec: Decoder[JString, A]): DecodeResult[Option[A]] =
    optionalId.traverse(dec.decode(_))
  def requiredId: DecodeResult[JString] =
    optionalId.map(_.validNec).getOrElse(UnspecifiedField(src.root, "id").invalidNec)
  def requiredIdAs[A](implicit dec: Decoder[JString, A]): DecodeResult[A] =
    requiredId.andThen(dec.decode(_))

  def optionalMeta(name: String): Option[JAny] =
    meta.flatMap(_.get(name))
  def optionalMetaAs[A](name: String)(implicit dec: Decoder[JAny, A]): DecodeResult[Option[A]] =
    optionalMeta(name).traverse(dec.decode(_))
  def requiredMeta(name: String): DecodeResult[JAny] =
    optionalMeta(name)
      .map(_.validNec)
      .getOrElse(UnspecifiedField(this.src.root, JPointer.Root / "meta" / name).invalidNec)
  def requiredMetaAs[A](name: String)(implicit dec: Decoder[JAny, A]): DecodeResult[A] =
    requiredMeta(name).andThen(dec.decode(_))

  def optionalAttribute(name: String): Option[JAny]

  def optionalAttributeAs[A](name: String)(implicit dec: Decoder[JAny, A]): DecodeResult[Option[A]] =
    optionalAttribute(name).traverse(dec.decode(_))
  def requiredAttribute(name: String): DecodeResult[JAny] =
    optionalAttribute(name)
      .map(_.validNec)
      .getOrElse(UnspecifiedField(this.src.root, JPointer.Root / "attributes" / name).invalidNec)
  def requiredAttributeAs[A](name: String)(implicit dec: Decoder[JAny, A]): DecodeResult[A] =
    requiredAttribute(name).andThen(dec.decode(_))

  def optionalRelationship(name: String): Option[Relationship]
  def optionalRelationshipAs[A](name: String)(implicit dec: Decoder[Relationship, A]): DecodeResult[Option[A]] =
    optionalRelationship(name).traverse(dec.decode(_))
  def requiredRelationship(name: String): DecodeResult[Relationship] =
    optionalRelationship(name)
      .map(_.validNec)
      .getOrElse(UnspecifiedField(this.src.root, JPointer.Root / "relationships" / name).invalidNec)
  def requiredRelationshipAs[A](name: String)(implicit dec: Decoder[Relationship, A]): DecodeResult[A] =
    requiredRelationship(name).andThen(dec.decode(_))
}

object ResourceLike {
  implicit val decoder: Decoder[JObject, ResourceLike] = Decoder { in =>
    val fieldNames = in.fieldList.map(_.name.value).toSet
    if (fieldNames.contains("id"))
      in.as[ResourceIdentifierLike]
    else
      in.as[ResourceObjectOptionalId]
  }
}

sealed trait ResourceIdentifierLike extends ResourceLike {
  val id: JString
  override val optionalId: Option[JString] = Some(id)
}

object ResourceIdentifierLike {
  implicit val decoder: Decoder[JObject, ResourceIdentifierLike] = Decoder { in =>
    val fieldNames = in.fieldList.map(_.name.value)
    if (fieldNames.exists(ResourceIdentifier.forbiddenFields))
      in.as[ResourceObject]
    else
      in.as[ResourceIdentifier]
  }

  // This is used to decode relatives in derived decoders when we don't know if the caller is providing a decoder
  // from ResourceIdentifier or from ResourceObject (after looking it up in included). When we actually do the
  // decoding, we have to know which is is to give it the appropriate input, but this allows us to know that at
  // least one of those decoders exists for compile-time checking.

  implicit def fromIdentifierDecoder[A](implicit
      dec: ResourceDecoder[ResourceIdentifier, A]
  ): ResourceDecoder[ResourceIdentifierLike, A] = { (_, _) =>
    throw ProgrammerError(
      "This decoder (fromIdentifierDecoder) shouldn't actually be used to decode. It's only for static type checking to ensure there's either a ResourceIdentifierDecoder or a ResourceObjectDecoder"
    )
  }

  implicit def fromObjectDecoder[A](implicit
      dec: ResourceDecoder[ResourceObject, A]
  ): ResourceDecoder[ResourceIdentifierLike, A] = { (_, _) =>
    throw ProgrammerError(
      "This decoder (fromObjectDecoder) shouldn't actually be used to decode. It's only for static type checking to ensure there's either a ResourceIdentifierDecoder or a ResourceObjectDecoder"
    )
  }
}

final case class ResourceIdentifier(src: JSource @@ SourceTag, `type`: JString, id: JString, meta: Option[Meta] = None)
    extends ResourceIdentifierLike {
  def included(document: Document): DecodeResult[ResourceObject] = document.requiredIncluded(this)
  def toEncoding: encoding.ResourceIdentifier =
    encoding.ResourceIdentifier(
      `type` = `type`.value,
      id = id.value,
      meta = meta.map(_.toEncoding)
    )

  override def optionalAttribute(name: String): Option[JAny] = None
  override def optionalRelationship(name: String): Option[Relationship] = None
}

object ResourceIdentifier {
  val forbiddenFields = Set("attributes", "relationships", "links")

  implicit val decoder: Decoder[JObject, ResourceIdentifier] =
    semiauto.deriveDecoderForCaseClass[ResourceIdentifier, Any]()

  implicit val resourceLikeTranscoder: Decoder[ResourceLike, ResourceIdentifier] = Decoder.fromPF {
    case ri: ResourceIdentifier => ri.validNec
    case rl                     => rl.src.root.as[ResourceIdentifier]
  }
}

trait ResourceObjectLike extends ResourceLike with HasLinks {
  val optionalId: Option[JString]
  val attributes: Option[Attributes]
  val relationships: Option[Relationships]
  val links: Option[Links]

  def optionalAttribute(name: String): Option[JAny] = attributes.flatMap(_.get(name))
  def optionalRelationship(name: String): Option[Relationship] = relationships.flatMap(_.get(name))
}

final case class ResourceObjectOptionalId(
    src: JSource @@ SourceTag,
    `type`: JString,
    id: Option[JString] = None,
    attributes: Option[Attributes] = None,
    relationships: Option[Relationships] = None,
    meta: Option[Meta] = None,
    links: Option[Links] = None
) extends ResourceObjectLike {
  override val optionalId: Option[JString] = id

  def toEncoding: encoding.ResourceObjectOptionalId =
    encoding.ResourceObjectOptionalId(
      `type` = `type`.value,
      id = id.map(_.value),
      attributes = attributes.map(_.toEncoding),
      relationships = relationships.map(_.toEncoding),
      meta = meta.map(_.toEncoding),
      links = links.map(_.toEncoding)
    )
}

object ResourceObjectOptionalId {
  implicit val decoder: Decoder[JObject, ResourceObjectOptionalId] =
    semiauto.deriveDecoderForCaseClass[ResourceObjectOptionalId, Any]()

  implicit val resourceLikeTranscoder: Decoder[ResourceLike, ResourceObjectOptionalId] = Decoder.fromPF {
    case ro: ResourceObjectOptionalId => ro.validNec
    case rl                           => rl.src.root.as[ResourceObjectOptionalId]
  }
}

final case class ResourceObject(
    src: JSource @@ SourceTag,
    `type`: JString,
    id: JString,
    attributes: Option[Attributes] = None,
    relationships: Option[Relationships] = None,
    meta: Option[Meta] = None,
    links: Option[Links] = None
) extends ResourceObjectLike
    with ResourceIdentifierLike {
  def toEncoding: encoding.ResourceObject =
    encoding.ResourceObject(
      `type` = `type`.value,
      id = id.value,
      attributes = attributes.map(_.toEncoding),
      relationships = relationships.map(_.toEncoding),
      meta = meta.map(_.toEncoding),
      links = links.map(_.toEncoding)
    )
}

object ResourceObject {
  implicit val decoder: Decoder[JObject, ResourceObject] =
    semiauto.deriveDecoderForCaseClass[ResourceObject, Any]()

  implicit val resourceLikeTranscoder: Decoder[ResourceLike, ResourceObject] = Decoder.fromPF {
    case ro: ResourceObject => ro.validNec
    case rl                 => rl.src.root.as[ResourceObject]
  }
}
