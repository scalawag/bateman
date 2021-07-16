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

package org.scalawag.bateman.jsonapi.encoding

import org.scalawag.bateman.jsonapi.encoding
import org.scalawag.bateman.json.encoding.{Encoder, JAny, JArray, JNull, JObject, JObjectEncoder, JString}
import org.scalawag.bateman.json.generic.semiauto

sealed trait HasMeta[A] {
  def meta: Option[Map[String, JAny]]

  def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): A

  def addMeta(meta: Map[String, JAny]): A = mapMeta(m => Some(m.getOrElse(Map.empty) ++ meta))
}

sealed trait Link

object Link {
  implicit val encoder: Encoder[Link, JAny] = {
    case x: BareLink => Encoder[BareLink, JAny].encode(x)
    case x: RichLink => Encoder[RichLink, JAny].encode(x)
  }
}

final case class BareLink(
    href: String
) extends Link

object BareLink {
  implicit val encoder: Encoder[BareLink, JAny] = Encoder[String, JString].contramap(_.href)
}

final case class RichLink(
    href: Option[String] = None,
    meta: Option[Map[String, JAny]] = None
) extends Link
    with HasMeta[RichLink] {
  override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): RichLink = copy(meta = fn(meta))
}

object RichLink {
  implicit val encoder: Encoder[RichLink, JObject] = semiauto.deriveEncoderForCaseClass[RichLink]()
}

final case class ErrorSource(
    pointer: Option[String] = None,
    parameter: Option[String] = None
)

object ErrorSource {
  implicit val encoder: JObjectEncoder[ErrorSource] = semiauto.deriveEncoderForCaseClass[ErrorSource]()
}

final case class Error(
    id: Option[String] = None,
    links: Option[Map[String, Link]] = None,
    status: Option[String] = None,
    code: Option[String] = None,
    title: Option[String] = None,
    detail: Option[String] = None,
    source: Option[ErrorSource] = None,
    meta: Option[Map[String, JAny]] = None
) extends HasMeta[Error] {
  override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): Error = copy(meta = fn(meta))
}

object Error {
  implicit val encoder: Encoder[Error, JObject] = semiauto.deriveEncoderForCaseClass[Error]()
}

final case class Jsonapi(
    version: Option[String] = None,
    meta: Option[Map[String, JAny]] = None
)

object Jsonapi {
  implicit val encoder: Encoder[Jsonapi, JObject] = semiauto.deriveEncoderForCaseClass[Jsonapi]()
}

sealed trait Data

sealed trait PrimaryData extends Data

object PrimaryData {
  // These make it possible to create a PrimaryData without having to use the specific Data.* constructor
  implicit def fromResourceIdentifier(data: ResourceIdentifier): PrimaryData =
    ResourceIdentifierData(data)
  implicit def fromResourceIdentifiers(data: List[ResourceIdentifier]): PrimaryData =
    ResourceIdentifiersData(data)
  implicit def fromResourceObject(data: ResourceObject): PrimaryData =
    ResourceObjectData(data)
  implicit def fromResourceObjects(data: List[ResourceObject]): PrimaryData =
    ResourceObjectsData(data)
  implicit def fromResourceObjectOptionalId(data: ResourceObjectOptionalId): PrimaryData =
    ResourceObjectOptionalIdData(data)

  implicit def encoder: Encoder[PrimaryData, JAny] = {
    case d: ResourceIdentifierData       => ResourceIdentifierData.encoder.encode(d)
    case d: ResourceObjectData           => ResourceObjectData.encoder.encode(d)
    case d: ResourceObjectOptionalIdData => ResourceObjectOptionalIdData.encoder.encode(d)
    case d: ResourceIdentifiersData      => ResourceIdentifiersData.encoder.encode(d)
    case d: ResourceObjectsData          => ResourceObjectsData.encoder.encode(d)
  }
}

sealed trait RelationshipData extends Data

object RelationshipData {
  // These make it possible to create a RelationshipData without having to use the specific *Data constructor
  implicit def fromResourceIdentifier(data: ResourceIdentifier): RelationshipData =
    ResourceIdentifierData(data)
  implicit def fromResourceIdentifiers(data: List[ResourceIdentifier]): RelationshipData =
    ResourceIdentifiersData(data)

  implicit def encoder: Encoder[RelationshipData, JAny] = {
    case d: NullData                => NullData.encoder.encode(d)
    case d: ResourceIdentifierData  => ResourceIdentifierData.encoder.encode(d)
    case d: ResourceIdentifiersData => ResourceIdentifiersData.encoder.encode(d)
  }
}

sealed trait NullData extends PrimaryData with RelationshipData
case object NullData extends NullData {
  implicit def encoder: Encoder[NullData, JAny] = Encoder { _ => JNull }
}

case class ResourceIdentifierData(data: encoding.ResourceIdentifier) extends PrimaryData with RelationshipData

object ResourceIdentifierData {
  implicit def encoder: Encoder[ResourceIdentifierData, JAny] =
    Encoder[encoding.ResourceIdentifier, JAny].contramap(_.data)
}

case class ResourceObjectData(data: encoding.ResourceObject) extends PrimaryData

object ResourceObjectData {
  implicit def encoder: Encoder[ResourceObjectData, JAny] =
    Encoder[encoding.ResourceObject, JAny].contramap(_.data)
}

case class ResourceObjectOptionalIdData(data: encoding.ResourceObjectOptionalId) extends PrimaryData

object ResourceObjectOptionalIdData {
  implicit def encoder: Encoder[ResourceObjectOptionalIdData, JAny] =
    Encoder[encoding.ResourceObjectOptionalId, JAny].contramap(_.data)
}

case class ResourceIdentifiersData(data: List[encoding.ResourceIdentifier]) extends PrimaryData with RelationshipData

object ResourceIdentifiersData {
  implicit def encoder: Encoder[ResourceIdentifiersData, JArray] =
    Encoder[List[encoding.ResourceIdentifier], JArray].contramap(_.data)
}

case class ResourceObjectsData(data: List[encoding.ResourceObject]) extends PrimaryData

object ResourceObjectsData {
  implicit def encoder: Encoder[ResourceObjectsData, JArray] =
    Encoder[List[encoding.ResourceObject], JArray].contramap(_.data)
}

final case class Relationship(
    data: Option[RelationshipData] = None,
    meta: Option[Map[String, JAny]] = None,
    links: Option[Map[String, Link]] = None
) extends HasMeta[Relationship] {
  override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): Relationship = copy(meta = fn(meta))
}

object Relationship {
  implicit val encoder: Encoder[Relationship, JObject] = semiauto.deriveEncoderForCaseClass[Relationship]()
}

sealed trait ResourceLike {
  val `type`: String
  val meta: Option[Map[String, JAny]]
}

sealed trait ResourceIdentifierLike extends ResourceLike {
  val id: String
}

final case class ResourceIdentifier(
    `type`: String,
    id: String,
    meta: Option[Map[String, JAny]] = None
) extends ResourceIdentifierLike
    with HasMeta[ResourceIdentifier] {
  override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): ResourceIdentifier =
    copy(meta = fn(meta))
}

object ResourceIdentifier {
  implicit val encoder: Encoder[ResourceIdentifier, JObject] = semiauto.deriveEncoderForCaseClass[ResourceIdentifier]()
}

trait ResourceObjectLike extends ResourceLike {
  val optionalId: Option[String]
  val attributes: Option[Map[String, JAny]]
  val relationships: Option[Map[String, Relationship]]
  val links: Option[Map[String, Link]]
}

final case class ResourceObjectOptionalId(
    `type`: String,
    optionalId: Option[String] = None,
    attributes: Option[Map[String, JAny]] = None,
    relationships: Option[Map[String, Relationship]] = None,
    meta: Option[Map[String, JAny]] = None,
    links: Option[Map[String, Link]] = None
) extends ResourceObjectLike
    with HasMeta[ResourceObjectOptionalId] {
  override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): ResourceObjectOptionalId =
    copy(meta = fn(meta))
}

object ResourceObjectOptionalId {
  implicit val encoder: Encoder[ResourceObjectOptionalId, JObject] =
    semiauto.deriveEncoderForCaseClass[ResourceObjectOptionalId]()
}

final case class ResourceObject(
    `type`: String,
    id: String,
    attributes: Option[Map[String, JAny]] = None,
    relationships: Option[Map[String, Relationship]] = None,
    meta: Option[Map[String, JAny]] = None,
    links: Option[Map[String, Link]] = None
) extends ResourceObjectLike
    with ResourceIdentifierLike
    with HasMeta[ResourceObject] {
  override val optionalId: Option[String] = Some(id)

  override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): ResourceObject =
    copy(meta = fn(meta))

  def getResourceIdentifier: ResourceIdentifier = ResourceIdentifier(this.`type`, this.id)
}

object ResourceObject {
  implicit val encoder: Encoder[ResourceObject, JObject] = semiauto.deriveEncoderForCaseClass[ResourceObject]()

  implicit val ordering: Ordering[ResourceObject] = { (l, r) =>
    Iterable(l.`type` compare r.`type`, l.id compare r.id).find(_ != 0).getOrElse(0)
  }
}

// None for all these None means that the key didn't appear in the document. If they're set to "null" or empty, they
// will have a Some.

final case class Document(
    disposition: Document.Disposition,
    jsonapi: Option[Jsonapi] = None,
    links: Option[Map[String, Link]] = None
) extends HasMeta[Document] {

  val data: Option[PrimaryData] =
    disposition match {
      case Document.DataDisposition(data, _, _) => Some(data)
      case _                                    => None
    }

  val included: Option[List[ResourceObject]] =
    disposition match {
      case Document.DataDisposition(_, included, _) => included
      case _                                        => None
    }

  val errors: Option[List[Error]] = disposition match {
    case Document.ErrorsDisposition(errors, _) => Some(errors)
    case _                                     => None
  }

  val meta: Option[Map[String, JAny]] = disposition match {
    case Document.MetaDisposition(meta)       => Some(meta)
    case Document.DataDisposition(_, _, meta) => meta
    case Document.ErrorsDisposition(_, meta)  => meta
  }

  // Sorts the included resource objects for easier diffing. Order should not be significant.
  def sortIncludes: encoding.Document =
    disposition match {
      case _: Document.MetaDisposition    => this
      case _: Document.ErrorsDisposition  => this
      case disp: Document.DataDisposition => this.copy(disposition = disp.copy(included = disp.included.map(_.sorted)))
    }

  def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): Document =
    copy(disposition = disposition.mapMeta(fn))
}

case object Document {

  /** Documents must have `meta`, `data` or `errors`. If it's either of the latter two, `meta` is optional as well.
    * This encodes that constraint statically into the JSON:API document model.
    *
    * https://jsonapi.org/format/#document-top-level
    */
  sealed trait Disposition {
    def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): Disposition
  }

  final case class MetaDisposition(
      meta: Map[String, JAny]
  ) extends Disposition {
    override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): Disposition =
      copy(meta = fn(Some(meta)).getOrElse(Map.empty))
  }

  final case class DataDisposition(
      data: PrimaryData,
      included: Option[List[ResourceObject]] = None,
      meta: Option[Map[String, JAny]] = None
  ) extends Disposition
      with HasMeta[DataDisposition] {
    override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): DataDisposition =
      copy(meta = fn(meta))

    override def equals(obj: Any): Boolean =
      obj match {
        case that: DataDisposition =>
          // Order of `included` is not significant for comparison.
          this.data == that.data && this.meta == that.meta && this.included.map(_.sorted) == that.included.map(_.sorted)
        case _ => false
      }
  }

  final case class ErrorsDisposition(
      errors: List[Error],
      meta: Option[Map[String, JAny]] = None
  ) extends Disposition
      with HasMeta[ErrorsDisposition] {
    override def mapMeta(fn: Option[Map[String, JAny]] => Option[Map[String, JAny]]): ErrorsDisposition =
      copy(meta = fn(meta))
  }

  def forData(
      data: PrimaryData,
      included: Option[List[ResourceObject]] = None,
      meta: Option[Map[String, JAny]] = None,
      jsonapi: Option[Jsonapi] = None,
      links: Option[Map[String, Link]] = None,
  ): Document = Document(DataDisposition(data, included, meta), jsonapi, links)

  def forErrors(
      errors: List[Error],
      meta: Option[Map[String, JAny]] = None,
      jsonapi: Option[Jsonapi] = None,
      links: Option[Map[String, Link]] = None,
  ): Document = Document(ErrorsDisposition(errors, meta), jsonapi, links)

  def forMeta(
      meta: Map[String, JAny],
      jsonapi: Option[Jsonapi] = None,
      links: Option[Map[String, Link]] = None,
  ): Document = Document(MetaDisposition(meta), jsonapi, links)

  implicit val encoder: Encoder[Document, JObject] = { in =>
    def optionally[A](name: String, oa: Option[A])(implicit enc: Encoder[A, JAny]) =
      oa.map(a => name -> enc.encode(a))

    JObject.fromOptions(
      optionally("errors", in.errors),
      optionally("data", in.data),
      optionally("included", in.included),
      optionally("meta", in.meta),
      optionally("jsonapi", in.jsonapi),
      optionally("links", in.links)
    )
  }
}
