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

package org.scalawag.bateman.jsonapi.encoding

import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.semiauto.unchecked._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{JAny, JAnyEncoder, JObject, JObjectEncoder, JStringEncoder, Nullable}
import org.scalawag.bateman.jsonapi._

sealed trait Document {
  type Self <: Document

  /** The metadata for this document.
    * [[None]] means that it will be excluded from the encoded document.
    * [[Some]] means that it will be included, even if empty.
    */
  val meta: Option[Meta]

  /** The `meta` metadata for this document.
    * [[None]] means that it will be excluded from the encoded document.
    * [[Some]] means that it will be included, even if empty.
    */
  val jsonapi: Option[Jsonapi]

  /** The `links` for this document.
    * [[None]] means that it will be excluded from the encoded document.
    * [[Some]] means that it will be included, even if empty.
    */
  val links: Option[Links]

  /** Adds (or overwrites) the specified key/value pair into the document's `meta` object. */
  def withMeta[A: JAnyEncoder](key: String, value: A): Self = withMetas(key -> value.toJAny)

  /** Adds (or overwrites) the specified key/value pairs into the document's `meta` object. */
  def withMetas(meta: (String, JAny)*): Self

  /** Adds (or overwrites) the specified link to the document's `links`. */
  def withLink(name: String, value: Link): Self = withLinks(name -> value)

  /** Adds (or overwrites) the specified key/value pairs into the document's `meta` object. */
  def withLinks(links: (String, Link)*): Self

  /** Replaces the document's `jsonapi` value with the one specified. */
  def withJsonapi(jsonapi: Jsonapi): Self
}

case object Document {
  implicit val batemanEncoder: JObjectEncoder[Document] = {
    case (doc: DataDocument, discs)     => JObjectEncoder[DataDocument].encode(doc, discs)
    case (doc: ErrorDocument, discs)    => JObjectEncoder[ErrorDocument].encode(doc, discs)
    case (doc: MetadataDocument, discs) => JObjectEncoder[MetadataDocument].encode(doc, discs)
  }

  /** Creates a document with the specified key/value pair in its `meta` object. */
  def withMeta[A: JAnyEncoder](key: String, value: A): MetadataDocument = withMetas(key -> value.toJAny)

  /** Creates a document with the specified key/value pairs in its `meta` object. */
  def withMetas(meta: (String, JAny)*): MetadataDocument = MetadataDocument(requiredMeta = meta)

  /** Creates a document with the specified primary data. */
  def withData[A: JObjectEncoder](data: A): DataDocument = DataDocument(data = data.toJAny)

  /** Creates a document with the specified primary data. */
  def withData[A: JObjectEncoder](data: Nullable[A]): DataDocument = DataDocument(data = data.toJAny)

  /** Creates a document with the specified primary data. */
  def withData[A: JObjectEncoder](data: Seq[A]): DataDocument = DataDocument(data = data.toJAny)

  /** Creates a document with the specified error. */
  def withError(error: Error): ErrorDocument =
    withErrors(error)

  /** Creates a document with the specified errors. */
  def withErrors(errors: Error*): ErrorDocument = encoding.ErrorDocument(errors = errors)
}

/** A JSON:API document that contains [[https://jsonapi.org/format/#document-top-level primary data]].
  *
  * [[None]] for any these [[Option]]s means that the key will not appear in the encoded document.
  * [[Some]] means that they will appear, even if empty.
  */
final case class DataDocument(
    override val jsonapi: Option[Jsonapi] = None,
    data: JAny,
    included: Option[Included] = None,
    override val meta: Option[Meta] = None,
    override val links: Option[Links] = None
) extends Document {
  override type Self = DataDocument

  override def withMetas(meta: (String, JAny)*): DataDocument =
    this.copy(meta = Some(this.meta.getOrElse(Nil) ++ meta))

  override def withLinks(links: (String, Link)*): DataDocument =
    this.copy(links = Some(this.links.getOrElse(Nil) ++ links))

  override def withJsonapi(jsonapi: Jsonapi): DataDocument =
    this.copy(jsonapi = Some(jsonapi))

  /** Adds the specified resource objects to the included array of this document. */
  def withIncluded(resources: ResourceObject*): DataDocument =
    this.copy(included = Some(this.included.getOrElse(Nil) ++ resources))
}

object DataDocument {
  implicit val batemanEncoder: JObjectEncoder[DataDocument] = deriveEncoderForCaseClass[DataDocument]()
}

/** A JSON:API document that contains [[https://jsonapi.org/format/#document-top-level errors]].
  *
  * [[None]] for any these [[Option]]s means that the key will not appear in the encoded document.
  * [[Some]] means that they will appear, even if empty.
  */

final case class ErrorDocument(
    override val jsonapi: Option[Jsonapi] = None,
    errors: Errors,
    override val meta: Option[Meta] = None,
    override val links: Option[Links] = None
) extends Document {
  override type Self = ErrorDocument

  override def withMetas(meta: (String, JAny)*): ErrorDocument =
    this.copy(meta = Some(this.meta.getOrElse(Nil) ++ meta))

  override def withLinks(links: (String, Link)*): ErrorDocument =
    this.copy(links = Some(this.links.getOrElse(Nil) ++ links))

  override def withJsonapi(jsonapi: Jsonapi): ErrorDocument =
    this.copy(jsonapi = Some(jsonapi))

  /** Adds the specified error to this error document. */
  def withError(error: Error): ErrorDocument =
    withErrors(error)

  /** Adds the specified errors to this error document. */
  def withErrors(errors: Error*): ErrorDocument =
    this.copy(errors = this.errors ++ errors)
}

object ErrorDocument {
  implicit val batemanEncoder: JObjectEncoder[ErrorDocument] = deriveEncoderForCaseClass[ErrorDocument]()
}

/** A JSON:API document that contains at least [[https://jsonapi.org/format/#document-top-level metadata]].
  * It can be turned into a [[DataDocument]] or an [[ErrorDocument]] by adding either data or errors.
  *
  * [[None]] for any these [[Option]]s means that the key will not appear in the encoded document.
  * [[Some]] means that they will appear, even if empty.
  */

final case class MetadataDocument(
    override val jsonapi: Option[Jsonapi] = None,
    requiredMeta: Meta,
    override val links: Option[Links] = None
) extends Document {
  override type Self = MetadataDocument

  override val meta: Option[Meta] = Some(requiredMeta)

  override def withMetas(meta: (String, JAny)*): MetadataDocument =
    new MetadataDocument(this.jsonapi, this.requiredMeta ++ meta, this.links)

  override def withLinks(links: (String, Link)*): MetadataDocument =
    new MetadataDocument(this.jsonapi, this.requiredMeta, Some(this.links.getOrElse(Nil) ++ links))

  override def withJsonapi(jsonapi: Jsonapi): MetadataDocument =
    new MetadataDocument(Some(jsonapi), this.requiredMeta, this.links)

  /** Returns an error document including the specified error. */
  def withError(error: Error): ErrorDocument =
    withErrors(error)

  /** Returns an error document including the specified errors. */
  def withErrors(errors: Error*): ErrorDocument =
    encoding.ErrorDocument(this.jsonapi, errors, this.meta, this.links)

  /** Adds primary data with the JSON representation of the specified value to the document. */
  def withData[A: JObjectEncoder](data: A): DataDocument =
    DataDocument(this.jsonapi, data.toJAny, None, this.meta, this.links)

  /** Adds the primary data with the JSON representation of the specified value or `null` to the document. */
  def withData[A: JObjectEncoder](data: Nullable[A]): DataDocument =
    DataDocument(this.jsonapi, data.toJAny, None, this.meta, this.links)

  /** Adds the primary data with an array of the JSON representations of the specified values to the document. */
  def withData[A: JObjectEncoder](data: Seq[A]): DataDocument =
    DataDocument(this.jsonapi, data.toJAny, None, this.meta, this.links)
}

object MetadataDocument {
  private val requiredMetaToMeta: Config => Config =
    _.withExplicitFieldNameMapping {
      case "requiredMeta" => "meta"
    }

  implicit val batemanEncoder: JObjectEncoder[MetadataDocument] =
    deriveEncoderForCaseClass[MetadataDocument](requiredMetaToMeta)
}

final case class Jsonapi(
    version: Option[String] = None,
    meta: Option[Meta] = None
)

object Jsonapi {
  implicit val batemanEncoder: JObjectEncoder[Jsonapi] =
    deriveEncoderForCaseClass[Jsonapi]()
}

sealed trait ErrorSource

object ErrorSource {

  final case class Pointer(pointer: String) extends ErrorSource

  object Pointer {
    implicit val batemanEncoder: JObjectEncoder[Pointer] = deriveEncoderForCaseClass[Pointer]()
  }

  final case class Parameter(parameter: String) extends ErrorSource

  object Parameter {
    implicit val batemanEncoder: JObjectEncoder[Parameter] = deriveEncoderForCaseClass[Parameter]()
  }

  final case class Header(header: String) extends ErrorSource

  object Header {
    implicit val batemanEncoder: JObjectEncoder[Header] = deriveEncoderForCaseClass[Header]()
  }

  implicit val batemanEncoder: JObjectEncoder[ErrorSource] = {
    case (src: Pointer, discs)   => JObjectEncoder[Pointer].encode(src, discs)
    case (src: Parameter, discs) => JObjectEncoder[Parameter].encode(src, discs)
    case (src: Header, discs)    => JObjectEncoder[Header].encode(src, discs)
  }
}

final case class Error(
    id: Option[String] = None,
    status: Option[String] = None,
    code: Option[String] = None,
    title: Option[String] = None,
    detail: Option[String] = None,
    source: Option[ErrorSource] = None,
    links: Option[Links] = None,
    meta: Option[Meta] = None
) {
  def withId(id: String): Error = this.copy(id = Some(id))
  def withStatus(status: Int): Error = this.copy(status = Some(status.toString))
  def withCode(code: String): Error = this.copy(code = Some(code))
  def withTitle(title: String): Error = this.copy(title = Some(title))
  def withDetail(detail: String): Error = this.copy(detail = Some(detail))
  def withSource(source: ErrorSource): Error = this.copy(source = Some(source))
  def withLink(key: String, value: Link): Error =
    this.copy(links = Some(this.links.getOrElse(Nil) :+ (key, value)))
  def withMeta[A: JAnyEncoder](key: String, value: A): Error =
    this.copy(meta = Some(this.meta.getOrElse(Nil) :+ (key, value.toJAny)))
}

object Error {
  implicit val batemanEncoder: JObjectEncoder[Error] =
    deriveEncoderForCaseClass[Error]()
}

sealed trait Resource {
  type Self <: Resource

//  val id: Option[String]
  val localId: Boolean
  val resourceType: String
  val meta: Option[Meta]

  def withMeta[A: JAnyEncoder](key: String, value: A): Self
}

final case class ResourceIdentifier(
    resourceType: String,
    id: String,
    localId: Boolean = false,
    meta: Option[Meta] = None
) extends Resource {
  override type Self = ResourceIdentifier
  override def withMeta[A: JAnyEncoder](key: String, value: A): ResourceIdentifier =
    this.copy(meta = Some(this.meta.getOrElse(Nil) :+ (key, value.toJAny)))
}

object ResourceIdentifier {
  // Needs custom encoder to support id/lid logic.
  implicit val batemanEncoder: JObjectEncoder[ResourceIdentifier] = (ri, _) => {
    import org.scalawag.bateman.json.focus.weak._
    import org.scalawag.bateman.json.state._
    import org.scalawag.bateman.jsonapi.lens._

    val create = for {
      _ <- encodeTo(resourceType, ri.resourceType.toJAny)
      _ <-
        if (ri.localId)
          encodeTo(lid, ri.id)
        else
          encodeTo(id, ri.id)
      _ <- encodeTo(meta, ri.meta)
    } yield ()

    create.runS(JObject.Empty.asRootFocus).flatMap(_.asObject).getOrThrow.value
  }

  def withLid(resourceType: String, id: String): ResourceIdentifier =
    ResourceIdentifier(resourceType, id, localId = true)
}

final case class ResourceObject(
    resourceType: String,
    id: Option[String] = None,
    localId: Boolean = false,
    attributes: Option[Attributes] = None,
    relationships: Option[Relationships] = None,
    meta: Option[Meta] = None,
    links: Option[Links] = None
) extends Resource {
  override type Self = ResourceObject

  def withMeta[A: JAnyEncoder](key: String, value: A): ResourceObject =
    this.copy(meta = Some(this.meta.getOrElse(Seq.empty) :+ (key, value.toJAny)))
  def withAttribute[A: JAnyEncoder](key: String, value: A): ResourceObject =
    this.copy(attributes = Some(this.attributes.getOrElse(Seq.empty) :+ (key, value.toJAny)))
  def withRelationship(key: String, value: Relationship): ResourceObject =
    this.copy(relationships = Some(this.relationships.getOrElse(Seq.empty) :+ (key, value)))
  def withLink(key: String, value: Link): ResourceObject =
    this.copy(links = Some(this.links.getOrElse(Seq.empty) :+ (key, value)))
}

object ResourceObject {
  // Needs custom encoder to support id/lid logic.
  implicit val batemanEncoder: JObjectEncoder[ResourceObject] = (ro, _) => {
    import org.scalawag.bateman.json.focus.weak._
    import org.scalawag.bateman.json.state._
    import org.scalawag.bateman.jsonapi.lens._

    val create = for {
      _ <- encodeTo(resourceType, ro.resourceType.toJAny)
      _ <-
        if (ro.localId)
          encodeTo(lid, ro.id)
        else
          encodeTo(id, ro.id)
      _ <- encodeTo(attributes, ro.attributes)
      _ <- encodeTo(relationships, ro.relationships)
      _ <- encodeTo(meta, ro.meta)
      _ <- encodeTo(links, ro.links)
    } yield ()

    create.runS(JObject.Empty.asRootFocus).flatMap(_.asObject).getOrThrow.value
  }

  def apply(resourceType: String, id: String): ResourceObject = ResourceObject(resourceType, Some(id))

  def withLid(resourceType: String, id: String): ResourceObject = ResourceObject(resourceType, Some(id), localId = true)
}

final case class Relationship(
    data: Option[JAny] = None,
    meta: Option[Meta] = None,
    links: Option[Links] = None
) {
  def withMeta[A: JAnyEncoder](key: String, value: A): Relationship =
    this.copy(meta = Some(this.meta.getOrElse(Seq.empty) :+ (key, value.toJAny)))
  def withLink(key: String, value: Link): Relationship =
    this.copy(links = Some(this.links.getOrElse(Seq.empty) :+ (key, value)))
}

object Relationship {
  implicit val batemanEncoder: JObjectEncoder[Relationship] =
    deriveEncoderForCaseClass[Relationship]()

  def apply[A: JObjectEncoder](data: A): Relationship = new Relationship(Some(data.toJAny))
  def apply[A: JObjectEncoder](data: Nullable[A]): Relationship = new Relationship(Some(data.toJAny))
  def apply[A: JObjectEncoder](data: Seq[A]): Relationship = new Relationship(Some(data.toJAny))
}

sealed trait Link

object Link {
  implicit val batemanEncoder: JAnyEncoder[Link] = {
    case in: BareLink => in.toJAny
    case in: RichLink => in.toJAny
  }

  implicit def fromString(href: String): BareLink = BareLink(href)
}

final case class BareLink(href: String) extends Link

object BareLink {
  import cats.syntax.contravariant._
  implicit val batemanEncoder: JStringEncoder[BareLink] = JStringEncoder[String].contramap(_.href)
}

final case class RichLink(
    href: String,
    rel: Option[String] = None,
    describedby: Option[Link] = None,
    title: Option[String] = None,
    linkType: Option[String] = None,
    hreflang: Option[String] = None,
    meta: Option[Meta] = None
) extends Link {
  def withMeta[A: JAnyEncoder](key: String, value: A): RichLink =
    this.copy(meta = Some(this.meta.getOrElse(Seq.empty) :+ (key, value.toJAny)))
}

object RichLink {
  private val linkTypeToType: Config => Config =
    _.withExplicitFieldNameMapping {
      case "linkType" => "type"
    }

  implicit val batemanEncoder: JObjectEncoder[RichLink] =
    deriveEncoderForCaseClass[RichLink](linkTypeToType)
}
