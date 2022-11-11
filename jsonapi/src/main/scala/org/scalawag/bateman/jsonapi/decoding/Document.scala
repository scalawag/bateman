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

import cats.data.NonEmptyChain
import cats.syntax.validated._
import cats.syntax.apply._
import org.scalawag.bateman.json.decoding.query.{Query, TraverseQuery, root}
import org.scalawag.bateman.json.decoding.{ContextualDecoder, DecodeResult, Decoder, JArray, JObject, JString}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.{SourceTag, semiauto}
import org.scalawag.bateman.json.validIfEmpty
import org.scalawag.bateman.jsonapi.encoding
import shapeless.tag.@@

// None for all these Options means that the key didn't appear in the document. If they're set to "null" or empty,
// they will have a Some.

final case class Document(
    src: JSource @@ SourceTag,
    data: Option[PrimaryData] = None,
    errors: Option[Errors] = None,
    meta: Option[Meta] = None,
    jsonapi: Option[Jsonapi] = None,
    links: Option[Links] = None,
    included: Option[List[ResourceObject]] = None,
) extends HasLinks
    with HasMeta
    with HasData[PrimaryData] {

  /** Contains all of the included resource objects as well as any from the primary data. */

  private lazy val includedMap = {
    val resourceObjectsFromData = data.toList.flatMap(_.toList).collect { case r: ResourceObject => r }
    val resourceObjectsFromIncluded = included.getOrElse(Nil)
    val allResourceObjects = resourceObjectsFromData ::: resourceObjectsFromIncluded

    import cats.syntax.foldable._
    import cats.instances.map._

    allResourceObjects.map(ro => Map(ro.`type`.value -> Map(ro.id.value -> List(ro)))).combineAll
  }

  def optionalIncluded(identifier: ResourceIdentifier): DecodeResult[Option[ResourceObject]] =
    includedMap.get(identifier.`type`.value) match {
      case None => None.validNec
      case Some(byId) =>
        byId.get(identifier.id.value) match {
          case None           => None.validNec
          case Some(Nil)      => None.validNec
          case Some(h :: Nil) => Some(h).validNec
          case Some(l)        => DuplicateResourceObjectDefinition(l.head, NonEmptyChain.fromSeq(l.tail).get).invalidNec
        }
    }

  def requiredIncluded(identifier: ResourceIdentifier): DecodeResult[ResourceObject] =
    optionalIncluded(identifier).andThen(
      _.map(_.validNec).getOrElse(MissingIncludedResourceObject(identifier).invalidNec)
    )

  // Runs extra validation for strict adherence to the spec. Totally optional.
  def validate: DecodeResult[Document] = {
    def checkDuplicateIncludes: DecodeResult[Unit] = {
      val duplicateErrors =
        for {
          byType <- includedMap.values
          byTypeAndId <- byType.values
          if byTypeAndId.length > 1
        } yield {
          DuplicateResourceObjectDefinition(byTypeAndId.head, NonEmptyChain.fromSeq(byTypeAndId.tail).get)
        }

      validIfEmpty(duplicateErrors, ())
    }

    def checkResourceLinkage: DecodeResult[Unit] = ().validNec // TODO

    (checkDuplicateIncludes, checkResourceLinkage).tupled.map(_ => this)
  }

  def toEncoding: encoding.Document =
    if (data.nonEmpty)
      encoding.Document.forData(
        data = data.get.toEncoding,
        included = included.map(_.map(_.toEncoding)),
        meta = meta.map(_.toEncoding),
        jsonapi = jsonapi.map(_.toEncoding),
        links = links.map(_.toEncoding)
      )
    else if (errors.nonEmpty)
      encoding.Document.forErrors(
        errors = errors.get.toEncoding,
        meta = meta.map(_.toEncoding),
        jsonapi = jsonapi.map(_.toEncoding),
        links = links.map(_.toEncoding)
      )
    else if (meta.nonEmpty)
      encoding.Document.forMeta(
        meta = meta.get.toEncoding,
        jsonapi = jsonapi.map(_.toEncoding),
        links = links.map(_.toEncoding)
      )
    else
      throw new IllegalArgumentException("invalid JSON:API doc can't be represented as encoding")

  def dquery[To](fn: Query[Document, Document, Document] => Query[Document, To, Document]): DecodeResult[To] =
    fn(root[Document, Document])(this, this)

  def dtquery[F[_], To](fn: Query[Document, Document, Document] => TraverseQuery[F, Document, To, Document]): DecodeResult[F[To]] =
    fn(root[Document, Document])(this, this)

  def primaryDatumAs[To](implicit decoder: ContextualDecoder[ResourceLike, To, Document]): DecodeResult[To] = {
    import org.scalawag.bateman.json.decoding.query._
    import org.scalawag.bateman.jsonapi.query
    dquery[To](_ ~> query.data ~> query.required ~> as[To])
  }
}

case object Document {
  implicit val decoder: Decoder[JObject, Document] = semiauto.deriveDecoderForCaseClass[Document, Any]()
}

final case class Jsonapi(
    src: JSource @@ SourceTag,
    version: Option[JString] = None,
    meta: Option[Meta] = None
) {
  def toEncoding: encoding.Jsonapi = encoding.Jsonapi(version = version.map(_.value), meta = meta.map(_.toEncoding))
}

object Jsonapi {
  implicit val decoder: Decoder[JObject, Jsonapi] = semiauto.deriveDecoderForCaseClass[Jsonapi, Any]()
}
