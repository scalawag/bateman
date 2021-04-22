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

import cats.data.NonEmptyChain
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import org.scalawag.bateman.json.encoding.{Encoder, JAny}
import org.scalawag.bateman.json.noneIfEmpty
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.PartiallyEncoded

import scala.annotation.tailrec

object GraphEncoder {
//  protected def encode[F[_]: Monad](a: A, context: EncodeContext[F]): F[EncodedResourceObject]
//
//  def encode[F[_]: Monad](
//      loader: Loader[F],
//      a: A,
//      include: List[String] = Nil,
//      fields: Map[String, Set[String]] = Map.empty,
//  ): F[EncodedResourceObject] = encode(a, EncodeContext(loader, include, fields))
//
//  def encode(
//      a: A,
//      include: List[String] = Nil,
//      fields: Map[String, Set[String]] = Map.empty
//  ): EncodedResourceObject = encode(Loader.none, a, include, fields)
//
//  def encodeDeep[F[_], A](a: A, context: EncodeContext[F])(implicit
//      enc: ResourceEncoder[A, ResourceObject]
//  ): EncodeContext[F] = {
//    enc.encode(a)
//    ???
//  }

  private def prepareData[A <: ResourceLike](
      encodeds: List[PartiallyEncoded[A]]
  ): EncodeResult[(List[A], List[ResourceObject])] = {
    // This will resolve all of the deferred encodings _or_ return an error if it can't.
//    @tailrec
    def iterate(inclusions: Inclusions, deferred: Set[DeferredEncoding]): EncodeResult[Inclusions] = {
      val stillUnresolved = deferred.filterNot(x => inclusions.contains(x.identifier))
      if (stillUnresolved.isEmpty)
        inclusions.validNec
      else {
        // See what can be resolved in our existing Inclusions and remove them.

//        val results = ???
        NonEmptyChain.fromSeq(stillUnresolved.map(x => UnavailableIncludePath(x.includeSpec.path)).toList).get.invalid
      }
    }

    // These will be our primary data.
    val primaryData = encodeds.map(_.root)

    // These are the things that should not be in the included array because they are in primary data.
    val primaryDataObjects = primaryData.collect { case ro: ResourceObject => ro }

    // Everything that was included by the incoming encodeds as well as the objects in our primary data, in one place.
    val allInclusions =
      encodeds.map(_.inclusions).combineAll combine primaryDataObjects.foldLeft(Inclusions.empty)(_ + _)

    // All the deferrals that came in through the encodeds.
    val allDeferrals = encodeds.flatMap(_.deferrals).toSet

    iterate(allInclusions, allDeferrals) map { inclusions =>
      // We managed to resolve all the deferred encodings. Get a list of ResourceObjects to include in the Document.
      // Remove the primary data objects from the inclusions, because they'll already be part of the document.
      val includedObjects = inclusions.objects.toSet -- primaryDataObjects

      // Remove anything from the inclusions that only contains a type and id, because they're useless.
      val filteredObjects = includedObjects.filter(o => o != ResourceObject(o.`type`, o.id))

      (primaryData, filteredObjects.toList)
    }
  }

  def encodeIdentifiers[A](
      aa: List[A],
      includeSpec: IncludeSpec = IncludeSpec.Never,
      fieldsSpec: FieldsSpec = FieldsSpec.All
  )(implicit
      enc: ResourceIdentifierEncoder[A]
  ): EncodeResult[Document] =
    aa.traverse(enc.encodeResource(_, includeSpec, fieldsSpec)).andThen(prepareData).map {
      case (data, includes) =>
        Document.forData(
          data = data,
          included = noneIfEmpty(includes)
        )
    }

  def encodeIdentifier[A](
      aa: Option[A],
      includeSpec: IncludeSpec = IncludeSpec.Never,
      fieldsSpec: FieldsSpec = FieldsSpec.All
  )(implicit
      enc: ResourceIdentifierEncoder[A]
  ): EncodeResult[Document] =
    aa.traverse(enc.encodeResource(_, includeSpec, fieldsSpec)).map(_.toList).andThen(prepareData).map {
      case (List(data), includes) => Document.forData(data = data, included = noneIfEmpty(includes))
      case (Nil, includes)        => Document.forData(data = NullData, included = noneIfEmpty(includes))
    }

  def encodeObjects[A](
      aa: List[A],
      includeSpec: IncludeSpec = IncludeSpec.Never,
      fieldsSpec: FieldsSpec = FieldsSpec.All
  )(implicit
      enc: ResourceObjectEncoder[A]
  ): EncodeResult[Document] =
    aa.traverse(enc.encodeResource(_, includeSpec, fieldsSpec)).andThen(prepareData).map {
      case (data, includes) =>
        Document.forData(
          data = data,
          included = noneIfEmpty(includes)
        )
    }

  def encodeObject[A](
      aa: Option[A],
      includeSpec: IncludeSpec = IncludeSpec.Never,
      fieldsSpec: FieldsSpec = FieldsSpec.All
  )(implicit
      enc: ResourceObjectEncoder[A]
  ): EncodeResult[Document] =
    aa.traverse(enc.encodeResource(_, includeSpec, fieldsSpec)).map(_.toList).andThen(prepareData).map {
      case (List(data), includes) => Document.forData(data = data, included = noneIfEmpty(includes))
      case (Nil, includes)        => Document.forData(data = NullData, included = noneIfEmpty(includes))
    }

  def encodeObjectOptionalId[A](
      aa: Option[A],
      includeSpec: IncludeSpec = IncludeSpec.Never,
      fieldsSpec: FieldsSpec = FieldsSpec.All
  )(implicit
      enc: ResourceObjectOptionalIdEncoder[A]
  ): EncodeResult[Document] =
    aa.traverse(enc.encodeResource(_, includeSpec, fieldsSpec)).map(_.toList).andThen(prepareData).map {
      case (List(data), includes) => Document.forData(data = data, included = noneIfEmpty(includes))
      case (Nil, includes)        => Document.forData(data = NullData, included = noneIfEmpty(includes))
    }

//  def encodeDataDocument[F[_], A, B <: ResourceIdentifierLike](loader: Loader[F])(
//      aa: List[A],
//      includeSpec: IncludeSpec = IncludeSpec.Never,
//      fieldsSpec: FieldsSpec = FieldsSpec.all
//  )(implicit
//      enc: ResourceEncoder[A, B]
//  ): EncodeResult[Document] =
//    aa.traverse(enc.encode(_, includeSpec, fieldsSpec)).andThen { encodeds => }

//  def encodeDatum[A <: ResourceLike](encoded: PartiallyEncoded[A]): Document

}
