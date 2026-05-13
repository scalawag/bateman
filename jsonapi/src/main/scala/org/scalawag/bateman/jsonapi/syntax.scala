// bateman -- Copyright 2021-2026 -- Justin Patterson
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

package org.scalawag.bateman.jsonapi
import cats.syntax.either._
import cats.syntax.traverse._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{JNull, noneIfEmpty}
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.Encoded
import org.scalawag.bateman.jsonapi.encoding.{
  DataDocument,
  Document,
  EncodeResult,
  FieldsSpec,
  IncludeSpec,
  Inclusions,
  InfallibleIncludeSpec,
  ResourceEncoder
}

object syntax {
  implicit class AnyBatemanJsonApiOps[A](a: A) {
    def toDocument(implicit enc: ResourceEncoder[A]): Document = toDocument()

    def toDocument(
        includeSpec: InfallibleIncludeSpec = IncludeSpec.Opportunistically,
        fieldsSpec: FieldsSpec.Infallible = FieldsSpec.All
    )(implicit enc: ResourceEncoder[A]): Document =
      enc.encodeInfallibly(a, includeSpec, fieldsSpec).toDocument

    def toDocument(includeSpec: IncludeSpec, fieldsSpec: FieldsSpec)(implicit
        enc: ResourceEncoder[A]
    ): EncodeResult[Document] =
      enc.encodeResource(a, includeSpec, fieldsSpec).map(_.toDocument)
  }

  implicit class SeqAnyBatemanJsonApiOps[A](a: Seq[A]) {
    def toDocument(implicit enc: ResourceEncoder[A]): Document = toDocument()

    private def encodedsToDocument(encodeds: List[Encoded]): Document = {
      val roots = encodeds.map(_.root)
      val allInclusions = encodeds.map(_.inclusions).foldLeft(Inclusions.empty)(Inclusions.monoid.combine)
      DataDocument(data = roots.toJAny, included = noneIfEmpty(allInclusions.objects).map(_.toList))
    }

    def toDocument(
        includeSpec: InfallibleIncludeSpec = IncludeSpec.Opportunistically,
        fieldsSpec: FieldsSpec.Infallible = FieldsSpec.All
    )(implicit enc: ResourceEncoder[A]): Document = {
      encodedsToDocument(a.map(enc.encodeInfallibly(_, includeSpec, fieldsSpec)).toList)
    }

    def toDocument(includeSpec: IncludeSpec, fieldsSpec: FieldsSpec)(implicit
        enc: ResourceEncoder[A]
    ): EncodeResult[Document] =
      a.toList.traverse(enc.encodeResource(_, includeSpec, fieldsSpec)).map(encodedsToDocument)
  }

  implicit class OptionBatemanJsonApiOps[A](a: Option[A]) {
    def toDocument(implicit enc: ResourceEncoder[A]): Document = toDocument()

    def toDocument(
        includeSpec: InfallibleIncludeSpec = IncludeSpec.Opportunistically,
        fieldsSpec: FieldsSpec.Infallible = FieldsSpec.All
    )(implicit enc: ResourceEncoder[A]): Document =
      a match {
        case Some(s) =>
          s.toDocument(includeSpec, fieldsSpec)
        case None =>
          DataDocument(data = JNull)
      }

    def toDocument(includeSpec: IncludeSpec, fieldsSpec: FieldsSpec)(implicit
        enc: ResourceEncoder[A]
    ): EncodeResult[Document] =
      a match {
        case Some(s) =>
          s.toDocument(includeSpec, fieldsSpec)
        case None =>
          DataDocument(data = JNull).asRight
      }
  }
}
