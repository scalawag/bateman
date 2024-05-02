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

import cats.data.NonEmptyChain
import cats.syntax.validated._
import org.scalatest.{Assertion, Inside}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.{NotNull, Null, Nullable, ParserTestUtils}
import org.scalawag.bateman.json.decoding.DecodeError.formatErrorReport
import org.scalawag.bateman.json.decoding.{DecodeError, JObject, UnexpectedValue}
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, SnakeCase}
import org.scalawag.bateman.jsonapi.query._
import org.scalawag.bateman.jsonapi.Model.{Blue, Green, RGB, Red}
import org.scalawag.bateman.jsonapi.{ResourceObjectCodec, encoding}
import org.scalawag.bateman.jsonapi.decoding.{
  Document,
  MissingIncludedResourceObject,
  ResourceIdentifier,
  ResourceObject,
  ResourceObjectDecoder
}
import org.scalawag.bateman.jsonapi.encoding.{GraphEncoder, IncludeSpec}
import org.scalawag.bateman.jsonapi.generic.CaseClassResourceCodec
import org.scalawag.bateman.jsonapi.generic.TestModel._

import java.util.UUID
import scala.annotation.tailrec
import scala.util.Try

class HListResourceDecoderTest extends AnyFunSpec with Matchers with ParserTestUtils with Inside {
  describe("allowUnknownFields = false") {
    val decoders = makeDecoders(
      Config.default.copy(allowUnknownFields = false, fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
    )
    import decoders._

    it("should decode with allowUnknownFields = false") {
      val square =
        Square(
          UUID.randomUUID,
          Some("blue square"),
          List(Red, Blue, RGB(3, 4, 3), Green),
          17
        )

      square.copy(sideLength = 11)

      val doc = parseAs[Document]("""
        {
          "data" : {
            "type" : "square",
            "id" : "55555555-5555-5555-5555-555555555555",
            "attributes" : {
              "label": "mike",
              "color" : ["red", "blue", { "r": 1, "g": 0, "b": 1 }],
              "side_length" : 12
            },
            "relationships": {
              "parent": {
                "data": {
                  "type": "square",
                  "id": "4"
                }
              }
            },
            "links": {
            },
            "meta": {
              "version": 7
            }
          }
        }
      """)

      val decoded = doc.cquery(doc)(_ ~> data ~> required ~> as[ResourceObject] ~> as[Square])
      val un1 = doc.src.root.query(_ ~> "data" ~> "relationships" ~> "parent").getOrElse(fail)
      val un2 = doc.src.root.query(_ ~> "data" ~> "meta" ~> "version").getOrElse(fail)
      decoded.leftMap(_.iterator.toSet) shouldBe Set(UnexpectedValue(un1), UnexpectedValue(un2)).invalid
    }
  }

  it("should decode defaulted relationship from missing include") {
    import HListResourceDecoderTest._
    import org.scalawag.bateman.json.syntax._

    val c = CDef("1", BDef("2", Null))
    val doc = GraphEncoder.encodeObject(Some(c), IncludeSpec.Opportunistically).getOrElse(fail)
    val ddoc = parseAs[Document](doc.toJAny.render)
    ddoc.included shouldBe None // No includes due to default values
    val cdec = ddoc.requiredData
      .andThen(_.required)
      .andThen(_.as[ResourceObject])
      .andThen(CDef.codec.decoder.decode(_, ddoc))
      .fold(ee => fail(DecodeError.formatErrorReport(ee)), identity)
    cdec shouldBe c
  }

  it("should decode optional relationship from missing include") {
    import HListResourceDecoderTest._
    import org.scalawag.bateman.json.syntax._

    val c = COpt("1", BOpt("2", None))
    val doc = GraphEncoder.encodeObject(Some(c), IncludeSpec.Opportunistically).getOrElse(fail)
    val ddoc = parseAs[Document](doc.toJAny.render)
    ddoc.included shouldBe None // No includes due to default values
    val cdec = ddoc.requiredData
      .andThen(_.required)
      .andThen(_.as[ResourceObject])
      .andThen(COpt.codec.decoder.decode(_, ddoc))
      .fold(ee => fail(DecodeError.formatErrorReport(ee)), identity)
    cdec shouldBe c
  }

  it("should correctly report missing include when stub include doesn't have resource") {
    import HListResourceDecoderTest._
    import org.scalawag.bateman.json.syntax._

    val c = CDef("1", BDef("2", Null))
    val doc = GraphEncoder.encodeObject(Some(c), IncludeSpec.Opportunistically).getOrElse(fail)
    val ddoc = parseAs[Document](doc.toJAny.render)
    ddoc.included shouldBe None // No includes due to default values during encode
    val cdec = ddoc.requiredData
      .andThen(_.required)
      .andThen(_.as[ResourceObject])
      .andThen(C.codec.decoder.decode(_, ddoc))
      .toEither
      .left
      .getOrElse(fail)
      .iterator
      .toList
    inside(cdec) {
      case List(MissingIncludedResourceObject(ResourceIdentifier(_, typ, id, _))) =>
        typ.value shouldBe "B"
        id.value shouldBe "2"
    }
  }
}

object HListResourceDecoderTest {
  import shapeless.tag.@@
  import org.scalawag.bateman.jsonapi.generic.{IdTag, RelationshipTag, semiauto}

  case class A(id: String @@ IdTag)

  object A {
    implicit val codec: CaseClassResourceCodec[ResourceObject, A, encoding.ResourceObject] =
      semiauto.deriveResourceObjectCodecForCaseClass[A]()
  }

  case class B(id: String @@ IdTag, a: Nullable[A] @@ RelationshipTag)

  object B {
    implicit val codec: CaseClassResourceCodec[ResourceObject, B, encoding.ResourceObject] =
      semiauto.deriveResourceObjectCodecForCaseClass[B]()
  }

  case class C(id: String @@ IdTag, b: B @@ RelationshipTag)

  object C {
    implicit val codec: CaseClassResourceCodec[ResourceObject, C, encoding.ResourceObject] =
      semiauto.deriveResourceObjectCodecForCaseClass[C]()
  }

  case class BDef(id: String @@ IdTag, a: Nullable[A] @@ RelationshipTag = Null)

  object BDef {
    implicit val codec: CaseClassResourceCodec[ResourceObject, BDef, encoding.ResourceObject] =
      semiauto.deriveResourceObjectCodecForCaseClass[BDef]("B")
  }

  case class CDef(id: String @@ IdTag, b: BDef @@ RelationshipTag)

  object CDef {
    implicit val codec: CaseClassResourceCodec[ResourceObject, CDef, encoding.ResourceObject] =
      semiauto.deriveResourceObjectCodecForCaseClass[CDef]("C")
  }
  case class BOpt(id: String @@ IdTag, a: Option[Nullable[A]] @@ RelationshipTag = None)

  object BOpt {
    implicit val codec: CaseClassResourceCodec[ResourceObject, BOpt, encoding.ResourceObject] =
      semiauto.deriveResourceObjectCodecForCaseClass[BOpt]("B")
  }

  case class COpt(id: String @@ IdTag, b: BOpt @@ RelationshipTag)

  object COpt {
    implicit val codec: CaseClassResourceCodec[ResourceObject, COpt, encoding.ResourceObject] =
      semiauto.deriveResourceObjectCodecForCaseClass[COpt]("C")
  }
}
