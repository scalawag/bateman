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

package org.scalawag.bateman.jsonapi.generic

import cats.data.NonEmptyChain
import cats.syntax.validated._
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, SnakeCase}
import org.scalawag.bateman.json.decoding.{
  ContextualDecoder,
  DecodeResult,
  JAny,
  JAnyDecoder,
  JObject,
  JString,
  JsonTypeMismatch
}
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.jsonapi.query._
import org.scalawag.bateman.jsonapi.Model.{Blue, Color, RGB, Red}
import org.scalawag.bateman.jsonapi.decoding.{
  Document,
  JsonApiTypeMismatch,
  ResourceIdentifier,
  ResourceIdentifierDecoder,
  ResourceObject,
  ResourceObjectDecoder,
  ResourceObjectLike,
  ResourceObjectOptionalId,
  ResourceObjectOptionalIdDecoder
}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.DecodeError.formatErrorReport
import org.scalawag.bateman.json.generic.{Config, SourceTag}
import org.scalawag.bateman.json.validating.EmptyTraitValidatedCompanion
import org.scalawag.bateman.jsonapi.generic.decoding.{JSource, ResourceDecoder}
import shapeless.tag.@@

import java.util.UUID

class DerivedResourceObjectDecoderTest extends AnyFunSpec with Matchers with ParserTestUtils {
  describe("tagged") {

    final case class Square(
        id: UUID @@ IdTag,
        label: Option[String] @@ AttributeTag = None,
        color: List[Color] @@ AttributeTag,
        sideLength: Int @@ AttributeTag,
        parent: Option[ResourceIdentifier] @@ RelationshipTag = None
    )

    object Square {
      implicit val cfg: Config = Config(fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
      implicit def decoder: ResourceObjectDecoder[Square] =
        semiauto.deriveResourceObjectDecoderForCaseClass[Square]("square")
    }

    val q1 = root[JAny, Any] ~> as[Document]
//    val q2 = root[JAny, Any].~>(as[Document])(ContextualDecoder.narrowObject(Document.decoder[Any]))

    it("should decode") {
      val square =
        Square(
          UUID.randomUUID,
          Some("blue square"),
          List(Red, Blue, RGB(3, 4, 3)),
          17,
          None
        )

      square.copy(sideLength = 11)

      val doc = parseAs[Document]("""
        {
          "data" : {
            "type" : "Square",
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
      val res = decoded.fold(formatErrorReport, identity)
      println(res)
    }
  }

  describe("source injection") {

    final case class MyClass(
        id: UUID @@ IdTag,
        label: Option[String] @@ AttributeTag = None,
        src: Option[JSource] @@ SourceTag = None
    )

    object MyClass {
      implicit val cfg: Config = Config(fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
      implicit def decoder: ResourceObjectDecoder[MyClass] =
        semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should decode") {
      val in = parseAs[ResourceObject]("""
        {
          "type" : "my_class",
          "id" : "55555555-5555-5555-5555-555555555555",
          "attributes" : {
            "label": "mike"
          }
        }
      """)

      val decoded = ResourceObjectDecoder[MyClass].decode(in, null)
      val res = decoded.fold(ee => fail(formatErrorReport(ee)), identity)
      println(res)

      val encoded = semiauto.deriveResourceObjectEncoderForCaseClass[MyClass]("my_class").encode(res)
      println(encoded)
    }
  }

  describe("tagged preciser (custom) relationship targets") {
    import shapeless.tag.@@

    final case class SquareId(
        id: UUID @@ IdTag
    )

    object SquareId {
//      implicit val tagUUidDec = IdTag.decoder[JAny, UUID]

      implicit val decoder: ResourceIdentifierDecoder[SquareId] =
        semiauto.deriveResourceIdentifierDecoderForCaseClass[SquareId]("square")
    }

    final case class Square(
        id: UUID @@ IdTag,
        label: Option[String] @@ AttributeTag = None,
        color: List[Color] @@ AttributeTag,
        sideLength: Int @@ AttributeTag,
        parent: Option[SquareId] @@ RelationshipTag
    )

    object Square {
      implicit val cfg: Config = Config(fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
      implicit def decoder: ResourceObjectDecoder[Square] =
        semiauto.deriveResourceObjectDecoderForCaseClass[Square]("square")
    }

    it("custom relation") {
      val square =
        Square(
          UUID.randomUUID,
          Some("blue square"),
          List(Red, Blue, RGB(3, 4, 3)),
          17,
          None
        )

      square.copy(sideLength = 11)

      val in = parse("""
        {
          "data" : {
            "type" : "Square",
            "id" : "55555555-5555-5555-5555-555555555555",
            "attributes" : {
              "label": "mike",
              "color" : ["red", "blue", { "r": 1, "g": 0, "b": 1 }],
              "side_length" : 12
            },
            "relationships": {
              "parent": {
                "data": {
                  "type": "Square",
                  "id": "44444444-4444-4444-4444-444444444444"
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
      """).query(_ ~> as[Document] ~> data ~> required ~> as[ResourceObject]).fold(ee => fail(ee.toString), identity)

      val dec = ResourceObjectDecoder[Square].decode(in, null)
      val res = dec.fold(formatErrorReport, identity)
      println(res)
    }
  }

  it("should derive decoders for traits") {
    sealed trait X
    final case class Y(b: Int @@ AttributeTag) extends X
    final case class Z(b: String @@ AttributeTag) extends X

    object Y {
      implicit val decoder = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[Y]("y")
    }

    object Z {
      implicit val decoder = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[Z]("z")
    }

    object X {
      implicit val decoder: ResourceObjectOptionalIdDecoder[X] =
        semiauto.deriveResourceObjectOptionalIdDecoderForTrait[X]()
    }
    val in = parseAs[ResourceObjectOptionalId]("""{"type": "foo", "b": 8}""")

    ResourceObjectOptionalIdDecoder[X].decode(in, null) shouldBe JsonApiTypeMismatch(
      in,
      NonEmptyChain("y", "z")
    ).invalidNec

    val in2 = parseAs[ResourceObjectOptionalId]("""{"type": "z", "attributes": {"b": 8}}""")

    ResourceObjectOptionalIdDecoder[X].decode(in2, null) shouldBe JsonTypeMismatch(
      in2.requiredAttribute("b").getOrElse(???),
      JString
    ).invalidNec

    val in3 = parseAs[ResourceObjectOptionalId]("""{"type": "y", "attributes": {"b": 8}}""")

    ResourceObjectOptionalIdDecoder[X].decode(in3, null) shouldBe Y(8).validNec
  }

  describe("relationships") {
    final case class MyRelative(id: UUID @@ IdTag, name: String @@ AttributeTag)

//    implicit val tagIdDec = IdTag.decoder[JString, Int]
//    implicit val tagStringDec = AttributeTag.decoder[JAny, String]

    object MyRelative {
//      implicit val isDec: Decoder[JString, Int] = Decoder.jstringToJNumber
      implicit val decoder = semiauto.deriveResourceObjectDecoderForCaseClass[MyRelative]("my_relative")
    }

    final case class MyRelativeId(id: UUID @@ IdTag)
    object MyRelativeId {
      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyRelativeId]("my_relative")
    }

//    implicit def full[A](implicit
//        dec: ResourceDecoder[ResourceObject, A]
//    ): ResourceDecoder[ResourceIdentifier, A] = {
//      case (in, Some(doc)) => doc.requiredIncluded(in).andThen(_.as[A])
//      case (in, None)      => ??? // TODO: handle gracefully with a missing inclusion error
//    }

    it("should handle a singular raw relation") {
      final case class MyClass(id: UUID @@ IdTag, relative: ResourceIdentifier @@ RelationshipTag)
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle an optional raw relation") {
      final case class MyClass(id: UUID @@ IdTag, relative: Option[ResourceIdentifier] @@ RelationshipTag)
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle multiple raw relations") {
      final case class MyClass(id: UUID @@ IdTag, relative: List[ResourceIdentifier] @@ RelationshipTag)
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle a singular custom id relation") {
      final case class MyClass(id: UUID @@ IdTag, relative: MyRelativeId @@ RelationshipTag)
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle an optional custom id relation") {
      final case class MyClass(id: UUID @@ IdTag, relative: Option[MyRelativeId] @@ RelationshipTag)
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle multiple custom id relations") {
      final case class MyClass(id: UUID @@ IdTag, relative: List[MyRelativeId] @@ RelationshipTag)
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle a singular custom object relation") {
      final case class MyClass(id: UUID @@ IdTag, relative: MyRelative @@ RelationshipTag)
//      implicit val f = full(MyRelative.decoder)
//      ResourceIdentifierLike.fromIdentifierDecoder[MyRelative]
//      ResourceIdentifierLike.fromObjectDecoder[MyRelative]
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle an optional custom object relation") {
      final case class MyClass(id: UUID @@ IdTag, relative: Option[MyRelative] @@ RelationshipTag)
      val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should handle multiple custom object relations") {
      val mainId = UUID.randomUUID
      val fooId = UUID.randomUUID
      val barId = UUID.randomUUID

      val json = parseAs[Document](s"""
        {
          "data": {
            "type": "my_class",
            "id": "$mainId",
            "relationships": {
              "relatives": {
                "data": [
                  {
                    "type": "my_relative",
                    "id": "$fooId"
                  },
                  {
                    "type": "my_relative",
                    "id": "$barId"
                  }
                ]
              }
            }
          },
          "included": [
            {
              "type": "my_relative",
              "id": "$fooId",
              "attributes": {
                "name": "foo"
              }
            },
            {
              "type": "my_relative",
              "id": "$barId",
              "attributes": {
                "name": "bar"
              }
            }
          ]
        }
      """)

      final case class MyClass(id: UUID @@ IdTag, relatives: List[MyRelative] @@ RelationshipTag)

//      implicit val tagRelDec = RelationshipTag.decoder[ResourceIdentifier, MyRelative]

      implicit val dec = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")

      json.requiredData.andThen(_.required)

      val q = json.cquery(json)(_ ~> data ~> required)

      val g = json.cquery(json)(_ ~> data ~> required ~> as[ResourceObject] ~> as[MyClass])
//      println(g.fold(formatErrorReport, identity))

      g.shouldSucceed shouldBe MyClass(mainId, List(MyRelative(fooId, "foo"), MyRelative(barId, "bar")))
//      val f = json.requiredData.andThen(_.singular).andThen(_.as[ResourceObject]).andThen(_.as[MyClass])
    }
  }

  describe("compile errors") {
    it("should list a missing resource identifier decoder") {
      trait NoDecoderId

      final case class Square(
          id: UUID @@ IdTag,
          label: Option[String] @@ AttributeTag = None,
          color: List[Color] @@ AttributeTag,
          sideLength: Int @@ AttributeTag,
          parent: Option[NoDecoderId] @@ RelationshipTag = None
      )

      object Square {
//        implicit def decoder: ResourceObjectDecoder[Square] =
//          semiauto.deriveResourceObjectDecoderForCaseClass[Square]("square")
      }
    }
  }

  describe("optional ID") {
    final case class MyClass(id: Option[Int] @@ IdTag = None)
    implicit val intIdDecoder = ContextualDecoder.jstringToJNumber(ContextualDecoder.jnumberToIntDecoder)
    implicit val decoder = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyClass]("my_class")

    it("should handle Some id") {
      val json = parseAs[ResourceObjectOptionalId]("""
        {
          "type": "my_class",
          "id": "8"
        }
      """)

      decoder.decode(json, null).shouldSucceed shouldBe MyClass(Some(8))
    }

    it("should handle None id") {
      val json = parseAs[ResourceObjectOptionalId]("""
        {
          "type": "my_class"
        }
      """)

      decoder.decode(json, null).shouldSucceed shouldBe MyClass(None)
    }

    it("should fail on a null ID") {
      val json = parse("""
        {
          "type": "my_class",
          "id": null
        }
      """)

      JAnyDecoder[ResourceObjectOptionalId].decode(json) shouldBe JsonTypeMismatch(
        json.asObject.andThen(_.fields).map(_("id").value).getOrElse(???),
        JString
      ).invalidNec
    }
  }

  describe("duplicate field types with traits") {
    trait MyTrait
    object MyTrait extends EmptyTraitValidatedCompanion[String, MyTrait, Any] {
      override def validate(in: String): List[String] = Nil
    }

    final case class MyClassFoo(
        id: String @@ IdTag,
        a: String with MyTrait @@ AttributeTag,
        b: String with MyTrait @@ AttributeTag
    )
    implicit val decoder = semiauto.deriveResourceObjectDecoderForCaseClass[MyClassFoo]("my_class")

    it("should double up prechecks") {
      ResourceObjectDecoder[MyClassFoo]
        .decode(
          parseAs[ResourceObject]("""
            {
              "type": "my_class",
              "id": "ID",
              "attributes": {
                "a": "A",
                "b": "B"
              }
            }
          """),
          null
        )
        .shouldSucceed shouldBe MyClassFoo("ID", MyTrait.unsafe("A"), MyTrait.unsafe("B"))
    }
  }

  describe("decode metadata") {
    final case class MyClass(
        id: String @@ IdTag,
        a: String @@ AttributeTag,
        b: String @@ MetaTag,
        c: Int @@ MetaTag
    )

    object MyClass {
      implicit val decoder = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
    }

    it("should decode b and c") {
      ResourceObjectDecoder[MyClass]
        .decode(
          parseAs[ResourceObject]("""
            {
              "type": "my_class",
              "id": "ID",
              "attributes": {
                "a": "A"
              },
              "meta": {
                "b": "B",
                "c": 31
              }
            }
          """),
          null
        )
        .shouldSucceed shouldBe MyClass("ID", "A", "B", 31)

    }
  }
}
