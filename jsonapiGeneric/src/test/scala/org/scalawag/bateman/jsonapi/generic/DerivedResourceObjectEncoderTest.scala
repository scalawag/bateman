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

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.encoding.{Encoder, JAny, JObject, PrettySpaces2}
import org.scalawag.bateman.json.generic.SourceTag
import org.scalawag.bateman.jsonapi.Model.{Blue, Color, RGB, Red}
import org.scalawag.bateman.jsonapi.TestResultUtils

import scala.collection.compat.immutable.LazyList
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.PartiallyEncoded
import org.scalawag.bateman.jsonapi.encoding.{
  DeferredEncoding,
  FieldsSpec,
  IncludeSpec,
  Inclusions,
  InvalidIncludePath,
  Relationship,
  RelationshipData,
  ResourceEncoder,
  ResourceIdentifier,
  ResourceIdentifierEncoder,
  ResourceObject,
  ResourceObjectEncoder,
  ResourceObjectOptionalId,
  ResourceObjectOptionalIdEncoder
}
import org.scalawag.bateman.jsonapi.generic.DerivedResourceObjectEncoderTest.{A, C, D, DR}
import org.scalawag.bateman.jsonapi.generic.decoding.JSource
import shapeless.tag.@@

import java.util.UUID

class DerivedResourceObjectEncoderTest extends AnyFunSpec with Matchers with TestResultUtils with ParserTestUtils {
  describe("tagged") {

    final case class SquareId(
        id: UUID @@ IdTag
    )

    object SquareId {
      implicit def encoder: ResourceIdentifierEncoder[SquareId] =
        semiauto.deriveResourceIdentifierEncoderForCaseClass[SquareId]("square")
    }

    final case class Square(
        id: UUID @@ IdTag,
        label: Option[String] @@ AttributeTag = None,
        color: List[Color] @@ AttributeTag,
        sideLength: Int @@ AttributeTag,
        parent: ResourceIdentifier @@ RelationshipTag
    )

    object Square {
      implicit def encoder: ResourceEncoder[Square, ResourceObject] =
        semiauto.deriveResourceObjectEncoderForCaseClass[Square]("square")
    }

    it("should encode") {
      val square =
        Square(
          UUID.randomUUID,
          Some("blue square"),
          List(Red, Blue, RGB(3, 4, 3)),
          17,
          ResourceIdentifier("jazz", "134")
        )

      val enc =
        ResourceEncoder[Square, ResourceObject]
          .encodeResource(square, fieldsSpec = FieldsSpec(Map("square" -> Set("label", "parent"))))
          .getOrElse(fail)
      println(enc)
      val json = Encoder[ResourceObject, JObject].encode(enc.root)
      println(json.render(PrettySpaces2))

      val expected = org.scalawag.bateman.json.decoding.parser.toJAny(s"""
        {
          "type": "square",
          "id": "${square.id}",
          "attributes": {
            "label": "blue square"
          },
          "relationships": {
            "parent": {
              "data": {
                "type": "jazz",
                "id": "134"
              }
            }
          }
        }
      """.to(LazyList)).fold(e => fail(e.toString), identity).toEncoding

      import org.scalawag.bateman.json.syntax._
      json.toJAny shouldMatch expected
    }
  }

  it("should derive decoders for traits") {
    sealed trait X
    final case class Y(b: Int @@ AttributeTag) extends X
    final case class Z(b: String @@ AttributeTag) extends X

    implicit val tagIntEnc = AttributeTag.encoder[Int, JAny]
    implicit val tagStringEnc = AttributeTag.encoder[String, JAny]

    object Y {
      implicit val encoder = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[Y]("y")
    }

    object Z {
      implicit val encoder = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[Z]("z")
    }

    object X {
      implicit val encoder = semiauto.deriveResourceObjectOptionalIdEncoderForTrait[X]()
    }

    val y = Y(17)
    val z = Z("foo")

    ResourceObjectOptionalIdEncoder[X]
      .encodeResource(y)
      .map(e => println(Encoder[ResourceObjectOptionalId, JAny].encode(e.root).render))
    ResourceObjectOptionalIdEncoder[X]
      .encodeResource(z)
      .map(e => println(Encoder[ResourceObjectOptionalId, JAny].encode(e.root).render))
    ResourceObjectOptionalIdEncoder[Y]
      .encodeResource(y)
      .map(e => println(Encoder[ResourceObjectOptionalId, JAny].encode(e.root).render))
    ResourceObjectOptionalIdEncoder[Z]
      .encodeResource(z)
      .map(e => println(Encoder[ResourceObjectOptionalId, JAny].encode(e.root).render))
  }

  describe("include paths") {
    import DerivedResourceObjectEncoderTest._

    // Uses the encoder just to get the ResourceObject
    def aro(a: A) = ResourceObjectEncoder[A].encodeResource(a).map(_.root).getOrElse(fail)
    def bro(b: B) = ResourceObjectEncoder[B].encodeResource(b).map(_.root).getOrElse(fail)

    it("should not include the A") {
      val b = B("foo", A("bar", 17))
      val encoded = ResourceObjectEncoder.encodeResource(b)
      encoded shouldBe PartiallyEncoded(bro(b)).validNec
    }

    it("should include the A") {
      val b = B("foo", A("bar", 17))
      val encoded = ResourceObjectEncoder.encodeResource(b, IncludeSpec.unsafe("a"))
      encoded shouldBe PartiallyEncoded(bro(b), Inclusions(aro(b.a))).validNec
    }

    it("should include a deferral for a.b") {
      val b = B("foo", A("bar", 17, Some(ResourceIdentifier("x", "y"))))
      val encoded = ResourceObjectEncoder.encodeResource(b, IncludeSpec.unsafe("a.b"))
      encoded shouldBe PartiallyEncoded(
        bro(b),
        Inclusions(aro(b.a)),
        Set(DeferredEncoding(b.a.b.get, IncludeSpec.Always("a.b", Map.empty), FieldsSpec.All))
      ).validNec
    }

    it("should not include a deferral for (absent) a.b") {
      val b = B("foo", A("bar", 17))
      val encoded = ResourceObjectEncoder.encodeResource(b, IncludeSpec.unsafe("a.b"))
      encoded shouldBe PartiallyEncoded(bro(b), Inclusions(aro(b.a))).validNec
    }

    it("should include opportunistically") {
      val b = B("foo", A("bar", 17))
      val encoded = ResourceObjectEncoder.encodeResource(b, IncludeSpec.Opportunistically)
      encoded shouldBe PartiallyEncoded(bro(b), Inclusions(aro(b.a))).validNec
    }

    it("should fail for invalid paths") {
      val b = B("foo", A("bar", 17))
      val encoded = ResourceObjectEncoder.encodeResource(b, IncludeSpec.unsafe("a.c"))
      encoded shouldBe InvalidIncludePath("a.c").invalidNec
    }

    it("should fail at first invalid path relationship") {
      val b = B("foo", A("bar", 17))
      val encoded = ResourceObjectEncoder.encodeResource(b, IncludeSpec.unsafe("a.c.d.c"))
      encoded shouldBe InvalidIncludePath("a.c").invalidNec
    }

//      val e5 =
//        ResourceObjectEncoder.encode(
//          B("foo", A("bar", 17)),
//          IncludeSpec.unsafe("a.c.d.e"),
//          FieldsSpec(Map("b" -> Set("a", "j", "k")))
//        )
//      println(e5)
  }

  describe("source tags") {
    it("should ignore the source-tagged fields") {
      ResourceObjectEncoder.encodeResource(C("ID"))(C.encoder).asValid shouldBe PartiallyEncoded(
        ResourceObject("c", "ID")
      )
    }
  }

  describe("relatives by ID") {
    it("should encode a required relative's ID only") {
      ResourceObjectEncoder.encodeResource(D("did", DR("drid"))).asValid shouldBe PartiallyEncoded(
        ResourceObject(
          "d",
          "did",
          relationships = Some(
            Map("rel" -> Relationship(Some(RelationshipData.fromResourceIdentifier(ResourceIdentifier("dr", "drid")))))
          )
        )
      )
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
      implicit val encoder = semiauto.deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    }

    it("should encode b and c") {
      import org.scalawag.bateman.json.syntax._
      ResourceObjectEncoder.encode(MyClass("ID", "A", "B", 31)) shouldBe
        ResourceObject(
          "my_class",
          "ID",
          Some(
            Map(
              "a" -> "A".toJAny
            )
          ),
          None,
          Some(
            Map(
              "b" -> "B".toJAny,
              "c" -> 31.toJAny
            )
          )
        )
    }
  }

}

object DerivedResourceObjectEncoderTest {
  final case class A(
      id: String @@ IdTag,
      a: Int @@ AttributeTag,
      b: Option[ResourceIdentifier] @@ RelationshipTag = None
  )

  object A {
    implicit val encoder = semiauto.deriveResourceObjectEncoderForCaseClass[A]("a")
  }

  final case class B(id: String @@ IdTag, a: A @@ RelationshipTag)

  object B {
    implicit val encoder = semiauto.deriveResourceObjectEncoderForCaseClass[B]("b")
  }

  final case class C(id: String @@ IdTag, src: Option[JSource] @@ SourceTag = None)

  object C {
    implicit val encoder = semiauto.deriveResourceObjectEncoderForCaseClass[C]("c")
  }

  final case class DR(id: String @@ IdTag)

  object DR {
    implicit val encoder = semiauto.deriveResourceIdentifierEncoderForCaseClass[DR]("dr")
  }

  final case class D(id: String @@ IdTag, rel: DR @@ RelationshipTag)

  object D {
    implicit val encoder = semiauto.deriveResourceObjectEncoderForCaseClass[D]("d")
  }
}
