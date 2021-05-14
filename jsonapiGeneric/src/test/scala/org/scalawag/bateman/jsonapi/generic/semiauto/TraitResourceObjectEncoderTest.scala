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

/*
package org.scalawag.bateman.jsonapi.generic.semiauto

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.encoding.{JAny, JNumber, JString}
import org.scalawag.bateman.json.generic.SourceTag
import org.scalawag.bateman.jsonapi.decoding
import org.scalawag.bateman.jsonapi.encoding.{Relationship, RelationshipData, ResourceIdentifier, ResourceObject}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, RelationshipTag}
import shapeless.tag.@@

class TraitResourceObjectEncoderTest extends AnyFunSpec with Matchers with ParserTestUtils {
  it("should generate a basic encoder") {
    final case class MyClass(
        id: String @@ IdTag,
        a: Int @@ AttributeTag,
        b: Option[ResourceIdentifier] @@ RelationshipTag
    )

    implicit val idEncoder = IdTag.encoder[String, JString]
    implicit val aEncoder = AttributeTag.encoder[Int, JAny]
    implicit val bEncoder = RelationshipTag.encoder[Option[ResourceIdentifier], Relationship]

    val enc = deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    enc.encode(MyClass("my_id", 17, Some(ResourceIdentifier("foo", "bar")))) shouldBe
      ResourceObject(
        "my_class",
        "my_id",
        Some(Map("a" -> JNumber(17))),
        Some(Map("b" -> Relationship(Some(RelationshipData.Singular(Some(ResourceIdentifier("foo", "bar"))))))),
        None,
        None
      )
  }

  it("should generate a basic encoder with source injection") {
    final case class MyClass(
        id: String @@ IdTag,
        a: Int @@ AttributeTag,
        b: Option[ResourceIdentifier] @@ RelationshipTag,
        src: decoding.ResourceObject @@ SourceTag
    )

    implicit val idEncoder = IdTag.encoder[String, JString]
    implicit val aEncoder = AttributeTag.encoder[Int, JAny]
    implicit val bEncoder = RelationshipTag.encoder[Option[ResourceIdentifier], Relationship]

    val enc = deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    enc.encode(MyClass("my_id", 17, Some(ResourceIdentifier("foo", "bar")), null)) shouldBe
      ResourceObject(
        "my_class",
        "my_id",
        Some(Map("a" -> JNumber(17))),
        Some(Map("b" -> Relationship(Some(RelationshipData.Singular(Some(ResourceIdentifier("foo", "bar"))))))),
        None,
        None
      )
  }

  it("should generate a basic encoder with optional source injection") {
    final case class MyClass(
        id: String @@ IdTag,
        a: Int @@ AttributeTag,
        b: Option[ResourceIdentifier] @@ RelationshipTag,
        src: Option[decoding.ResourceObject] @@ SourceTag
    )

    implicit val idEncoder = IdTag.encoder[String, JString]
    implicit val aEncoder = AttributeTag.encoder[Int, JAny]
    implicit val bEncoder = RelationshipTag.encoder[Option[ResourceIdentifier], Relationship]

    val enc = deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    enc.encode(MyClass("my_id", 17, Some(ResourceIdentifier("foo", "bar")), None)) shouldBe
      ResourceObject(
        "my_class",
        "my_id",
        Some(Map("a" -> JNumber(17))),
        Some(Map("b" -> Relationship(Some(RelationshipData.Singular(Some(ResourceIdentifier("foo", "bar"))))))),
        None,
        None
      )
  }

  it("should fail for a case class with no id") {
    assertTypeError("""
      final case class MyClass()
      deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with an untagged field") {
    assertTypeError("""
      final case class MyClass(id: String)
      deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with no id encoder") {
    assertTypeError("""
      sealed trait MyId
      final case class MyClass(id: MyId @@ IdTag)
      deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with an invalid src type") {
    assertTypeError("""
      final case class MyClass(id: String @@ IdTag, src: Int @@ SourceTag)
      deriveResourceObjectEncoderForCaseClass[MyClass]("my_class")
    """)
  }
}
*/
