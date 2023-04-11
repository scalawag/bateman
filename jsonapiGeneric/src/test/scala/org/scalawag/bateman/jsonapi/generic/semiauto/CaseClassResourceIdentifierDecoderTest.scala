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

/*
package org.scalawag.bateman.jsonapi.generic.semiauto

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.{JResult, Decoder, JObject, JString}
import org.scalawag.bateman.json.generic.SourceTag
import org.scalawag.bateman.jsonapi.decoding.{ResourceIdentifier, ResourceLike}
import org.scalawag.bateman.jsonapi.generic.{IdTag, RelationshipTag}
import shapeless.tag.@@

class CaseClassResourceIdentifierDecoderTest extends AnyFunSpec with Matchers with ParserTestUtils {
  it("should generate a basic decoder") {
    final case class MyClass(@Id id: String)

    implicit val idDecoder = IdTag.decoder[JString, String]

    val dec = deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    dec.decode(parseAs[ResourceIdentifier]("""
      {
        "type": "my_class",
        "id": "my_id"
      }
    """)) shouldBe MyClass("my_id").rightNec
  }

  it("should generate a basic decoder with source injection") {
    final case class MyClass(@Id id: String, @Source src: ResourceLike)

    implicit val idDecoder = IdTag.decoder[JString, String]

    val dec = deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    val ri = parseAs[ResourceIdentifier]("""
      {
        "type": "my_class",
        "id": "my_id"
      }
    """)

    dec.decode(ri) shouldBe MyClass("my_id", ri).rightNec
  }

  it("should generate a basic decoder with optional source injection") {
    final case class MyClass(@Id id: String, @Source src: Option[ResourceLike])

    implicit val idDecoder = IdTag.decoder[JString, String]

    val dec = deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    val ri = parseAs[ResourceIdentifier]("""
      {
        "type": "my_class",
        "id": "my_id"
      }
    """)

    dec.decode(ri) shouldBe MyClass("my_id", Some(ri)).rightNec
  }

  it("should fail for a case class with no id") {
    assertTypeError("""
      final case class MyClass()
      deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with an untagged id") {
    assertTypeError("""
      final case class MyClass(id: String)
      deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with no id encoder") {
    assertTypeError("""
      sealed trait MyId
      final case class MyClass(@Id id: MyId)
      deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with an attribute") {
    assertTypeError("""
      final case class MyClass(@Id id: String, @Attribute a: Int)
      deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with an relationship") {
    assertTypeError("""
      final case class MyClass(@Id id: String, @Relationship r: Int)
      deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    """)
  }

  it("should fail for a case class with an invalid src type") {
    assertTypeError("""
      final case class MyClass(@Id id: String, @Source src: Int)
      deriveResourceIdentifierDecoderForCaseClass[MyClass]("my_class")
    """)
  }
}
*/
