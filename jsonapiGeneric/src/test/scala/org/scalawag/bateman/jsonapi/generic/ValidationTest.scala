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
package org.scalawag.bateman.jsonapi.generic

import cats.data.ValidatedNec
import cats.syntax.apply._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.Decoder
import org.scalawag.bateman.json.encoding.Encoder
import org.scalawag.bateman.json.generic.{TaggedValidation, TaggedValidator}
import org.scalawag.bateman.json.validating.{ValidationFailure, ValidationResult}
import org.scalawag.bateman.json.{ParserTestUtils, decoding, encoding}
import org.scalawag.bateman.jsonapi.decoding.{Attributes, ResourceObjectDecoder, ResourceObjectOptionalId}
import org.scalawag.bateman.jsonapi.encoding.ResourceObjectEncoder
import org.scalawag.bateman.jsonapi.generic.ValidationTest.MyClass.AttributeTags.Name
import shapeless.tag.@@
import shapeless.{Lazy, tag}

import java.util.UUID

object ValidationTest {

  final case class MyClass private (
      id: UUID @@ IdTag,
      name: String @@ MyClass.AttributeTags.Name,
      last: String @@ AttributeTag
  )

  object MyClass {
    object AttributeTags {
      trait Name extends AttributeTag
      object Name extends TaggedValidation[String, Name] {
        override def validate(in: String): List[String] =
          if (in.length < 1)
            List("must not be empty")
          else if (in.length > 20)
            List(s"length must be twenty characters or fewer: '$in'")
          else
            Nil
      }
    }

    def apply(id: UUID, name: String, last: String): ValidatedNec[ValidationFailure, MyClass] =
      (
        TaggedValidator[IdTag].validate(id).leftMap(_.map(_.descend("id"))): ValidationResult[UUID @@ IdTag],
        TaggedValidator[Name].validate(name).leftMap(_.map(_.descend("name"))): ValidationResult[String @@ Name],
        TaggedValidator[AttributeTag]
          .validate(last)
          .leftMap(_.map(_.descend("last"))): ValidationResult[String @@ AttributeTag]
      ).mapN { case (u, n, l) => new MyClass(u, n, l) }

    def unsafe(id: UUID, name: String, last: String): MyClass =
      apply(id, name, last).valueOr(ValidationFailure.throwValidationErrors)

    // This should not be necessary. It should be derived automatically.
    implicit val nameAttributeDecoder =
      AttributeTag.decoder[decoding.JAny, String](Lazy(Decoder.stringDecoder))
    implicit val nameAttributeEncoder =
      AttributeTag.encoder[String, encoding.JAny](Lazy(Encoder.stringEncoder))

    implicit def decoder = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyClass]("my_class")
    implicit def encoder = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyClass]("my_class")
  }
}

class ValidationTest extends AnyFunSpec with Matchers with ParserTestUtils {
  import ValidationTest._
  it("should") {
//    val a = MyClass.unsafe(UUID.randomUUID, "Justin", "Patterson")
//    val e = ResourceEncoder[MyClass, ResourceObject].encode(a)
//    println(e)
//    val j = Encoder[ResourceObject, encoding.JAny].encode(e)
//    println(j.render)

    import org.scalawag.bateman.jsonapi.decoding._
    println(
      ResourceObjectOptionalIdDecoder[MyClass]
        .decode(parseAs[ResourceObjectOptionalId]("""
          {
            "type": "blah",
            "attributes": {
              "name": "98549837498347698374987349687349687349687",
              "last": "dddddddddddddddddddddddddddddddddddddddddd"
            }
          }
        """))
    )

    println(
      ResourceObjectOptionalIdDecoder[MyClass]
        .decode(parseAs[ResourceObjectOptionalId]("""
          {
            "type": "blah",
            "attributes": {
              "name": "Robert",
              "last": "Paulson"
            }
          }
        """))
    )

    println(
      MyClass(UUID.randomUUID, "", "abcdefghijklmnopqrstuvwxyz")
    )

    println(
      MyClass(UUID.randomUUID, tag[Name](""), "abcdefghijklmnopqrstuvwxyz")
    )

    println(
      MyClass.unsafe(UUID.randomUUID, "", "abcdefghijklmnopqrstuvwxyz")
    )
  }

}
*/
