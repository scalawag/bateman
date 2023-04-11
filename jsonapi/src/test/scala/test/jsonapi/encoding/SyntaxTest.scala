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

package test.jsonapi.encoding

import cats.syntax.either._
import org.scalamock.scalatest.MockFactory
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.syntax._
import org.scalawag.bateman.jsonapi.encoding.{EncodeResult, FieldsSpec, IncludeSpec, Inclusions, ResourceEncoder}
import test.json.BatemanTestBase
import test.jsonapi.encoding.SyntaxTest._

object SyntaxTest {
  // NOTE: the encoder for this class doesn't have anything to with the contents. I just needed a type.
  final case class MyClass()
  val instance = MyClass()

  // Choose an encoder based on how you want it to be encoded.

  val dataOnlyEncoder: ResourceEncoder[MyClass] = (_, _, _, _) =>
    ResourceEncoder
      .Encoded(
        JObject("id" -> "A".toJAny),
      )
      .asRight
}

class SyntaxTest extends BatemanTestBase with MockFactory {

  it("should encode a document") {
    new Fixture {
      mockEncodeResource
        .expects(instance, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
        .returns(
          ResourceEncoder
            .Encoded(
              JObject("id" -> "A".toJAny),
            )
            .asRight
        )
        .once()

      instance.toDocument shouldEncodeTo json"""
        {
          "data": {
            "id": "A"
          }
        }
      """
    }
  }

  it("should encode a document with includes") {
    new Fixture {
      mockEncodeResource
        .expects(instance, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
        .returns(
          ResourceEncoder
            .Encoded(
              JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
              Inclusions(
                JObject("type" -> "T".toJAny, "id" -> "B".toJAny),
                JObject("type" -> "T".toJAny, "id" -> "C".toJAny),
              )
            )
            .asRight
        )
        .once()

      instance.toDocument shouldEncodeTo json"""
        {
          "data": { "type": "T", "id": "A" },
          "included": [
            { "type": "T", "id": "B" },
            { "type": "T", "id": "C" }
          ]
        }
      """
    }
  }

  it("should pass infallible arguments through to encoder") {
    new Fixture {
      val includeSpec = IncludeSpec.Never
      val fieldsSpec = FieldsSpec.None

      mockEncodeResource
        .expects(instance, includeSpec, fieldsSpec, JObject.Empty)
        .returns(
          ResourceEncoder
            .Encoded(
              JObject("id" -> "A".toJAny),
            )
            .asRight
        )
        .once()

      instance.toDocument(includeSpec, fieldsSpec) shouldEncodeTo json"""
        {
          "data": {
            "id": "A"
          }
        }
      """
    }
  }

  it("should pass fallible arguments through to encoder") {
    new Fixture {
      val includeSpec = IncludeSpec.unsafe("a.b.c,d.e")
      val fieldsSpec =
        FieldsSpec(Map("a" -> FieldsSpec.Fields.None, "b" -> FieldsSpec.Fields.Explicit("x")), FieldsSpec.Fields.All)

      mockEncodeResource
        .expects(*, *, *, *)
        .returns(
          ResourceEncoder
            .Encoded(
              JObject("id" -> "A".toJAny),
            )
            .asRight
        )
        .once()

      instance.toDocument(includeSpec, fieldsSpec).map(_.render) shouldBe json"""
        {
          "data": {
            "id": "A"
          }
        }
      """.render.asRight
    }
  }

  trait Fixture {
    val mockEncodeResource =
      mockFunction[MyClass, IncludeSpec, FieldsSpec, JObject, EncodeResult[ResourceEncoder.Encoded]]("encodeResource")

    implicit val encoder: ResourceEncoder[MyClass] = mockEncodeResource(_, _, _, _)
  }
}
