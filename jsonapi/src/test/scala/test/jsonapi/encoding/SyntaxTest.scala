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

package test.jsonapi.encoding

import cats.syntax.either._
import org.scalamock.scalatest.MockFactory
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.syntax._
import org.scalawag.bateman.jsonapi.encoding.{EncodeResult, FieldsSpec, IncludeSpec, Inclusions, ResourceEncoder, ResourceObject}
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
        ResourceObject("", Some("A")),
      )
      .asRight
}

class SyntaxTest extends BatemanTestBase with MockFactory {

  describe("AnyBatemanJsonApiOps") {

    it("should encode a document") {
      new Fixture {
        mockEncodeResource
          .expects(instance, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("id" -> "A".toJAny),
                ResourceObject("", Some("A")),
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
                ResourceObject("T", Some("A")),
                Inclusions(
                  ResourceObject("T", Some("B")),
                  ResourceObject("T", Some("C")),
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
                ResourceObject("", Some("A")),
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
                ResourceObject("", Some("A")),
              )
              .asRight
          )
          .once()

        instance.toDocument(includeSpec, fieldsSpec).map(_.toJObject.render) shouldBe json"""
          {
            "data": {
              "id": "A"
            }
          }
        """.render.asRight
      }
    }
  }

  describe("SeqAnyBatemanJsonApiOps") {

    it("should encode an empty Seq to an empty data array") {
      new Fixture {
        // No mock expectation: encoder must never be called for an empty Seq.
        Seq.empty[MyClass].toDocument shouldEncodeTo json"""{"data":[]}"""
      }
    }

    it("should encode a Seq of one element") {
      new Fixture {
        mockEncodeResource
          .expects(instance, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
              )
              .asRight
          )
          .once()

        Seq(instance).toDocument shouldEncodeTo json"""
          {
            "data": [
              { "type": "T", "id": "A" }
            ]
          }
        """
      }
    }

    it("should encode a Seq of multiple elements") {
      new Fixture {
        val instance2 = MyClass()
        mockEncodeResource
          .expects(instance, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
              )
              .asRight
          )
          .once()
        mockEncodeResource
          .expects(instance2, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "B".toJAny),
                ResourceObject("T", Some("B")),
              )
              .asRight
          )
          .once()

        Seq(instance, instance2).toDocument shouldEncodeTo json"""
          {
            "data": [
              { "type": "T", "id": "A" },
              { "type": "T", "id": "B" }
            ]
          }
        """
      }
    }

    it("should merge inclusions across elements") {
      new Fixture {
        val instance2 = MyClass()
        mockEncodeResource
          .expects(instance, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
                Inclusions(ResourceObject("T", Some("X")))
              )
              .asRight
          )
          .once()
        mockEncodeResource
          .expects(instance2, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "B".toJAny),
                ResourceObject("T", Some("B")),
                Inclusions(ResourceObject("T", Some("Y")))
              )
              .asRight
          )
          .once()

        Seq(instance, instance2).toDocument shouldEncodeTo json"""
          {
            "data": [
              { "type": "T", "id": "A" },
              { "type": "T", "id": "B" }
            ],
            "included": [
              { "type": "T", "id": "X" },
              { "type": "T", "id": "Y" }
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
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
              )
              .asRight
          )
          .once()

        Seq(instance).toDocument(includeSpec, fieldsSpec) shouldEncodeTo json"""
          {
            "data": [
              { "type": "T", "id": "A" }
            ]
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
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
              )
              .asRight
          )
          .once()

        Seq(instance).toDocument(includeSpec, fieldsSpec).map(_.toJObject.render) shouldBe json"""
          {
            "data": [
              { "type": "T", "id": "A" }
            ]
          }
        """.render.asRight
      }
    }

    it("should propagate encoder errors from any element in fallible mode") {
      new Fixture {
        val err = org.scalawag.bateman.jsonapi.encoding.InvalidIncludePath("bogus")
        mockEncodeResource
          .expects(*, *, *, *)
          .returns(cats.data.NonEmptyChain.one(err).asLeft)
          .once()

        Seq(instance).toDocument(IncludeSpec.unsafe("bogus"), FieldsSpec.All).isLeft shouldBe true
      }
    }
  }

  describe("OptionBatemanJsonApiOps") {

    it("should encode None to a null data document") {
      new Fixture {
        // Encoder must not be invoked for None.
        (None: Option[MyClass]).toDocument shouldEncodeTo json"""{"data":null}"""
      }
    }

    it("should encode Some to a single-resource data document") {
      new Fixture {
        mockEncodeResource
          .expects(instance, IncludeSpec.Opportunistically, FieldsSpec.All, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
              )
              .asRight
          )
          .once()

        Some(instance).toDocument shouldEncodeTo json"""
          {
            "data": { "type": "T", "id": "A" }
          }
        """
      }
    }

    it("should pass infallible arguments through to encoder for Some") {
      new Fixture {
        val includeSpec = IncludeSpec.Never
        val fieldsSpec = FieldsSpec.None

        mockEncodeResource
          .expects(instance, includeSpec, fieldsSpec, JObject.Empty)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
              )
              .asRight
          )
          .once()

        Some(instance).toDocument(includeSpec, fieldsSpec) shouldEncodeTo json"""
          {
            "data": { "type": "T", "id": "A" }
          }
        """
      }
    }

    it("should produce a null data document in fallible mode for None") {
      new Fixture {
        val includeSpec = IncludeSpec.unsafe("a.b")
        val fieldsSpec = FieldsSpec(Map.empty, FieldsSpec.Fields.All)

        // Encoder must not be invoked for None.
        (None: Option[MyClass]).toDocument(includeSpec, fieldsSpec).map(_.toJObject.render) shouldBe
          json"""{"data":null}""".render.asRight
      }
    }

    it("should pass fallible arguments through to encoder for Some") {
      new Fixture {
        val includeSpec = IncludeSpec.unsafe("a.b.c,d.e")
        val fieldsSpec =
          FieldsSpec(Map("a" -> FieldsSpec.Fields.None, "b" -> FieldsSpec.Fields.Explicit("x")), FieldsSpec.Fields.All)

        mockEncodeResource
          .expects(*, *, *, *)
          .returns(
            ResourceEncoder
              .Encoded(
                JObject("type" -> "T".toJAny, "id" -> "A".toJAny),
                ResourceObject("T", Some("A")),
              )
              .asRight
          )
          .once()

        Some(instance).toDocument(includeSpec, fieldsSpec).map(_.toJObject.render) shouldBe json"""
          {
            "data": { "type": "T", "id": "A" }
          }
        """.render.asRight
      }
    }
  }

  trait Fixture {
    val mockEncodeResource =
      mockFunction[MyClass, IncludeSpec, FieldsSpec, JObject, EncodeResult[ResourceEncoder.Encoded]]("encodeResource")

    implicit val encoder: ResourceEncoder[MyClass] = mockEncodeResource(_, _, _, _)
  }
}
