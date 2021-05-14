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

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.{TestResultUtils, decoding}
import org.scalawag.bateman.json.encoding.{Encoder, JAny, JObject, PrettySpaces2}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, RelationshipTag, semiauto}
import shapeless.tag.@@

class GraphEncoderTest extends AnyFunSpec with Matchers with ParserTestUtils with TestResultUtils {
  describe("blah") {
    final case class X(id: String @@ IdTag, children: List[Y] @@ RelationshipTag = Nil)
    final case class Y(id: String @@ IdTag, name: Option[String] @@ AttributeTag = None)
    implicit val yencoder = semiauto.deriveResourceEncoderForCaseClass[Y, ResourceObject]("y")
    implicit val xencoder = semiauto.deriveResourceEncoderForCaseClass[X, ResourceObject]("x")

    it("should encode a bunch of things with includeds") {
      val d = Y("d", Some("foo"))
      val c = Y("c")
      val b = X("b", List(c, d))
      val a = X("a", List(c))

      val e = GraphEncoder.encodeObjects(List(a, b), IncludeSpec.unsafe("children"), FieldsSpec.All)

      val expected = parseAs[decoding.Document]("""
        {
          "data": [
            {
              "type": "x",
              "id": "a",
              "relationships": {
                "children": {
                  "data": [
                    {
                      "type": "y",
                      "id": "c"
                    }
                  ]
                }
              }
            },
            {
              "type": "x",
              "id": "b",
              "relationships": {
                "children": {
                  "data": [
                    {
                      "type": "y",
                      "id": "c"
                    },
                    {
                      "type": "y",
                      "id": "d"
                    }
                  ]
                }
              }
            }
          ],
          "included": [
            {
              "type": "y",
              "id": "d",
              "attributes": {
                "name": "foo"
              }
            }
          ]
        }
      """)

      println(e.asValid.to[JAny].spaces2)
      e.asValid shouldMatch expected
    }

    it("should not include referents with no useful fields") {
      val d = Y("d")
      val c = Y("c")
      val b = X("b", List(c, d))
      val a = X("a", List(c))

      val e = GraphEncoder.encodeObjects(List(a, b), IncludeSpec.unsafe("children"), FieldsSpec.All)

      val expected = parseAs[decoding.Document]("""
        {
          "data": [
            {
              "type": "x",
              "id": "a",
              "relationships": {
                "children": {
                  "data": [
                    {
                      "type": "y",
                      "id": "c"
                    }
                  ]
                }
              }
            },
            {
              "type": "x",
              "id": "b",
              "relationships": {
                "children": {
                  "data": [
                    {
                      "type": "y",
                      "id": "c"
                    },
                    {
                      "type": "y",
                      "id": "d"
                    }
                  ]
                }
              }
            }
          ]
        }
      """)

      println(e.asValid.to[JAny].spaces2)
      e.asValid shouldMatch expected
    }
  }

  describe("recursive") {
    final case class X(id: String @@ IdTag, children: List[X] @@ RelationshipTag = Nil)
    // You have to do all this to make a `val` that is recursive
    val rec: ResourceObjectEncoder[X] = {
      implicit def e: ResourceObjectEncoder[X] = semiauto.deriveResourceEncoderForCaseClass[X, ResourceObject]("x")
      e
    }
    implicit val xencoder = rec

    val g = X("g")
    val f = X("f", List(g))
    val e = X("e", List(g))
    val d = X("d", List(e))
    val c = X("c", List(d, e))
    val b = X("b", List(f, e))
    val a = X("a", List(b, c, g))

    it("should include two deep") {

      val expected = parseAs[decoding.Document]("""
        {
          "data": {
            "type": "x",
            "id": "a",
            "relationships": {
              "children": {
                "data": [
                  { "type": "x", "id": "b" },
                  { "type": "x", "id": "c" },
                  { "type": "x", "id": "g" }
                ]
              }
            }
          },
          "included": [
            {
              "type": "x",
              "id": "b",
              "relationships": {
                "children": {
                  "data": [
                    { "type": "x", "id": "f" },
                    { "type": "x", "id": "e" }
                  ]
                }
              }
            },
            {
              "type": "x",
              "id": "f",
              "relationships": {
                "children": {
                  "data": [
                    { "type": "x", "id": "g" }
                  ]
                }
              }
            },
            {
              "type": "x",
              "id": "e",
              "relationships": {
                "children": {
                  "data": [
                    { "type": "x", "id": "g" }
                  ]
                }
              }
            },
            {
              "type": "x",
              "id": "d",
              "relationships": {
                "children": {
                  "data": [
                    { "type": "x", "id": "e" }
                  ]
                }
              }
            },
            {
              "type": "x",
              "id": "c",
              "relationships": {
                "children": {
                  "data": [
                    { "type": "x", "id": "d" },
                    { "type": "x", "id": "e" }
                  ]
                }
              }
            }
          ]
        }
      """).toEncoding

      val actual = GraphEncoder.encodeObject(Some(a), IncludeSpec.unsafe("children.children"))
      actual.asValid shouldMatch expected
    }
  }

  it("should make a singular resource identifier document") {
    final case class MyId(id: String @@ IdTag)
    object MyId {
      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyId]("my")
    }

    GraphEncoder.encodeIdentifier(Some(MyId("888"))) shouldBe Document.forData(ResourceIdentifier("my", "888")).validNec
  }
}
