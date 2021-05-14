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

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.{JBoolean, JLocation, JPointer, JString}
import org.scalawag.bateman.json.generic.SourceTag
import org.scalawag.bateman.jsonapi.decoding.{Document, ResourceObject}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, RelationshipTag, semiauto}
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.jsonapi.decoding
import org.scalawag.bateman.jsonapi.query._
import shapeless.tag.@@

import java.util.UUID

object DerivedFieldPointersTest {
  sealed trait X {
    val id: String
    val a: Int
    val src: Option[JSource]
  }

  object X {
    implicit val decoder: TraitResourceDecoder[decoding.ResourceObject, X] =
      semiauto.deriveResourceObjectDecoderForTrait[X]()
  }

  final case class Y(
      id: String @@ IdTag,
      a: Int @@ AttributeTag,
      b: String @@ AttributeTag,
      src: Option[JSource] @@ SourceTag = None
  ) extends X

  object Y {
    implicit val decoder: CaseClassResourceDecoder[decoding.ResourceObject, Y] =
      semiauto.deriveResourceObjectDecoderForCaseClass[Y]()
  }

  final case class Z(
      id: String @@ IdTag,
      a: Int @@ AttributeTag,
      c: Boolean @@ AttributeTag,
      src: Option[JSource] @@ SourceTag = None
  ) extends X

  object Z {
    implicit val decoder: CaseClassResourceDecoder[decoding.ResourceObject, Z] =
      semiauto.deriveResourceObjectDecoderForCaseClass[Z]()
  }

  final case class XX(
      id: String @@ IdTag,
      x: X @@ RelationshipTag,
      src: JSource @@ SourceTag
  )

  object XX {
    implicit val decoder: CaseClassResourceDecoder[decoding.ResourceObject, XX] =
      semiauto.deriveResourceObjectDecoderForCaseClass[XX]()
  }
}

class DerivedFieldPointersTest extends AnyFunSpec with Matchers with ParserTestUtils {
  import DerivedFieldPointersTest._

  it("should work for case classes") {
    val doc = parseAs[Document]("""
      {
        "data" : {
          "type" : "Y",
          "id" : "yyy",
          "attributes" : {
            "a": 31,
            "b" : "foo"
          }
        }
      }
    """)

    val y = doc.dquery(_ ~> data ~> required ~> as[ResourceObject] ~> as[Y]).shouldSucceed
    val b = y.src.get.getFieldSource("b")
    b shouldBe JString("foo", JLocation(8, 19), JPointer.Root / "data" / "attributes" / "b").validNec
  }

  it("should work for traits") {
    val doc = parseAs[Document]("""
        {
          "data" : {
            "type" : "Z",
            "id" : "ZZZ",
            "attributes" : {
              "a": 8,
              "c" : false
            }
          }
        }
      """)

    val x = doc.dquery(_ ~> data ~> required ~> as[ResourceObject] ~> as[X]).shouldSucceed
    x.src.get.getFieldSource("c") shouldBe JBoolean(
      false,
      JLocation(8, 21),
      JPointer.Root / "data" / "attributes" / "c"
    ).validNec
  }

  it("should work for nested fields") {
    val doc = parseAs[Document]("""
        {
          "data" : {
            "type" : "XX",
            "id" : "xxxx",
            "relationships": {
              "x": {
                "data": {
                  "type": "Z",
                  "id": "ZZZ"
                }
              }
            }
          },
          "included": [
            {
              "type" : "Z",
              "id" : "ZZZ",
              "attributes" : {
                "a": 8,
                "c" : false
              }
            }
          ]
        }
      """)

    val xx = doc.dquery(_ ~> data ~> required ~> as[ResourceObject] ~> as[XX]).shouldSucceed
    xx.src.getFieldSource("x").shouldSucceed shouldBe doc.data.get.src.asObject
      .getOrElse(fail)
      .fields
      .getOrElse(fail)("relationships")
      .value
      .asObject
      .getOrElse(fail)
      .fields
      .getOrElse(fail)("x")
      .value
    xx.x.src.get.getFieldSource("c").shouldSucceed shouldBe JBoolean(
      false,
      JLocation(21, 23),
      JPointer.Root / "included" / 0 / "attributes" / "c"
    )
  }
}
