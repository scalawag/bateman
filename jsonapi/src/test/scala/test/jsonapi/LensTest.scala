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

package test.jsonapi

import org.scalawag.bateman.json.{Decoder, MissingField}
import test.json.BatemanTestBase
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.focus.weak._

class LensTest extends BatemanTestBase {
  // This has a number of semantic errors, but we just need something to navigate down into.
  val json = json"""
    {
      "data": {
        "type": "thing",
        "id": "23",
        "lid": "#1",
        "attributes": {
          "a": 17
        },
        "relationships": {
          "b": {
            "data": {
              "type": "other",
              "id": "B"
            },
            "links": {
              "self": {
                "href": "http://example.com/other/B",
                "meta": {
                  "key": "K0080"
                }
              }
            },
            "meta": {
              "created_on": "2023-03-24"
            }
          }
        },
        "meta": {
          "level": "top_secret"
        }
      },
      "included": [
        {
          "type": "other",
          "id": "B",
          "attributes": {
            "foo": "bar"
          },
          "meta": {
            "version": "4"
          }
        }
      ],
      "meta": {
        "service_version": "1.0.22"
      }
    }
  """.asRootFocus

  val errorJson = json"""
    {
      "errors": [
        {
          "id": "E001",
          "links": {
            "about": "http://example.com/errors/E001"
          },
          "status": "403",
          "code": "pseudo_error",
          "title": "Badness 10000",
          "detail": "Sometimes, bad things happen to good code.",
          "source": {
            "pointer": "/data/attributes/a",
            "header": "Content-Type",
            "parameter": "filter[name]"
          },
          "meta": {
            "more_info": "bar"
          }
        }
      ]
    }
  """.asRootFocus

  describe("id") {
    it("should get id") {
      json(data ~> id).value.shouldSucceed shouldBe "23".toJAny
    }

    it("should decode id") {
      implicit val dec = Decoder.jstringToJNumber.andThen(Decoder.jnumberToIntDecoder)
      json(data ~> id).decode[Int].shouldSucceed shouldBe 23
    }
  }

  describe("lid") {
    it("should get lid") {
      json(data ~> lid).value.shouldSucceed shouldBe "#1".toJAny
    }
  }

  describe("resourceType") {
    it("should get type") {
      json(data ~> resourceType).value.shouldSucceed shouldBe "thing".toJAny
    }
  }

  describe("attributes") {
    it("should focus on the attributes") {
      val f = json(data ~> attributes).shouldSucceed
      f shouldBe json.field("data").flatMap(_.asObject).flatMap(_.field("attributes")).shouldSucceed
    }
  }

  describe("attribute") {
    it("should focus on a single attribute") {
      val f = json(data ~> attribute("a")).shouldSucceed
      f shouldBe json
        .field("data")
        .flatMap(_.asObject)
        .flatMap(_.field("attributes"))
        .flatMap(_.asObject)
        .flatMap(_.field("a"))
        .shouldSucceed
    }

    it("should fail to find an attribute") {
      val f = json(data ~> attribute("b")).shouldFailSingle
      f shouldBe MissingField(
        json
          .field("data")
          .flatMap(_.asObject)
          .flatMap(_.field("attributes"))
          .flatMap(_.asObject)
          .shouldSucceed,
        "b"
      )
    }
  }

  describe("relationships") {
    it("should focus on the relationships") {
      val f = json(data ~> relationships).shouldSucceed
      f shouldBe json.field("data").flatMap(_.asObject).flatMap(_.field("relationships")).shouldSucceed
    }
  }

  describe("relationship") {
    it("should focus on a single relationship") {
      val f = json(data ~> relationship("b")).shouldSucceed
      f shouldBe json
        .field("data")
        .flatMap(_.asObject)
        .flatMap(_.field("relationships"))
        .flatMap(_.asObject)
        .flatMap(_.field("b"))
        .shouldSucceed
    }

    it("should fail to find a relationship") {
      val f = json(data ~> relationship("a")).shouldFailSingle
      f shouldBe MissingField(
        json
          .field("data")
          .flatMap(_.asObject)
          .flatMap(_.field("relationships"))
          .flatMap(_.asObject)
          .shouldSucceed,
        "a"
      )
    }
  }

  describe("meta") {
    it("should focus on the meta") {
      val f = json(data ~> meta).shouldSucceed
      f shouldBe json.field("data").flatMap(_.asObject).flatMap(_.field("meta")).shouldSucceed
    }

    it("should focus on a single meta") {
      val f = json(data ~> relationship("b") ~> meta("created_on")).shouldSucceed
      f shouldBe json
        .field("data")
        .flatMap(_.asObject)
        .flatMap(_.field("relationships"))
        .flatMap(_.asObject)
        .flatMap(_.field("b"))
        .flatMap(_.asObject)
        .flatMap(_.field("meta"))
        .flatMap(_.asObject)
        .flatMap(_.field("created_on"))
        .shouldSucceed
    }

    it("should fail to find a meta") {
      val f = json(data ~> meta("X")).shouldFailSingle
      f shouldBe MissingField(
        json
          .field("data")
          .flatMap(_.asObject)
          .flatMap(_.field("meta"))
          .flatMap(_.asObject)
          .shouldSucceed,
        "X"
      )
    }

    it("should add meta to existing document") {
      val out = json.writeTo(data ~> meta("quux"), "froboz".toJAny).shouldSucceed.root
      out(data ~> meta("quux")).value.shouldSucceed shouldBe "froboz".toJAny
    }
  }

  describe("errors") {
    it("should get id") {
      errorJson(errors ~> * ~> id).values.shouldSucceed shouldBe List("E001".toJAny)
    }

    it("should get link") {
      errorJson(errors ~> * ~> links ~> about).values.shouldSucceed shouldBe List(
        "http://example.com/errors/E001".toJAny
      )
    }

    it("should get status") {
      errorJson(errors ~> * ~> status).values.shouldSucceed shouldBe List("403".toJAny)
    }

    it("should get code") {
      errorJson(errors ~> * ~> code).values.shouldSucceed shouldBe List("pseudo_error".toJAny)
    }

    it("should get title") {
      errorJson(errors ~> * ~> title).values.shouldSucceed shouldBe List("Badness 10000".toJAny)
    }

    it("should get detail") {
      errorJson(errors ~> * ~> detail).values.shouldSucceed shouldBe List(
        "Sometimes, bad things happen to good code.".toJAny
      )
    }

    it("should get pointer") {
      errorJson(errors ~> * ~> source ~> pointer).values.shouldSucceed shouldBe List("/data/attributes/a".toJAny)
    }

    it("should get header") {
      errorJson(errors ~> * ~> source ~> header).values.shouldSucceed shouldBe List("Content-Type".toJAny)
    }

    it("should get parameter") {
      errorJson(errors ~> * ~> source ~> parameter).values.shouldSucceed shouldBe List("filter[name]".toJAny)
    }

    it("should get meta") {
      errorJson(errors ~> * ~> meta("more_info")).values.shouldSucceed shouldBe List("bar".toJAny)
    }
  }
}
