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

package org.scalawag.bateman.json.decoding.query

import cats.syntax.validated._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.{ParserTestUtils, encoding}
import org.scalawag.bateman.json.decoding.{
  JAny,
  JAnyGenerators,
  JArray,
  JObject,
  JsonTypeMismatch,
  UnspecifiedField,
  UnspecifiedIndex
}
import org.scalawag.bateman.json.decoding.query.TraverseQuery.RichTraverseQuery

class QueryTest extends AnyFunSpec with Matchers with ParserTestUtils with JAnyGenerators {
  private val json = parse("""
    {
      "a": {
        "g": 4,
        "f": "thing",
        "b": true
      },
      "b": 6,
      "g": [
        {
          "c": 8
        },
        {
          "c": "foo"
        }
      ],
      "deep": [
        [
          { "a": 4 },
          { "a": true },
          { "a": "g" }
        ],
        [
          { "a": 17 },
          { "a": 83 },
          { "a": 56.78e4 }
        ]
      ]
    }
  """)

  describe("root") {
    it("should create an identity query (which is useless)") {
      val q = root[JAny, Unit]

      Prop.forAll { a: JAny =>
        q(a, ()) == a.validNec
      }
    }
  }

  describe("field extraction") {
    it("should extract a field from an object using ~> string") {
      val a = arbitrary[JObject].retryUntil(_.fieldList.nonEmpty).sample.get
      val f = a.fieldList.head.name.value
      a.query(_ ~> f) shouldBe a.fieldList.head.value.validNec
    }

    it("should extract a field from an object using ~> field()") {
      val a = arbitrary[JObject].retryUntil(_.fieldList.nonEmpty).sample.get
      val f = a.fieldList.head.name.value
      a.query(_ ~> field(f)) shouldBe a.fieldList.head.value.validNec
    }

    it("should fail with a type error if the LHS is not an object") {
      val p = (root[JAny, Unit] ~> "a" ~> "b" ~> "c")(json, ())
      p shouldBe JsonTypeMismatch(
        json.query(_ ~> "a" ~> "b").getOrElse(???),
        JObject
      ).invalidNec
    }

    it("should fail with a missing error if the field doesn't exist") {
      val p = (root[JAny, Unit] ~> "a" ~> "z")(json, ())
      p shouldBe UnspecifiedField(
        json.query(_ ~> "a" ~> as[JObject]).getOrElse(???),
        "z"
      ).invalidNec
    }

    it("should extract None if the field doesn't exist (optionally)") {
      val p = (root[JAny, Unit] ~> "a" ~>? "z")(json, ())
      p shouldBe None.validNec
    }
  }

  describe("item indexing") {
    it("should extract a field from an object using ~> int") {
      val a = arbitrary[JArray].retryUntil(_.items.nonEmpty).sample.get
      a.query(_ ~> 0) shouldBe a.items.head.validNec
    }

    it("should extract a field from an object using ~> index()") {
      val a = arbitrary[JArray].retryUntil(_.items.nonEmpty).sample.get
      a.query(_ ~> index(0)) shouldBe a.items.head.validNec
    }

    it("should fail with a type error if the LHS is not an array") {
      val p = (root[JAny, Unit] ~> "a" ~> 8)(json, ())
      p shouldBe JsonTypeMismatch(
        json.query(_ ~> "a").getOrElse(???),
        JArray
      ).invalidNec
    }

    it("should fail with a missing error if the index doesn't exist") {
      val p = (root[JAny, Unit] ~> "g" ~> 9)(json, ())
      p shouldBe UnspecifiedIndex(
        json.query(_ ~> "g" ~> as[JArray]).getOrElse(???),
        9
      ).invalidNec
    }

    it("should extract None if the field doesn't exist (optionally)") {
      val p = (root[JAny, Unit] ~> "g" ~>? 9)(json, ())
      p shouldBe None.validNec
    }
  }

  describe("deep queries") {
    it("should extract fields from functor") {
      val p = (root[JAny, Unit] ~> "deep" ~> 0 ~> * ~> "a")(json, ())

      p.map(_.map(_.toEncoding)) shouldBe
        List(
          encoding.JNumber(4),
          encoding.JBoolean(true),
          encoding.JString("g"),
        ).validNec
    }

    // TODO: this works in Scala 2.13
//    it("should extract fields from deep functor") {
//      val p = (root[JAny, Unit] ~> "deep" ~> * ~> * ~> "a")(json, ())
//
//      p.map(_.map(_.map(_.toEncoding))) shouldBe
//        List(
//          List(
//            encoding.JNumber(4),
//            encoding.JBoolean(true),
//            encoding.JString("g"),
//          ),
//          List(
//            encoding.JNumber(17),
//            encoding.JNumber(83),
//            encoding.JNumber.unsafe("56.78e4")
//          )
//        ).validNec
//    }

    it("should extract fields from deep flatmap") {
      val p = (root[JAny, Unit] ~> "deep" ~> * ~>* * ~> "a")(json, ())

      p.map(_.map(_.toEncoding)) shouldBe
        List(
          encoding.JNumber(4),
          encoding.JBoolean(true),
          encoding.JString("g"),
          encoding.JNumber(17),
          encoding.JNumber(83),
          encoding.JNumber.unsafe("56.78e4")
        ).validNec
    }

    it("should not work too deeply") {
      // TODO: This is a problem. I'd like to get this so that it just works recursively, but I can't get Scala to
      //       follow the implicits chain far enough to see that it can generate the functors it needs.
      assertTypeError("""
        val p = (root[JAny] ~> "deep" ~> * ~> * ~> * ~> "a")(json)
      """)
    }
  }

  describe("decoding") {
    it("should decode a thing") {
      val q = root[JAny, Unit] ~> "b" ~> as[Int]
      val p = q(json, ())
      p shouldBe 6.validNec

    }

    it("should decode multiple things") {
      val q = root[JAny, Unit] ~> "deep" ~> 1 ~> * ~> "a" ~> as[BigDecimal]
      val p = q(json, ())
      p shouldBe List(BigDecimal(17), BigDecimal(83), BigDecimal(567800)).validNec
    }
  }
}
