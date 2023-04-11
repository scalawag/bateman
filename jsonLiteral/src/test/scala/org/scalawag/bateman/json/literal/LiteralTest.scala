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

package org.scalawag.bateman.json.literal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.{JAnyEncoder, JArray, JBoolean, JNull, JNumber, JObject, JString}

import scala.collection.BitSet
import cats.syntax.contravariant._

class LiteralTest extends AnyFunSpec with Matchers {
  describe("json") {
    it("should compile") {
      val json: JObject = json"""
        {
          "a": 4,
          "b": true,
          "c": null,
          "d": [
            { "a": 8 },
            [1,2,3],
            "my\\\":{}[]()*_-12",
            true,
            false,
            null,
            []
          ]
        }
      """

      json shouldBe JObject(
        "a" -> JNumber(4),
        "b" -> JBoolean(true),
        "c" -> JNull,
        "d" -> JArray(
          JObject("a" -> JNumber(8)),
          JArray(JNumber(1), JNumber(2), JNumber(3)),
          JString("my\\\":{}[]()*_-12"),
          JBoolean(true),
          JBoolean(false),
          JNull,
          JArray()
        )
      )
    }

    it("should parse JSON text with interpolations") {
      val string = "what \"about\" escape\\sequences? 🤷"

      val json =
        json"""
          {
            "blah": $string,
            "6": 6,
            "C": ${List(0, 1, 4, 3, 5)},
            "D": ${false}
          }
        """

      json shouldBe JObject(
        "blah" -> JString(string),
        "6" -> JNumber(6),
        "C" -> JArray(List(0, 1, 4, 3, 5).map(JNumber(_)): _*),
        "D" -> JBoolean(false)
      )
    }

    it("should disallow any insertion without an encoder") {
      assertTypeError("""
        jsona"[${BitSet.empty}]"
      """)
    }

    it("should allow any insertion that has an encoder") {
      val insert = BitSet.empty

      implicit val customEncoder: JAnyEncoder[BitSet] = JAnyEncoder[String].contramap(_.toString)

      val json = json"""{"foo": $insert}"""

      json shouldBe JObject("foo" -> JString(insert.toString))
    }

    it("should not compile (expecting JSON object)") {
      assertTypeError("""
          json"[]"
        """)
    }

    it("should not compile (invalid JSON text)") {
      assertTypeError("""
          json"{"
        """)
    }

    it("should not compile (empty JSON text)") {
      assertTypeError("""
          json""
        """)
    }
  }

  describe("jsona") {
    it("should compile") {
      val json: JArray = jsona"""
        [
          4,
          true,
          null,
          { "a": 8 }
        ]
      """

      json shouldBe JArray(
        JNumber(4),
        JBoolean(true),
        JNull,
        JObject("a" -> JNumber(8))
      )
    }

    it("should parse JSON text with interpolations") {
      val string = "what \"about\" escape\\sequences? 🤷"

      val json =
        jsona"""
          [
            $string,
            6,
            ${List(0, 1, 4, 3, 5)},
            ${false}
          ]
        """

      json shouldBe JArray(
        JString(string),
        JNumber(6),
        JArray(List(0, 1, 4, 3, 5).map(JNumber(_)): _*),
        JBoolean(false)
      )
    }

    it("should disallow any insertion without an encoder") {
      assertTypeError("""
        jsona"[${BitSet.empty}]"
      """)
    }

    it("should allow any insertion that has an encoder") {
      val insert = BitSet.empty

      implicit val customEncoder: JAnyEncoder[BitSet] = JAnyEncoder[String].contramap(_.toString)

      val json = jsona"""[$insert]"""

      json shouldBe JArray(JString(insert.toString))
    }

    it("should not compile (expecting JSON array)") {
      assertTypeError("""
          jsona"{}"
        """)
    }

    it("should not compile (invalid JSON text)") {
      assertTypeError("""
          jsona"["
        """)
    }

    it("should not compile (empty JSON text)") {
      assertTypeError("""
          jsona""
        """)
    }
  }
}
