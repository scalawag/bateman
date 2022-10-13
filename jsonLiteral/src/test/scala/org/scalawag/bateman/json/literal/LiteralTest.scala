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

package org.scalawag.bateman.json.literal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.encoding.{JAnyEncoder, JArray, JBoolean, JNull, JNumber, JObject, JString}
import scala.collection.BitSet
import cats.syntax.contravariant._

class LiteralTest extends AnyFunSpec with Matchers {
  it("should parse JSON text with no variables") {
    val json = jany"""
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
      jany"""
        {
          "blah": $string,
          "6": 6,
          "C": ${List(0, 1, 4, 3, 5)},
          "D": ${false}
        }
      """

    json shouldBe JObject(
      "blah" -> JString("what \"about\" escape\\sequences? 🤷"),
      "6" -> JNumber(6),
      "C" -> JArray(List(0, 1, 4, 3, 5).map(JNumber(_)): _*),
      "D" -> JBoolean(false)
    )
  }

  it("should disallow any insertion without an encoder") {
    val insert = BitSet.empty

    assertTypeError("json\"\"\"{\"foo\":$insert}\"\"\"")
  }

  it("should allow any insertion that has an encoder") {
    val insert = BitSet.empty

    implicit val urlEncoder: JAnyEncoder[BitSet] = JAnyEncoder[String].contramap(_.toString)

    val json =
      jany"""
        {
          "foo": $insert
        }
      """

    json shouldBe JObject("foo" -> JString(insert.toString))
  }

  it("should be a narrower type when jobject is used") {
    val j: JObject = jobject"{}"
  }

  it("should be a narrower type when jarray is used") {
    val j: JArray = jarray"[]"
  }

  it("should fail to compile if the JSON text is not an object") {
    assertTypeError("""
      jobject"[]"
    """)
  }

  it("should fail to compile if the JSON text is not an array") {
    assertTypeError("""
      jarray"true"
    """)
  }
}
