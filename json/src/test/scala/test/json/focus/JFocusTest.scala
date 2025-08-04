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

package test.json.focus

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import org.scalawag.bateman.json.lens._
import test.json.BatemanTestBase

class JFocusTest extends BatemanTestBase {
  private val json = parseAs[JObject]("""
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
          "c": 12
        }
      ],
      "h": [1, 2, 3],
      "opt": { "x": 99 },
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

  describe("decodeFrom (focus lens)") {
    it("should decode a nested field") {
      json.decodeFrom[Int]("a" ~> "g") shouldBe Right(4)
    }

    it("should decode a top-level field") {
      json.decodeFrom[Int]("b") shouldBe Right(6)
    }

    it("should decode a string field") {
      json.decodeFrom[String]("a" ~> "f") shouldBe Right("thing")
    }

    it("should fail on type mismatch") {
      val err = json.decodeFrom[Int]("a" ~> "f").shouldFail
      err.head shouldBe a[JsonTypeMismatch]
    }

    it("should fail on missing field") {
      val err = json.decodeFrom[Int]("a" ~> "missing").shouldFail
      err.head shouldBe a[MissingField]
    }

    it("should decode a boolean") {
      json.decodeFrom[Boolean]("a" ~> "b") shouldBe Right(true)
    }
  }

  describe("decodeFrom (cursor lens)") {
    it("should decode all items in an array") {
      json.decodeFrom[Int]("h" ~> *).shouldSucceed shouldBe List(1, 2, 3)
    }

    it("should decode nested fields across array items") {
      json.decodeFrom[Int]("g" ~> * ~> "c").shouldSucceed shouldBe List(8, 12)
    }

    it("should fail when any item has a type mismatch") {
      val badJson: JRootFocus[JAny] = parse("""{ "items": [1, "two", 3] }""")
      badJson.decodeFrom[Int]("items" ~> *).shouldFail
    }

    it("should decode with an optional lens") {
      json.decodeFrom[Int]("opt" ~> "x".?).shouldSucceed shouldBe Some(99)
    }

    it("should return None for a missing optional field") {
      json.decodeFrom[Int]("opt" ~> "missing".?).shouldSucceed shouldBe None
    }

    it("should decode all values with **") {
      val nums: JRootFocus[JAny] = parse("""{ "a": { "x": 1, "y": 2, "z": 3 } }""")
      nums.decodeFrom[Int]("a" ~> **).shouldSucceed shouldBe List(1, 2, 3)
    }

    it("should accumulate errors across cursor items") {
      json.decodeFrom[Int]("a" ~> **).shouldFail.length shouldBe 2
    }
  }
}
