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

package test.json

import org.scalawag.bateman.json._

class RendererTest extends BatemanTestBase {

  describe("NoSpacesRenderer") {
    it("should render null") {
      JNull.render shouldBe "null"
    }

    it("should render true") {
      JBoolean(true).render shouldBe "true"
    }

    it("should render false") {
      JBoolean(false).render shouldBe "false"
    }

    it("should render integer") {
      JNumber(42).render shouldBe "42"
    }

    it("should render decimal") {
      JNumber.unsafe("3.14").render shouldBe "3.14"
    }

    it("should render negative number") {
      JNumber(-7).render shouldBe "-7"
    }

    it("should render exponent") {
      JNumber.unsafe("1e10").render shouldBe "1e10"
    }

    it("should render simple string") {
      JString("hello").render shouldBe "\"hello\""
    }

    it("should render empty string") {
      JString("").render shouldBe "\"\""
    }

    it("should escape special characters in strings") {
      JString("a\"b").render shouldBe "\"a\\\"b\""
      JString("a\\b").render shouldBe "\"a\\\\b\""
      JString("a\bb").render shouldBe "\"a\\bb\""
      JString("a\fb").render shouldBe "\"a\\fb\""
      JString("a\rb").render shouldBe "\"a\\rb\""
      JString("a\nb").render shouldBe "\"a\\nb\""
      JString("a\tb").render shouldBe "\"a\\tb\""
    }

    it("should escape control characters") {
      JString("\u0000").render shouldBe "\"\\u0000\""
      JString("\u001e").render shouldBe "\"\\u001e\""
    }

    it("should render empty object") {
      JObject.Empty.render shouldBe "{}"
    }

    it("should render object with one field") {
      JObject("a" -> JNumber(1)).render shouldBe "{\"a\":1}"
    }

    it("should render object with multiple fields") {
      JObject("a" -> JNumber(1), "b" -> JNumber(2)).render shouldBe "{\"a\":1,\"b\":2}"
    }

    it("should render empty array") {
      JArray().render shouldBe "[]"
    }

    it("should render array with one element") {
      JArray(JNumber(1)).render shouldBe "[1]"
    }

    it("should render array with multiple elements") {
      JArray(JNumber(1), JNumber(2), JNumber(3)).render shouldBe "[1,2,3]"
    }

    it("should render nested structures") {
      val json = JObject(
        "arr" -> JArray(JNumber(1), JObject("x" -> JBoolean(true))),
        "nil" -> JNull
      )
      json.render shouldBe "{\"arr\":[1,{\"x\":true}],\"nil\":null}"
    }
  }

  describe("PrettySpaces2") {
    it("should render null") {
      JNull.spaces2 shouldBe "null"
    }

    it("should render primitives without indentation") {
      JString("hello").spaces2 shouldBe "\"hello\""
      JNumber(42).spaces2 shouldBe "42"
      JBoolean(true).spaces2 shouldBe "true"
    }

    it("should render empty object") {
      JObject.Empty.spaces2 shouldBe "{\n  \n}"
    }

    it("should render object with one field") {
      JObject("a" -> JNumber(1)).spaces2 shouldBe
        """{
          |  "a": 1
          |}""".stripMargin
    }

    it("should render object with multiple fields") {
      JObject("a" -> JNumber(1), "b" -> JNumber(2)).spaces2 shouldBe
        """{
          |  "a": 1,
          |  "b": 2
          |}""".stripMargin
    }

    it("should render empty array") {
      JArray().spaces2 shouldBe "[\n  \n]"
    }

    it("should render array with elements") {
      JArray(JNumber(1), JNumber(2)).spaces2 shouldBe
        """[
          |  1,
          |  2
          |]""".stripMargin
    }

    it("should render nested objects with correct indentation") {
      val json = JObject("outer" -> JObject("inner" -> JNumber(1)))
      json.spaces2 shouldBe
        """{
          |  "outer": {
          |    "inner": 1
          |  }
          |}""".stripMargin
    }

    it("should render nested arrays with correct indentation") {
      val json = JArray(JArray(JNumber(1), JNumber(2)))
      json.spaces2 shouldBe
        """[
          |  [
          |    1,
          |    2
          |  ]
          |]""".stripMargin
    }
  }

  describe("PrettySpaces4") {
    it("should use four-space indentation") {
      JObject("a" -> JNumber(1)).spaces4 shouldBe
        """{
          |    "a": 1
          |}""".stripMargin
    }
  }

  describe("SortedFields") {
    val sortedRenderer = new PrettySpaces2 with SortedFields

    it("should sort object fields alphabetically") {
      val json = JObject("c" -> JNumber(3), "a" -> JNumber(1), "b" -> JNumber(2))
      json.render(sortedRenderer) shouldBe
        """{
          |  "a": 1,
          |  "b": 2,
          |  "c": 3
          |}""".stripMargin
    }

    it("should sort nested object fields") {
      val json = JObject("z" -> JObject("b" -> JNumber(2), "a" -> JNumber(1)))
      json.render(sortedRenderer) shouldBe
        """{
          |  "z": {
          |    "a": 1,
          |    "b": 2
          |  }
          |}""".stripMargin
    }
  }

  describe("AsciiOnly") {
    val asciiRenderer = new PrettyRenderer(2) with AsciiOnly

    it("should escape non-ASCII characters above 0xff") {
      JString("\u0100").render(asciiRenderer) shouldBe "\"\\u0100\""
    }

    it("should not escape extended ASCII characters at or below 0xff") {
      JString("\u00e9").render(asciiRenderer) shouldBe "\"\u00e9\""
    }

    it("should not escape ASCII characters") {
      JString("hello").render(asciiRenderer) shouldBe "\"hello\""
    }
  }

  describe("round-trip through all renderers") {
    val renderers: List[(String, Renderer)] = List(
      "NoSpacesRenderer" -> NoSpacesRenderer,
      "PrettySpaces2" -> PrettySpaces2,
      "PrettySpaces4" -> PrettySpaces4,
      "PrettySpaces2 with SortedFields" -> new PrettySpaces2 with SortedFields,
      "PrettySpaces2 with AsciiOnly" -> new PrettyRenderer(2) with AsciiOnly,
    )

    val inputs: List[(String, JAny)] = List(
      "null" -> JNull,
      "true" -> JBoolean(true),
      "false" -> JBoolean(false),
      "integer" -> JNumber(42),
      "negative integer" -> JNumber(-7),
      "decimal" -> JNumber.unsafe("3.14"),
      "exponent" -> JNumber.unsafe("1e10"),
      "zero" -> JNumber(0),
      "large number" -> JNumber(Long.MaxValue),
      "empty string" -> JString(""),
      "simple string" -> JString("hello"),
      "string with quotes" -> JString("say \"hi\""),
      "string with backslash" -> JString("path\\to"),
      "string with newline" -> JString("line1\nline2"),
      "string with tab" -> JString("col1\tcol2"),
      "string with backspace" -> JString("ab\bc"),
      "string with formfeed" -> JString("ab\fc"),
      "string with carriage return" -> JString("ab\rc"),
      "string with control char" -> JString("ab\u0000cd"),
      "string with unicode" -> JString("caf\u00e9"),
      "string with high unicode" -> JString("\u0100\u0101"),
      "empty object" -> JObject.Empty,
      "simple object" -> JObject("key" -> JString("value")),
      "multi-field object" -> JObject("x" -> JNumber(1), "y" -> JNumber(2), "z" -> JNumber(3)),
      "empty array" -> JArray(),
      "simple array" -> JArray(JNumber(1), JNumber(2), JNumber(3)),
      "mixed array" -> JArray(JString("a"), JNumber(1), JBoolean(true), JNull),
      "nested object" -> JObject(
        "outer" -> JObject(
          "middle" -> JObject(
            "inner" -> JString("deep")
          )
        )
      ),
      "nested array" -> JArray(JArray(JArray(JNumber(1)))),
      "complex structure" -> JObject(
        "id" -> JNumber(99),
        "name" -> JString("test\twith\ttabs"),
        "active" -> JBoolean(true),
        "score" -> JNumber.unsafe("98.6"),
        "tags" -> JArray(JString("alpha"), JString("beta")),
        "metadata" -> JObject(
          "created" -> JString("2024-01-01"),
          "notes" -> JNull
        ),
        "items" -> JArray(
          JObject("a" -> JNumber(1)),
          JObject("b" -> JNumber(2))
        ),
        "empty" -> JObject.Empty,
        "emptyList" -> JArray()
      ),
    )

    for ((rendererName, renderer) <- renderers) {
      describe(rendererName) {
        for ((inputName, input) <- inputs) {
          it(s"should round-trip $inputName") {
            val rendered = renderer.render(input)
            val reparsed = parse(rendered).value.stripLocation
            renderer.render(reparsed) shouldBe renderer.render(input)
          }
        }
      }
    }
  }
}
