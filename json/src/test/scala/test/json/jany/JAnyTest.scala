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

package test.json.jany

import org.scalawag.bateman.json.focus.{JFocus, JRootFocus}
import test.json.BatemanTestBase
import org.scalawag.bateman.json.{JAny, JArray, JBoolean, JNull, JNumber, JObject, JString}

class JAnyTest extends BatemanTestBase {
  describe("rendering") {
    val json = parse("""
      {
        "a": true,
        "b": 4.5,
        "c": "foo"
      }
    """).value

    it("should render minimally") {
      json.render shouldBe """{"a":true,"b":4.5,"c":"foo"}"""
    }

    it("should render with 2 spaces") {
      json.spaces2 shouldBe """
        |{
        |  "a": true,
        |  "b": 4.5,
        |  "c": "foo"
        |}
      """.trim.stripMargin
    }

    it("should render with 4 spaces") {
      json.spaces4 shouldBe """
        |{
        |    "a": true,
        |    "b": 4.5,
        |    "c": "foo"
        |}
      """.trim.stripMargin
    }
  }

  describe("asRootFocus") {
    it("should create a root focus from a JAny") {
      val json: JAny = JString("quux")
      val focus: JFocus[JAny] = json.asRootFocus
      focus shouldBe JRootFocus(json)
    }

    it("should create a specifically-typed root focus") {
      val json = JString("quux")
      val focus: JFocus[JString] = json.asRootFocus
      focus shouldBe JRootFocus(json)
    }
  }

  describe("stripLocation") {
    val json = parse("""
      {
        "a": true,
        "b": 4.5,
        "c": "foo",
        "d": null,
        "e": [1],
        "f": {
          "g": 7
        }
      }
    """).value

    it("should have location info in parsed JSON") {
      json.stripLocation shouldNot be(json)
    }

    it("should strip all location info") {
      val stripped = json.stripLocation

      stripped shouldBe JObject(
        "a" -> JBoolean(true),
        "b" -> JNumber(4.5),
        "c" -> JString("foo"),
        "d" -> JNull,
        "e" -> JArray(JNumber(1)),
        "f" -> JObject("g" -> JNumber(7))
      )
    }
  }
}
