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
import org.scalawag.bateman.json.focus.{JCursor, JFocus}
import org.scalawag.bateman.json.lens._
import test.json.BatemanTestBase

class JCursorTest extends BatemanTestBase {
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
        "foo",
        []
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

  private def emptyFoci: JCursor[List, JAny] = json("w".**).shouldSucceed

  describe("values") {

    it("should extract all values from a list cursor") {
      val cursor = json("deep" ~> * ~> 0 ~> "a").shouldSucceed
      val values = cursor.values
      values.map(_.stripLocation) shouldBe List(JNumber(4), JNumber(17))
    }

    it("should return Nil for empty cursor") {
      emptyFoci.values shouldBe Nil
    }

    it("should extract values matching the foci") {
      val cursor = json("g" ~> *).shouldSucceed
      cursor.values shouldBe cursor.foci.map(_.value)
    }
  }

  describe("modify") {

    it("should modify all foci and produce a consistent document") {
      val cursor = json("deep" ~> * ~> * ~> "a").shouldSucceed
      val modified = cursor.modify(_ => JNull)

      // All modified foci should have JNull values
      modified.values.foreach(_ shouldBe JNull)

      // The root document should reflect all modifications
      val root = modified.root.get
      val allAs = root("deep" ~> * ~> * ~> "a").shouldSucceed.values
      allAs.foreach(_ shouldBe JNull)
    }

    it("should preserve unrelated parts of the document") {
      val cursor = json("a" ~> **).shouldSucceed
      val modified = cursor.modify(_ => JNumber(0))
      val root = modified.root.get

      // "b" and "g" fields should be untouched
      root("b").shouldSucceed.value.stripLocation shouldBe JNumber(6)
      root("g" ~> 1).shouldSucceed.value.stripLocation shouldBe JString("foo")
    }

    it("should return the same cursor for empty foci") {
      val modified = emptyFoci.modify(_ => JNull)
      modified.foci shouldBe Nil
    }

    it("should modify array items and keep indices consistent") {
      val cursor = json("g" ~> *).shouldSucceed
      cursor.foci should have size 3

      val modified = cursor.modify { f =>
        f.value match {
          case _: JObject => JString("was-object")
          case _: JString => JString("was-string")
          case _: JArray  => JString("was-array")
          case other      => other
        }
      }

      modified.values.map(_.stripLocation) shouldBe List(
        JString("was-object"),
        JString("was-string"),
        JString("was-array")
      )
    }

    it("should handle nested modifications correctly") {
      // Modify all "a" fields inside "deep" to be their string representation
      val cursor = json("deep" ~> * ~> * ~> "a").shouldSucceed
      cursor.foci should have size 6

      val modified = cursor.modify(f => JString(f.value.render))
      modified.foci should have size 6
      modified.values.foreach {
        case _: JString => succeed
        case other      => fail(s"Expected JString but got $other")
      }
    }
  }

  describe("delete") {

    it("should delete all foci and produce a document without them") {
      val cursor = json("deep" ~> * ~> * ~> "a").shouldSucceed
      cursor.foci should have size 6

      val deleted = cursor.delete().shouldSucceed
      val root = deleted.root.get
      // After deleting all "a" fields, none should remain
      root("deep" ~> * ~> * ~> "a".**).shouldSucceed.foci shouldBe empty
      // But the containing objects should still exist
      root("deep" ~> * ~> *).shouldSucceed.foci should have size 6
    }

    it("should delete array items and shrink the arrays") {
      val cursor = json("g" ~> *).shouldSucceed
      cursor.foci should have size 3

      val deleted = cursor.delete().shouldSucceed
      // After deleting all items, the parent array should be empty
      val root = deleted.root.get
      root("g" ~> narrowTo[JArray]).shouldSucceed.value.items shouldBe empty
    }

    it("should succeed with no changes for empty cursor") {
      val deleted = emptyFoci.delete().shouldSucceed
      deleted.foci shouldBe Nil
    }

    it("should fail when trying to delete root foci") {
      val rootCursor = JCursor(List(json.asInstanceOf[JFocus[JAny]]))
      rootCursor.delete().shouldFail
    }

    it("should produce a valid document after partial deletion") {
      // check preconditions
      val arrays = json("deep" ~> * ~> narrowTo[JArray]).shouldSucceed
      arrays.foci.length shouldBe 2
      arrays.foci.map(_.value.length) shouldBe List(3, 3)

      val cursor = json("deep" ~> * ~> 0).shouldSucceed
      cursor.foci.length shouldBe 2

      // Delete the first item of each inner array in "deep"
      val deleted = cursor.delete().shouldSucceed
      val root = deleted.root.get

      // Each array in the new doc should now have 2 items instead of 3
      val newArrays = root("deep" ~> * ~> narrowTo[JArray]).shouldSucceed
      newArrays.foci.length shouldBe 2
      newArrays.foci.map(_.value.length) shouldBe List(2, 2)
    }
  }

  describe("decode") {

    it("should decode all foci to Scala values") {
      val strJson = parseAs[JObject]("""{"items": ["hello", "world", "!"]}""")
      val cursor = strJson("items" ~> * ~> narrowTo[JString]).shouldSucceed
      val decoded = cursor.decode[String]
      decoded.shouldSucceed shouldBe List("hello", "world", "!")
    }

    it("should accumulate errors when multiple foci fail to decode") {
      // "g" contains an object, a string, and an empty array — decoding all as String fails for non-strings
      val cursor = json("g" ~> *).shouldSucceed
      cursor.foci should have size 3
      val errors = cursor.decode[String].shouldFail // object and array can't decode as String
      errors.map(_.pointer).iterator.toList shouldBe List(JPointer.Root / "g" / 0, JPointer.Root / "g" / 2)
    }

    it("should return empty for empty cursor") {
      val decoded = emptyFoci.decode[String]
      decoded.shouldSucceed shouldBe Nil
    }
  }

  describe("root") {

    it("should return None for an empty cursor") {
      emptyFoci.root shouldBe None
    }

    it("should return the root document for a non-empty cursor") {
      val cursor = json("deep" ~> * ~> 0).shouldSucceed
      val root = cursor.root
      root shouldBe defined
      root.get.value shouldBe json.value
    }

    it("should return a single root document for a non-empty list cursor") {
      val cursor = json("deep" ~> *).shouldSucceed
      cursor.foci.length shouldBe 2
      val root = cursor.root
      root shouldBe defined
      root.get.value shouldBe json.value
    }

    it("should return the modified root after modify") {
      val cursor = json("a" ~> **).shouldSucceed
      val modified = cursor.modify(_ => JString("changed"))
      val root = modified.root.get
      // All fields of "a" should now be "changed"
      root("a" ~> **).shouldSucceed.values.foreach { v =>
        v.stripLocation shouldBe JString("changed")
      }
    }
  }

  describe("foci") {

    it("should contain the correct number of foci for a list lens") {
      json("g" ~> *).shouldSucceed.foci should have size 3
      json("deep" ~> *).shouldSucceed.foci should have size 2
      json("deep" ~> * ~> *).shouldSucceed.foci should have size 6
    }

    it("should contain foci that point into the same document") {
      val cursor = json("deep" ~> * ~> *).shouldSucceed
      val roots = cursor.foci.map(_.root.value)
      roots.distinct should have size 1
    }

    it("should support option cursor with Some") {
      // Types are explicit here to ensure that we're getting the right conversions/types.
      val lens: JCursorLens[Option, JAny, JAny] = "a".?
      val cursor: JCursor[Option, JAny] = json(lens).shouldSucceed
      cursor.foci shouldBe Some(json.asObject.shouldSucceed.field("a").shouldSucceed)
    }

    it("should support option cursor with None") {
      // Types are explicit here to ensure that we're getting the right conversions/types.
      val lens: JCursorLens[Option, JAny, JAny] = "nonexistent".?
      val cursor: JCursor[Option, JAny] = json(lens).shouldSucceed
      cursor.foci shouldBe None
    }
  }

  describe("cursorToFoci implicit conversion") {
    it("should implicitly allow using cursor as its underlying foci collection") {
      val cursor = json("g" ~> *).shouldSucceed
      cursor.map(_.value) shouldBe cursor.foci.map(_.value)
    }
  }
}
