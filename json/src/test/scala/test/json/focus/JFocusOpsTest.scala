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

import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import org.scalawag.bateman.json.lens._
import test.json.BatemanTestBase

class JFocusOpsTest extends BatemanTestBase {

  val janyMutator: JAny => JAny = {
    case _: JNull    => JNumber(-1)
    case s: JString  => JNumber(s.value.length)
    case b: JBoolean => if (b.value) JNumber(1001) else JNumber(1000)
    case n: JNumber  => JNumber(n.toBigDecimal.toInt * 17)
    case a: JArray   => a.prepend(JBoolean(false))
    case o: JObject  => o.append("modified", JBoolean(true))
  }

  val weakFocusMutator: JFocus[JAny] => JAny = janyMutator.compose(_.value)

  describe("replace") {
    it("should replace the value in focus") {
      forAll(genJFocus(genJAny), genJAny) { (in, value) =>
        val out = in.replace(value)

        out.pointer shouldBe in.pointer
        out.value shouldBe value.stripLocation
        out.root.value.shouldHaveNoLocations

        inside(out.parentOption, in.parentOption) {
          case (Some(JFocus.Value(o: JObject)), Some(JFocus.Value(i: JObject))) =>
            val index = in.asInstanceOf[JFieldFocus[_, _]].index
            o shouldBe i.updated(index, value)
          case (Some(JFocus.Value(o: JArray)), Some(JFocus.Value(i: JArray))) =>
            val index = in.asInstanceOf[JItemFocus[_, _]].index
            o shouldBe i.updated(index, value)
          case (None, None) =>
            succeed
        }
      }
    }

    it("should return a narrow type (compilation)") {
      JNumber(4).asRootFocus.replace(JString("s")): JFocus[JString]
    }
  }

  describe("modify (weak, pure)") {
    it("should modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modify(janyMutator)

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JAny => JString = ???
      def out: JFocus[JString] = (JNumber(4).asRootFocus: JFocus[JAny]).modify(fn)
    }
  }

  describe("modify (weak, fallible)") {
    it("should modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modify(v => janyMutator(v).rightNec).shouldSucceed

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should fail to modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modify(_ => JsonTypeMismatch(f, JNull).leftNec)

        modified shouldBe JsonTypeMismatch(f, JNull).leftNec
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JAny => JResult[JString] = ???
      def out: JResult[JFocus[JString]] = (JNumber(4).asRootFocus: JFocus[JAny]).modify(fn)
    }
  }

  describe("modifyFocus (weak, pure)") {
    it("should modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modifyFocus(weakFocusMutator)

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JFocus[JAny] => JString = ???
      def out: JFocus[JString] = (JNumber(4).asRootFocus: JFocus[JAny]).modifyFocus(fn)
    }
  }

  describe("modifyFocus (weak, fallible)") {
    it("should modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modifyFocus(ff => weakFocusMutator(ff).rightNec).shouldSucceed

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should fail to modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modifyFocus(ff => JsonTypeMismatch(ff, JNull).leftNec)

        modified shouldBe JsonTypeMismatch(f, JNull).leftNec
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JFocus[JAny] => JResult[JString] = ???
      def out: JResult[JFocus[JString]] = (JNumber(4).asRootFocus: JFocus[JAny]).modifyFocus(fn)
    }
  }

  describe("delete") {
    it("should delete the value in focus") {
      forAll(genJFocus(genJAny).retryUntil(_.parentOption.isDefined)) { in =>
        val out = in.delete().shouldSucceed

        out.pointer shouldBe in.pointer.parent
        out.root.value.shouldHaveNoLocations

        inside((out.value, in.parentOption)) {
          case (o: JObject, Some(JFocus.Value(i: JObject))) =>
            val index = in.asInstanceOf[JFieldFocus[_, _]].index
            o shouldBe i.delete(index)
          case (o: JArray, Some(JFocus.Value(i: JArray))) =>
            val index = in.asInstanceOf[JItemFocus[_, _]].index
            o shouldBe i.delete(index)
        }
      }
    }

    it("should fail for root foci") {
      forAll(genJAny) { in =>
        in.asRootFocus.delete() shouldBe NoParent(in.asRootFocus).leftNec
      }
    }
  }

  // --- decodeFrom tests (from original JFocusTest) ---

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

  describe("writeTo") {
    it("should write a new field") {
      val obj: JFocus[JAny] = parseAs[JObject]("""{"a": 1}""")
      val result = obj.writeTo("b", "new").shouldSucceed
      result.value shouldBe JString("new")
    }

    it("should write with prepend = true") {
      val obj: JFocus[JAny] = parseAs[JObject]("""{"a": 1}""")
      val result = obj.writeTo("b" ~> "c", "deep", prepend = true).shouldSucceed
      result.value shouldBe JString("deep")
      // Verify "b" was prepended
      val root = result.root.value.asInstanceOf[JObject]
      root.fieldList.head.name.value shouldBe "b"
    }

    it("should fail when navigating through a non-object") {
      val obj: JFocus[JAny] = parseAs[JObject]("""{"a": 1}""")
      val result = obj.writeTo("a" ~> "b", "value")
      val err = result.shouldFail
      err.head shouldBe a[JsonTypeMismatch]
    }
  }

  // TODO: create
}
