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
import test.json.BatemanTestBase

class JItemFocusOpsTest extends BatemanTestBase {

  // Array with one item of each JSON type, for narrowing tests.
  val json: JRootFocus[JArray] = parseAs[JArray]("""
    ["hello", 42, true, null, [1, 2], {"k": "v"}]
  """)

  // Helper: get an item focus by index.
  def itemFocus(index: Int): JItemFocus[JAny, JRootFocus[JArray]] =
    json.item(index).shouldSucceed

  // Concrete typed focus for modify tests (item 0 is "hello").
  def stringItemFocus: JItemFocus[JString, JRootFocus[JArray]] =
    itemFocus(0).asString.shouldSucceed

  describe("narrow") {
    it("should narrow an item to the correct type") {
      val result: JResult[JItemFocus[JString, JRootFocus[JArray]]] = itemFocus(0).narrow[JString]
      val f = result.shouldSucceed
      f.value.value shouldBe "hello"
      f.index shouldBe 0
    }

    it("should fail when narrowing to the wrong type") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JNumber, JRootFocus[JArray]]] = f.narrow[JNumber]
      result shouldBe JsonTypeMismatch(f, JNumber).leftNec
    }
  }

  describe("asString") {
    it("should succeed for a string item") {
      val result: JResult[JItemFocus[JString, JRootFocus[JArray]]] = itemFocus(0).asString
      result.shouldSucceed.value.value shouldBe "hello"
    }

    it("should fail for a non-string item") {
      val f = itemFocus(1)
      val result: JResult[JItemFocus[JString, JRootFocus[JArray]]] = f.asString
      result shouldBe JsonTypeMismatch(f, JString).leftNec
    }
  }

  describe("asNumber") {
    it("should succeed for a number item") {
      val result: JResult[JItemFocus[JNumber, JRootFocus[JArray]]] = itemFocus(1).asNumber
      result.shouldSucceed.value.toBigDecimal shouldBe BigDecimal(42)
    }

    it("should fail for a non-number item") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JNumber, JRootFocus[JArray]]] = f.asNumber
      result shouldBe JsonTypeMismatch(f, JNumber).leftNec
    }
  }

  describe("asBoolean") {
    it("should succeed for a boolean item") {
      val result: JResult[JItemFocus[JBoolean, JRootFocus[JArray]]] = itemFocus(2).asBoolean
      result.shouldSucceed.value.value shouldBe true
    }

    it("should fail for a non-boolean item") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JBoolean, JRootFocus[JArray]]] = f.asBoolean
      result shouldBe JsonTypeMismatch(f, JBoolean).leftNec
    }
  }

  describe("asNull") {
    it("should succeed for a null item") {
      val result: JResult[JItemFocus[JNull, JRootFocus[JArray]]] = itemFocus(3).asNull
      result.shouldSucceed.value.isInstanceOf[JNull] shouldBe true
    }

    it("should fail for a non-null item") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JNull, JRootFocus[JArray]]] = f.asNull
      result shouldBe JsonTypeMismatch(f, JNull).leftNec
    }
  }

  describe("asArray") {
    it("should succeed for an array item") {
      val result: JResult[JItemFocus[JArray, JRootFocus[JArray]]] = itemFocus(4).asArray
      result.shouldSucceed.value.items should have size 2
    }

    it("should fail for a non-array item") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JArray, JRootFocus[JArray]]] = f.asArray
      result shouldBe JsonTypeMismatch(f, JArray).leftNec
    }
  }

  describe("asObject") {
    it("should succeed for an object item") {
      val result: JResult[JItemFocus[JObject, JRootFocus[JArray]]] = itemFocus(5).asObject
      result.shouldSucceed.value.fieldList should have size 1
    }

    it("should fail for a non-object item") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JObject, JRootFocus[JArray]]] = f.asObject
      result shouldBe JsonTypeMismatch(f, JObject).leftNec
    }
  }

  describe("delete") {
    it("should remove the item and return the parent focus type") {
      val f = stringItemFocus
      val result: JRootFocus[JArray] = f.delete()
      result.value.items should have size (json.value.items.size - 1)
      result.root.value.shouldHaveNoLocations
    }

    it("should return a weak focus when called on a weak reference") {
      val weak: JFocus[JAny] = stringItemFocus
      val result: JFocus[JAny] = weak.delete().shouldSucceed
      assertTypeError("""
        val strong: JRootFocus[JArray] = weak.delete().shouldSucceed
      """)
      result.asArray.shouldSucceed.value.items should have size (json.value.items.size - 1)
    }
  }

  describe("modify (value, pure)") {
    it("should transform the item value and preserve the focus type") {
      val fn: JString => JNumber = s => JNumber(s.value.length)
      val result: JItemFocus[JNumber, JRootFocus[JArray]] = stringItemFocus.modify(fn)
      result.value.toBigDecimal shouldBe BigDecimal(5)
      result.index shouldBe 0
    }
  }

  describe("modify (value, fallible)") {
    it("should transform the item value on success") {
      val fn: JString => JResult[JNumber] = s => JNumber(s.value.length).rightNec
      val result: JResult[JItemFocus[JNumber, JRootFocus[JArray]]] = stringItemFocus.modify(fn)
      val f = result.shouldSucceed
      f.value.toBigDecimal shouldBe BigDecimal(5)
      f.index shouldBe 0
    }

    it("should propagate the error on failure") {
      val err = JsonTypeMismatch(stringItemFocus, JNull)
      val fn: JString => JResult[JNumber] = _ => err.leftNec
      val result: JResult[JItemFocus[JNumber, JRootFocus[JArray]]] = stringItemFocus.modify(fn)
      result.shouldFailSingle shouldBe err
    }
  }

  describe("modify (focus, pure)") {
    it("should transform via focus and preserve the focus type") {
      val f = stringItemFocus
      val fn: JItemFocus[JString, JRootFocus[JArray]] => JNumber = ff => JNumber(ff.value.value.length)
      val result: JItemFocus[JNumber, JRootFocus[JArray]] = f.modify(fn)
      result.value.toBigDecimal shouldBe BigDecimal(5)
    }
  }

  describe("modify (focus, fallible)") {
    it("should transform via focus on success") {
      val f = stringItemFocus
      val fn: JItemFocus[JString, JRootFocus[JArray]] => JResult[JNumber] =
        ff => JNumber(ff.value.value.length).rightNec
      val result: JResult[JItemFocus[JNumber, JRootFocus[JArray]]] = f.modify(fn)
      val out = result.shouldSucceed
      out.value.toBigDecimal shouldBe BigDecimal(5)
    }

    it("should propagate the error on failure") {
      val f = stringItemFocus
      val err = JsonTypeMismatch(f, JNull)
      val fn: JItemFocus[JString, JRootFocus[JArray]] => JResult[JNumber] = _ => err.leftNec
      val result: JResult[JItemFocus[JNumber, JRootFocus[JArray]]] = f.modify(fn)
      result.shouldFailSingle shouldBe err
    }
  }

  describe("root") {
    it("should return the root focus with the correct strong type") {
      val f = stringItemFocus
      val result: JRootFocus[JArray] = f.root
      result.value shouldBe json.value
    }
  }

  describe("previous") {
    it("should navigate to the previous sibling item") {
      val f = itemFocus(1)
      val result: JResult[JItemFocus[JAny, JRootFocus[JArray]]] = f.previous
      val prev = result.shouldSucceed
      prev.index shouldBe 0
    }

    it("should fail when there is no previous sibling") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JAny, JRootFocus[JArray]]] = f.previous
      result shouldBe MissingIndex(json, -1).leftNec
    }
  }

  describe("next") {
    it("should navigate to the next sibling item") {
      val f = itemFocus(0)
      val result: JResult[JItemFocus[JAny, JRootFocus[JArray]]] = f.next
      result.shouldSucceed.index shouldBe 1
    }

    it("should fail when there is no next sibling") {
      val f = itemFocus(json.value.items.size - 1)
      val result: JResult[JItemFocus[JAny, JRootFocus[JArray]]] = f.next
      result shouldBe MissingIndex(json, json.value.items.size).leftNec
    }
  }

  describe("first") {
    it("should navigate to the first item") {
      val f = itemFocus(3)
      val result: JItemFocus[JAny, JRootFocus[JArray]] = f.first
      result.index shouldBe 0
    }
  }

  describe("last") {
    it("should navigate to the last item") {
      val f = itemFocus(0)
      val result: JItemFocus[JAny, JRootFocus[JArray]] = f.last
      result.index shouldBe json.value.items.size - 1
    }
  }

  describe("with object root") {
    // When the array lives inside an object field, the parent type changes.
    val objJson: JRootFocus[JObject] = parseAs[JObject]("""{"scores": [10, 20, 30]}""")
    type ScoresField = JFieldFocus[JArray, JRootFocus[JObject]]
    val scoresField: ScoresField = objJson.field("scores").shouldSucceed.asArray.shouldSucceed

    def objItemFocus(index: Int): JItemFocus[JAny, ScoresField] =
      scoresField.item(index).shouldSucceed

    def objNumberItemFocus: JItemFocus[JNumber, ScoresField] =
      objItemFocus(0).asNumber.shouldSucceed

    it("should narrow an item and reflect the object-rooted parent chain") {
      val result: JResult[JItemFocus[JNumber, ScoresField]] = objItemFocus(0).narrow[JNumber]
      val f = result.shouldSucceed
      f.value.toBigDecimal shouldBe BigDecimal(10)
      f.parent shouldBe scoresField
    }

    it("should navigate to the root object through the parent chain") {
      val f = objNumberItemFocus
      val result: JRootFocus[JObject] = f.root
      result.value shouldBe objJson.value
    }

    it("should delete an item and return the field focus parent") {
      val f = objNumberItemFocus
      val result: ScoresField = f.delete()
      result.value.items should have size 2
      result.name.value shouldBe "scores"
    }

    it("should return a weak focus when delete is called on a weak reference") {
      val weak: JFocus[JAny] = objNumberItemFocus
      val result: JFocus[JAny] = weak.delete().shouldSucceed
      assertTypeError("""
        val strong: ScoresField = weak.delete().shouldSucceed
      """)
      result.asArray.shouldSucceed.value.items should have size 2
    }

    it("should modify an item value and preserve the parent chain") {
      val fn: JNumber => JString = n => JString(n.toBigDecimal.toString)
      val result: JItemFocus[JString, ScoresField] = objNumberItemFocus.modify(fn)
      result.value.value shouldBe "10"
      result.index shouldBe 0
      result.parent.name.value shouldBe "scores"
    }

    it("should navigate between siblings within the object-rooted array") {
      val f = objItemFocus(1)
      val prev: JResult[JItemFocus[JAny, ScoresField]] = f.previous
      prev.shouldSucceed.index shouldBe 0

      val next: JResult[JItemFocus[JAny, ScoresField]] = f.next
      next.shouldSucceed.index shouldBe 2

      val first: JItemFocus[JAny, ScoresField] = f.first
      first.index shouldBe 0

      val last: JItemFocus[JAny, ScoresField] = f.last
      last.index shouldBe 2
    }
  }
}
