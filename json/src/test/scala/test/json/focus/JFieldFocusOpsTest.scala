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

class JFieldFocusOpsTest extends BatemanTestBase {

  // Object with one field of each JSON type, for narrowing tests.
  val json: JRootFocus[JObject] = parseAs[JObject]("""
    {
      "name": "alice",
      "age": 30,
      "tags": ["a", "b"],
      "active": true,
      "meta": null,
      "addr": {"city": "x"}
    }
  """)

  // Helper: get a field focus by name.
  def fieldFocus(name: String): JFieldFocus[JAny, JRootFocus[JObject]] =
    json.field(name).shouldSucceed

  // Concrete typed foci for modify tests.
  def nameFocus: JFieldFocus[JString, JRootFocus[JObject]] =
    fieldFocus("name").asString.shouldSucceed

  describe("narrow") {
    it("should narrow a field to the correct type") {
      val result: JResult[JFieldFocus[JString, JRootFocus[JObject]]] = fieldFocus("name").narrow[JString]
      val f = result.shouldSucceed
      f.value.value shouldBe "alice"
      f.name.value shouldBe "name"
      f.index shouldBe 0
    }

    it("should fail when narrowing to the wrong type") {
      val f = fieldFocus("name")
      val result: JResult[JFieldFocus[JNumber, JRootFocus[JObject]]] = f.narrow[JNumber]
      result shouldBe JsonTypeMismatch(f, JNumber).leftNec
    }
  }

  describe("asString") {
    it("should succeed for a string field") {
      val result: JResult[JFieldFocus[JString, JRootFocus[JObject]]] = fieldFocus("name").asString
      result.shouldSucceed.value.value shouldBe "alice"
    }

    it("should fail for a non-string field") {
      val f = fieldFocus("age")
      val result: JResult[JFieldFocus[JString, JRootFocus[JObject]]] = f.asString
      result shouldBe JsonTypeMismatch(f, JString).leftNec
    }
  }

  describe("asNumber") {
    it("should succeed for a number field") {
      val result: JResult[JFieldFocus[JNumber, JRootFocus[JObject]]] = fieldFocus("age").asNumber
      result.shouldSucceed.value.toBigDecimal shouldBe BigDecimal(30)
    }

    it("should fail for a non-number field") {
      val f = fieldFocus("name")
      val result: JResult[JFieldFocus[JNumber, JRootFocus[JObject]]] = f.asNumber
      result shouldBe JsonTypeMismatch(f, JNumber).leftNec
    }
  }

  describe("asBoolean") {
    it("should succeed for a boolean field") {
      val result: JResult[JFieldFocus[JBoolean, JRootFocus[JObject]]] = fieldFocus("active").asBoolean
      result.shouldSucceed.value.value shouldBe true
    }

    it("should fail for a non-boolean field") {
      val f = fieldFocus("name")
      val result: JResult[JFieldFocus[JBoolean, JRootFocus[JObject]]] = f.asBoolean
      result shouldBe JsonTypeMismatch(f, JBoolean).leftNec
    }
  }

  describe("asNull") {
    it("should succeed for a null field") {
      val result: JResult[JFieldFocus[JNull, JRootFocus[JObject]]] = fieldFocus("meta").asNull
      result.shouldSucceed.value.isInstanceOf[JNull] shouldBe true
    }

    it("should fail for a non-null field") {
      val f = fieldFocus("name")
      val result: JResult[JFieldFocus[JNull, JRootFocus[JObject]]] = f.asNull
      result shouldBe JsonTypeMismatch(f, JNull).leftNec
    }
  }

  describe("asArray") {
    it("should succeed for an array field") {
      val result: JResult[JFieldFocus[JArray, JRootFocus[JObject]]] = fieldFocus("tags").asArray
      result.shouldSucceed.value.items should have size 2
    }

    it("should fail for a non-array field") {
      val f = fieldFocus("name")
      val result: JResult[JFieldFocus[JArray, JRootFocus[JObject]]] = f.asArray
      result shouldBe JsonTypeMismatch(f, JArray).leftNec
    }
  }

  describe("asObject") {
    it("should succeed for an object field") {
      val result: JResult[JFieldFocus[JObject, JRootFocus[JObject]]] = fieldFocus("addr").asObject
      result.shouldSucceed.value.fieldList should have size 1
    }

    it("should fail for a non-object field") {
      val f = fieldFocus("name")
      val result: JResult[JFieldFocus[JObject, JRootFocus[JObject]]] = f.asObject
      result shouldBe JsonTypeMismatch(f, JObject).leftNec
    }
  }

  describe("delete") {
    it("should remove the field and return the parent focus type") {
      val f = nameFocus
      val result: JRootFocus[JObject] = f.delete()
      result.value.fieldList.map(_.name.value) should not contain "name"
      result.value.fieldList should have size (json.value.fieldList.size - 1)
      result.root.value.shouldHaveNoLocations
    }

    it("should return a weak focus when called on a weak reference") {
      val weak: JFocus[JAny] = nameFocus
      val result: JFocus[JAny] = weak.delete().shouldSucceed
      assertTypeError("""
        val strong: JRootFocus[JObject] = weak.delete().shouldSucceed
      """)
      result.asObject.shouldSucceed.value.fieldList.map(_.name.value) should not contain "name"
    }
  }

  describe("modify (value, pure)") {
    it("should transform the field value and preserve the focus type") {
      val fn: JString => JNumber = s => JNumber(s.value.length)
      val result: JFieldFocus[JNumber, JRootFocus[JObject]] = nameFocus.modify(fn)
      result.value.toBigDecimal shouldBe BigDecimal(5)
      result.name.value shouldBe "name"
    }
  }

  describe("modify (value, fallible)") {
    it("should transform the field value on success") {
      val fn: JString => JResult[JNumber] = s => JNumber(s.value.length).rightNec
      val result: JResult[JFieldFocus[JNumber, JRootFocus[JObject]]] = nameFocus.modify(fn)
      val f = result.shouldSucceed
      f.value.toBigDecimal shouldBe BigDecimal(5)
      f.name.value shouldBe "name"
    }

    it("should propagate the error on failure") {
      val err = JsonTypeMismatch(nameFocus, JNull)
      val fn: JString => JResult[JNumber] = _ => err.leftNec
      val result: JResult[JFieldFocus[JNumber, JRootFocus[JObject]]] = nameFocus.modify(fn)
      result.shouldFailSingle shouldBe err
    }
  }

  describe("modify (focus, pure)") {
    it("should transform via focus and preserve the focus type") {
      val f = nameFocus
      val fn: JFieldFocus[JString, JRootFocus[JObject]] => JNumber = ff => JNumber(ff.value.value.length)
      val result: JFieldFocus[JNumber, JRootFocus[JObject]] = f.modify(fn)
      result.value.toBigDecimal shouldBe BigDecimal(5)
      result.name.value shouldBe "name"
    }
  }

  describe("modify (focus, fallible)") {
    it("should transform via focus on success") {
      val f = nameFocus
      val fn: JFieldFocus[JString, JRootFocus[JObject]] => JResult[JNumber] =
        ff => JNumber(ff.value.value.length).rightNec
      val result: JResult[JFieldFocus[JNumber, JRootFocus[JObject]]] = f.modify(fn)
      val out = result.shouldSucceed
      out.value.toBigDecimal shouldBe BigDecimal(5)
    }

    it("should propagate the error on failure") {
      val f = nameFocus
      val err = JsonTypeMismatch(f, JNull)
      val fn: JFieldFocus[JString, JRootFocus[JObject]] => JResult[JNumber] = _ => err.leftNec
      val result: JResult[JFieldFocus[JNumber, JRootFocus[JObject]]] = f.modify(fn)
      result.shouldFailSingle shouldBe err
    }
  }

  describe("root") {
    it("should return the root focus with the correct strong type") {
      val f = nameFocus
      val result: JRootFocus[JObject] = f.root
      result.value shouldBe json.value
    }
  }

  describe("previous") {
    it("should navigate to the previous sibling field") {
      val f = fieldFocus("age") // index 1
      val result: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = f.previous
      result.shouldSucceed.name.value shouldBe "name"
    }

    it("should fail when there is no previous sibling") {
      val f = fieldFocus("name") // index 0
      val result: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = f.previous
      result shouldBe MissingFieldIndex(json, -1).leftNec
    }
  }

  describe("next") {
    it("should navigate to the next sibling field") {
      val f = fieldFocus("name") // index 0
      val result: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = f.next
      result.shouldSucceed.name.value shouldBe "age"
    }

    it("should fail when there is no next sibling") {
      val f = fieldFocus("addr") // last field
      val result: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = f.next
      result shouldBe MissingFieldIndex(json, json.value.fieldList.size).leftNec
    }
  }

  describe("first") {
    it("should navigate to the first field") {
      val f = fieldFocus("addr")
      val result: JFieldFocus[JAny, JRootFocus[JObject]] = f.first
      result.name.value shouldBe "name"
      result.index shouldBe 0
    }
  }

  describe("last") {
    it("should navigate to the last field") {
      val f = fieldFocus("name")
      val result: JFieldFocus[JAny, JRootFocus[JObject]] = f.last
      result.name.value shouldBe "addr"
      result.index shouldBe json.value.fieldList.size - 1
    }
  }

  describe("with array root") {
    // When the object lives inside an array item, the parent type changes.
    val arrJson: JRootFocus[JArray] = parseAs[JArray]("""[{"x": 1, "y": 2, "z": 3}]""")
    type ObjItem = JItemFocus[JObject, JRootFocus[JArray]]
    val objItem: ObjItem = arrJson.item(0).shouldSucceed.asObject.shouldSucceed

    def arrFieldFocus(name: String): JFieldFocus[JAny, ObjItem] =
      objItem.field(name).shouldSucceed

    def arrNumberFieldFocus: JFieldFocus[JNumber, ObjItem] =
      arrFieldFocus("x").asNumber.shouldSucceed

    it("should narrow a field and reflect the array-rooted parent chain") {
      val result: JResult[JFieldFocus[JNumber, ObjItem]] = arrFieldFocus("x").narrow[JNumber]
      val f = result.shouldSucceed
      f.value.toBigDecimal shouldBe BigDecimal(1)
      f.parent shouldBe objItem
    }

    it("should navigate to the root array through the parent chain") {
      val f = arrNumberFieldFocus
      val result: JRootFocus[JArray] = f.root
      result.value shouldBe arrJson.value
    }

    it("should delete a field and return the item focus parent") {
      val f = arrNumberFieldFocus
      val result: ObjItem = f.delete()
      result.value.fieldList.map(_.name.value) should not contain "x"
      result.value.fieldList should have size 2
    }

    it("should return a weak focus when delete is called on a weak reference") {
      val weak: JFocus[JAny] = arrNumberFieldFocus
      val result: JFocus[JAny] = weak.delete().shouldSucceed
      assertTypeError("""
        val strong: ObjItem = weak.delete().shouldSucceed
      """)
      result.asObject.shouldSucceed.value.fieldList.map(_.name.value) should not contain "x"
    }

    it("should modify a field value and preserve the parent chain") {
      val fn: JNumber => JString = n => JString(n.toBigDecimal.toString)
      val result: JFieldFocus[JString, ObjItem] = arrNumberFieldFocus.modify(fn)
      result.value.value shouldBe "1"
      result.name.value shouldBe "x"
      result.parent.index shouldBe 0
    }

    it("should navigate between siblings within the array-rooted object") {
      val f = arrFieldFocus("y")
      val prev: JResult[JFieldFocus[JAny, ObjItem]] = f.previous
      prev.shouldSucceed.name.value shouldBe "x"

      val next: JResult[JFieldFocus[JAny, ObjItem]] = f.next
      next.shouldSucceed.name.value shouldBe "z"

      val first: JFieldFocus[JAny, ObjItem] = f.first
      first.name.value shouldBe "x"

      val last: JFieldFocus[JAny, ObjItem] = f.last
      last.name.value shouldBe "z"
    }
  }
}
