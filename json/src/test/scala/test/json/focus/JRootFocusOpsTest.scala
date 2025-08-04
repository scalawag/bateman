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

class JRootFocusOpsTest extends BatemanTestBase {

  // Each parseAs gives us a JRootFocus with known value type.
  val str: JRootFocus[JString] = parseAs[JString](""""hello"""")
  val num: JRootFocus[JNumber] = parseAs[JNumber]("42")
  val bool: JRootFocus[JBoolean] = parseAs[JBoolean]("true")
  val nul: JRootFocus[JNull] = parseAs[JNull]("null")
  val arr: JRootFocus[JArray] = parseAs[JArray]("""[1, 2, 3]""")
  val obj: JRootFocus[JObject] = parseAs[JObject]("""{"a": 1}""")

  describe("narrow") {
    it("should narrow to the correct type") {
      val result: JResult[JRootFocus[JString]] = str.narrow[JString]
      result.shouldSucceed.value.value shouldBe "hello"
    }

    it("should fail when narrowing to the wrong type") {
      val result: JResult[JRootFocus[JNumber]] = str.narrow[JNumber]
      result shouldBe JsonTypeMismatch(str, JNumber).leftNec
    }
  }

  describe("asString") {
    it("should succeed for a string root") {
      val result: JResult[JRootFocus[JString]] = str.asString
      result.shouldSucceed.value.value shouldBe "hello"
    }

    it("should fail for a non-string root") {
      val result: JResult[JRootFocus[JString]] = num.asString
      result shouldBe JsonTypeMismatch(num, JString).leftNec
    }
  }

  describe("asNumber") {
    it("should succeed for a number root") {
      val result: JResult[JRootFocus[JNumber]] = num.asNumber
      result.shouldSucceed.value.toBigDecimal shouldBe BigDecimal(42)
    }

    it("should fail for a non-number root") {
      val result: JResult[JRootFocus[JNumber]] = str.asNumber
      result shouldBe JsonTypeMismatch(str, JNumber).leftNec
    }
  }

  describe("asBoolean") {
    it("should succeed for a boolean root") {
      val result: JResult[JRootFocus[JBoolean]] = bool.asBoolean
      result.shouldSucceed.value.value shouldBe true
    }

    it("should fail for a non-boolean root") {
      val result: JResult[JRootFocus[JBoolean]] = str.asBoolean
      result shouldBe JsonTypeMismatch(str, JBoolean).leftNec
    }
  }

  describe("asNull") {
    it("should succeed for a null root") {
      val result: JResult[JRootFocus[JNull]] = nul.asNull
      result.shouldSucceed.value.isInstanceOf[JNull] shouldBe true
    }

    it("should fail for a non-null root") {
      val result: JResult[JRootFocus[JNull]] = str.asNull
      result shouldBe JsonTypeMismatch(str, JNull).leftNec
    }
  }

  describe("asArray") {
    it("should succeed for an array root") {
      val result: JResult[JRootFocus[JArray]] = arr.asArray
      result.shouldSucceed.value.items should have size 3
    }

    it("should fail for a non-array root") {
      val result: JResult[JRootFocus[JArray]] = str.asArray
      result shouldBe JsonTypeMismatch(str, JArray).leftNec
    }
  }

  describe("asObject") {
    it("should succeed for an object root") {
      val result: JResult[JRootFocus[JObject]] = obj.asObject
      result.shouldSucceed.value.fieldList should have size 1
    }

    it("should fail for a non-object root") {
      val result: JResult[JRootFocus[JObject]] = str.asObject
      result shouldBe JsonTypeMismatch(str, JObject).leftNec
    }
  }

  describe("modify (value, pure)") {
    it("should transform the value and preserve JRootFocus type") {
      val fn: JString => JNumber = s => JNumber(s.value.length)
      val result: JRootFocus[JNumber] = str.modify(fn)
      result.value.toBigDecimal shouldBe BigDecimal(5)
    }
  }

  describe("modify (value, fallible)") {
    it("should transform the value on success") {
      val fn: JString => JResult[JNumber] = s => JNumber(s.value.length).rightNec
      val result: JResult[JRootFocus[JNumber]] = str.modify(fn)
      result.shouldSucceed.value.toBigDecimal shouldBe BigDecimal(5)
    }

    it("should propagate the error on failure") {
      val err = JsonTypeMismatch(str, JNull)
      val fn: JString => JResult[JNumber] = _ => err.leftNec
      val result: JResult[JRootFocus[JNumber]] = str.modify(fn)
      result.shouldFailSingle shouldBe err
    }
  }

  describe("modify (focus, pure)") {
    it("should transform via focus and preserve JRootFocus type") {
      val fn: JRootFocus[JString] => JNumber = f => JNumber(f.value.value.length)
      val result: JRootFocus[JNumber] = str.modify(fn)
      result.value.toBigDecimal shouldBe BigDecimal(5)
    }
  }

  describe("modify (focus, fallible)") {
    it("should transform via focus on success") {
      val fn: JRootFocus[JString] => JResult[JNumber] = f => JNumber(f.value.value.length).rightNec
      val result: JResult[JRootFocus[JNumber]] = str.modify(fn)
      result.shouldSucceed.value.toBigDecimal shouldBe BigDecimal(5)
    }

    it("should propagate the error on failure") {
      val err = JsonTypeMismatch(str, JNull)
      val fn: JRootFocus[JString] => JResult[JNumber] = _ => err.leftNec
      val result: JResult[JRootFocus[JNumber]] = str.modify(fn)
      result.shouldFailSingle shouldBe err
    }
  }

  describe("root") {
    it("should return itself") {
      str.root shouldBe str
      obj.root shouldBe obj
    }
  }

  describe("pointer") {
    it("should always be root pointer") {
      str.pointer shouldBe JPointer.Root
      obj.pointer shouldBe JPointer.Root
    }
  }

  describe("parentOption") {
    it("should always be None") {
      str.parentOption shouldBe None
      obj.parentOption shouldBe None
    }
  }
}
