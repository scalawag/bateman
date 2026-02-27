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
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import test.json.BatemanTestBase

import scala.reflect.ClassTag

class JFocusTest extends BatemanTestBase {

  describe("narrowing") {
    val ins: List[JRootFocus[JAny]] = List(JString("a"), JNumber(4), JBoolean(true), JNull, JObject.Empty, JArray.Empty).map(_.asRootFocus)

    def narrowTestCases[A <: JAny: ClassTag: Summoner](): Unit = {
      describe(s"narrow[${JType[A]}]") {
        ins.foreach { in =>
          if (in.value.jType == JType[A])
            it(s"should succeed from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = in.narrow[A]
              out shouldBe in.rightNec
            }
          else
            it(s"should fail from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = in.narrow[A]
              out shouldBe JsonTypeMismatch(in, JType[A]).leftNec
            }
        }
      }
    }

    narrowTestCases[JNull]()
    narrowTestCases[JNumber]()
    narrowTestCases[JString]()
    narrowTestCases[JBoolean]()
    narrowTestCases[JObject]()
    narrowTestCases[JArray]()

    def asTestCases[A <: JAny: Summoner](fn: JFocus[JAny] => JResult[JFocus[A]]): Unit = {
      describe(s"as${JType[A].toString.tail}") {
        ins.foreach { in =>
          if (in.value.jType == JType[A])
            it(s"should succeed from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = fn(in)
              out shouldBe in.rightNec
            }
          else
            it(s"should fail from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = fn(in)
              out shouldBe JsonTypeMismatch(in, JType[A]).leftNec
            }
        }
      }
    }

    asTestCases(_.asNull)
    asTestCases(_.asNumber)
    asTestCases(_.asString)
    asTestCases(_.asBoolean)
    asTestCases(_.asObject)
    asTestCases(_.asArray)
  }

  describe("map") {
    it("should map over a root focus") {
      val f: JFocus[JString] = JString("hello").asRootFocus
      val result: JFocus[JNumber] = f.map(s => JNumber(s.value.length))
      result.value.toBigDecimal shouldBe BigDecimal(5)
      result.pointer shouldBe f.pointer
    }

    it("should map over a field focus") {
      val obj = parseAs[JObject]("""{"a": "hello"}""")
      val f: JFocus[JAny] = obj.field("a").shouldSucceed
      val result = f.map(_ => JNumber(42))
      result.value.toBigDecimal shouldBe BigDecimal(42)
      result.pointer shouldBe f.pointer
    }

    it("should map over an item focus") {
      val arr = parseAs[JArray]("""["hello", "world"]""")
      val f: JFocus[JAny] = arr.item(1).shouldSucceed
      val result = f.map(_ => JNumber(42))
      result.value.toBigDecimal shouldBe BigDecimal(42)
      result.pointer shouldBe f.pointer
    }
  }

  describe("map with constant") {
    it("should replace the value while preserving focus structure") {
      val f: JFocus[JAny] = JString("hello").asRootFocus
      val result: JFocus[JNumber] = f.map(_ => JNumber(42))
      result.value.toBigDecimal shouldBe BigDecimal(42)
      result.pointer shouldBe f.pointer
    }

    it("should work on a field focus") {
      val obj = parseAs[JObject]("""{"a": "hello"}""")
      val f: JFocus[JAny] = obj.field("a").shouldSucceed
      val result = f.map(_ => JNumber(42))
      result.value.toBigDecimal shouldBe BigDecimal(42)
      result.pointer shouldBe f.pointer
    }
  }

  describe("navigate") {
    val json = parseAs[JObject]("""
      {
        "users": [
          {"name": "alice"},
          {"name": "bob"}
        ]
      }
    """)

    it("should navigate to a nested field via key tokens") {
      val pointer = JPointer.Root.field("users")
      val result = json.navigate(pointer).shouldSucceed
      result.value shouldBe a[JArray]
    }

    it("should navigate into an array via index token") {
      val pointer = JPointer.Root.field("users").item(0)
      val result = json.navigate(pointer).shouldSucceed
      result.asObject.shouldSucceed.field("name").shouldSucceed.asString.shouldSucceed.value.value shouldBe "alice"
    }

    it("should navigate to a deeply nested value via mixed tokens") {
      val pointer = JPointer.Root.field("users").item(1).field("name")
      val result = json.navigate(pointer).shouldSucceed
      result.asString.shouldSucceed.value.value shouldBe "bob"
    }

    it("should fail when an index token targets a non-array") {
      val pointer = JPointer.Root.field("users").item(0).field("name").item(0)
      val nameFocus = json.navigate(JPointer.Root.field("users").item(0).field("name")).shouldSucceed
      val err = json.navigate(pointer).shouldFail
      err.head shouldBe JsonTypeMismatch(nameFocus, JArray)
    }

    it("should fail when a key token targets a non-object") {
      val pointer = JPointer.Root.field("users").field("invalid")
      val usersFocus = json.navigate(JPointer.Root.field("users")).shouldSucceed
      val err = json.navigate(pointer).shouldFail
      err.head shouldBe JsonTypeMismatch(usersFocus, JObject)
    }

    it("should return itself for an empty pointer") {
      val result = json.navigate(JPointer.Root).shouldSucceed
      result.value shouldBe json.value
    }
  }

  describe("decode") {
    it("should decode the value") {
      forAll(genJFocus(genJString)) { in =>
        val out = in.decode[String].shouldSucceed

        out shouldBe in.value.value
      }
    }

    it("should fail of the decoder fails") {
      forAll(genJFocus(genJAny)) {
        case in @ JFocus.Value(s: JString) =>
          in.decode[String].shouldSucceed shouldBe s.value
        case in =>
          in.decode[String] shouldBe JsonTypeMismatch(in, JString).leftNec
      }
    }

    it("should decode the value with explicit decoder (use case)") {
      forAll(genJFocus(genJString)) { in =>
        val out = in.decode(Decoder.stringDecoder).shouldSucceed
        out shouldBe in.value.value
      }
    }
  }
}
