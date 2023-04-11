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

package test.json.generic.encoding

import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, PascalCase}
import org.scalawag.bateman.json.generic.semiauto.unchecked.deriveEncoderForCaseClass
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.{JNumber, _}
import test.json.BatemanTestBase

object FieldEncoderTest {
  object MyBareField {
    case class MyClass(a: Int)
  }
  object MyOptionField {
    case class MyClass(a: Option[Int])
  }
  object MyNullableField {
    case class MyClass(a: Nullable[Int])
  }
  object MyOptionNullableField {
    case class MyClass(a: Option[Nullable[Int]])
  }
  object MyBareFieldWithDefault {
    case class MyClass(a: Int = 417)
  }
  object MyOptionFieldWithDefault {
    case class MyClass(a: Option[Int] = Some(7))
  }
  object MyNullableFieldWithDefault {
    case class MyClass(a: Nullable[Int] = NotNull(7))
  }
  object MyOptionNullableFieldWithDefault {
    case class MyClass(a: Option[Nullable[Int]] = Some(NotNull(7)))
  }
}

class FieldEncoderTest extends BatemanTestBase {
  describe("MyBareField") {
    import FieldEncoderTest.MyBareField._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode bare field") {
      MyClass(8) shouldEncodeTo json"""{"a": 8}"""
    }
  }

  describe("MyOptionField") {
    import FieldEncoderTest.MyOptionField._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode Some") {
      MyClass(Some(8)) shouldEncodeTo json"""{"a": 8}"""
    }

    it("should encode None") {
      MyClass(None) shouldEncodeTo json"""{}"""
    }
  }

  describe("MyNullableField") {
    import FieldEncoderTest.MyNullableField._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode NotNull") {
      MyClass(NotNull(8)) shouldEncodeTo json"""{"a": 8}"""
    }

    it("should encode Null") {
      MyClass(Null) shouldEncodeTo json"""{"a": null}"""
    }
  }

  describe("MyOptionNullableField") {
    import FieldEncoderTest.MyOptionNullableField._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode Some NotNull") {
      MyClass(Some(NotNull(8))) shouldEncodeTo json"""{"a": 8}"""
    }

    it("should encode Some Null") {
      MyClass(Some(Null)) shouldEncodeTo json"""{"a": null}"""
    }

    it("should encode None") {
      MyClass(None) shouldEncodeTo json"""{}"""
    }
  }

  describe("MyBareFieldWithDefault") {
    import FieldEncoderTest.MyBareFieldWithDefault._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode bare field") {
      MyClass(8) shouldEncodeTo json"""{"a": 8}"""
    }

    it("should not encode default value") {
      MyClass(417) shouldEncodeTo json"""{}"""
    }

    it("should encode default value") {
      implicit val config: Config = Config(encodeDefaultValues = true)
      MyClass(417) shouldEncodeTo json"""{"a": 417}"""
    }
  }

  describe("MyOptionFieldWithDefault") {
    import FieldEncoderTest.MyOptionFieldWithDefault._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode Some") {
      MyClass(Some(8)) shouldEncodeTo json"""{"a": 8}"""
    }

    it("should encode None") {
      MyClass(None) shouldEncodeTo json"""{}"""
    }

    it("should not encode default value") {
      MyClass(Some(7)) shouldEncodeTo json"""{}"""
    }

    it("should encode default value") {
      implicit val config: Config = Config(encodeDefaultValues = true)
      MyClass(Some(7)) shouldEncodeTo json"""{"a":7}"""
    }
  }

  describe("MyNullableFieldWithDefault") {
    import FieldEncoderTest.MyNullableFieldWithDefault._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode NotNull") {
      MyClass(NotNull(8)) shouldEncodeTo json"""{"a": 8}"""
    }

    it("should encode Null") {
      MyClass(Null) shouldEncodeTo json"""{"a":  null}"""
    }

    it("should not encode default value") {
      MyClass(NotNull(7)) shouldEncodeTo json"""{}"""
    }

    it("should encode default value") {
      implicit val config: Config = Config(encodeDefaultValues = true)
      MyClass(NotNull(7)) shouldEncodeTo json"""{"a": 7}"""
    }
  }

  describe("MyOptionNullableFieldWithDefault") {
    import FieldEncoderTest.MyOptionNullableFieldWithDefault._
    import org.scalawag.bateman.json.generic.auto._

    it("should encode Some NotNull") {
      MyClass(Some(NotNull(8))) shouldEncodeTo json"""{"a": 8}"""
    }

    it("should encode Some Null") {
      MyClass(Some(Null)) shouldEncodeTo json"""{"a": null}"""
    }

    it("should encode None") {
      MyClass(None) shouldEncodeTo json"""{}"""
    }

    it("should not encode default value") {
      MyClass(Some(NotNull(7))) shouldEncodeTo json"""{}"""
    }

    it("should encode default value") {
      implicit val config: Config = Config(encodeDefaultValues = true)
      MyClass(Some(NotNull(7))) shouldEncodeTo json"""{"a": 7}"""
    }
  }

  it("shoud") {
    List(1,2,3).toJAny
  }

  it("should heed the configuration for field names") {
    import FieldEncoderTest.MyBareField.MyClass
    implicit val decoder: JAnyEncoder[MyClass] =
      deriveEncoderForCaseClass[MyClass](Config(fieldNameMapping = CamelCase to PascalCase))

    MyClass(-13437) shouldEncodeTo json"""{"A": -13437}"""
  }
}
