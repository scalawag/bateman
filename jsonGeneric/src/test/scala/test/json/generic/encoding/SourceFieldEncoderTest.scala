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

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.generic.semiauto.unchecked._
import org.scalawag.bateman.json.generic.{Config, Source}
import org.scalawag.bateman.json.literal._
import test.json.BatemanTestBase

object SourceFieldEncoderTest {
  object MySourceField {
    case class MyClass(@Source a: JSource, b: Int)
    implicit val encoderForMyClass: JObjectEncoder[MyClass] = deriveEncoderForCaseClass[MyClass]()
  }
  object MyOptionSourceField {
    case class MyClass(@Source a: Option[JSource], b: Int)
    implicit val encoderForMyClass: JObjectEncoder[MyClass] = deriveEncoderForCaseClass[MyClass]()
  }
  object MySourceFieldWithDefault {
    case class MyClass(@Source a: JSource = JSource(JObject("c" -> JNull).asRootFocus), b: Int = 42)
    implicit val encoderForMyClass: JObjectEncoder[MyClass] = deriveEncoderForCaseClass[MyClass]()
  }
  object MyOptionSourceFieldWithDefault {
    case class MyClass(@Source a: Option[JSource] = Some(JSource(JObject("c" -> JNull).asRootFocus)), b: Int = 77)
    implicit val encoderForMyClass: JObjectEncoder[MyClass] = deriveEncoderForCaseClass[MyClass]()
  }
}

class SourceFieldEncoderTest extends BatemanTestBase {
  private val simpleFocus = json"""{"b": 747}""".asRootFocus
  private val simpleSource = JSource(simpleFocus, Map("b" -> simpleFocus.field("b").shouldSucceed))

  private val collisionFocus = json"""{"a": "oops", "b":  808}""".asRootFocus
  private val collisionSource = JSource(collisionFocus, Map("b" -> collisionFocus.field("b").shouldSucceed))

  describe("MySourceField") {
    import SourceFieldEncoderTest.MySourceField._

    it("should ignore source") {
      MyClass(JSource(simpleFocus), 8).toJAny shouldBe json"""{"b": 8}"""
    }
  }

  describe("MyOptionSourceField") {
    import SourceFieldEncoderTest.MyOptionSourceField._

    it("should ignore None source") {
      MyClass(None, 8).toJAny shouldBe json"""{"b": 8}"""
    }

    it("should ignore Some source") {
      MyClass(Some(JSource(simpleFocus)), 8).toJAny shouldBe json"""{"b": 8}"""
    }
  }

  describe("MySourceFieldWithDefault") {
    import SourceFieldEncoderTest.MySourceFieldWithDefault._

    it("should ignore source") {
      MyClass(JSource(simpleFocus), 8).toJAny shouldBe json"""{"b": 8}"""
    }
  }

  describe("MyOptionSourceFieldWithDefault") {
    import SourceFieldEncoderTest.MyOptionSourceFieldWithDefault._

    it("should ignore None source") {
      MyClass(None, 8).toJAny shouldBe json"""{"b": 8}"""
    }

    it("should ignore Some source") {
      MyClass(Some(JSource(simpleFocus)), 8).toJAny shouldBe json"""{"b": 8}"""
    }
  }

  it("source ignore source field even with encodeDefaultValues") {
    import SourceFieldEncoderTest.MySourceFieldWithDefault.MyClass
    implicit val config = Config(encodeDefaultValues = true)

    val f = json"""{"a": null, "b": 747}""".asRootFocus
    val fa = f.field("a").shouldSucceed

    MyClass().toJAny shouldBe json"{}"
  }
}
