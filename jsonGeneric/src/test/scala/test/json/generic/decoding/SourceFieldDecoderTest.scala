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

package test.json.generic.decoding

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, PascalCase}
import org.scalawag.bateman.json.generic.semiauto.unchecked._
import org.scalawag.bateman.json.generic.{Config, Source}
import org.scalawag.bateman.json.literal._

object SourceFieldDecoderTest {
  object MySourceField {
    case class MyClass(@Source a: JSource, b: Int)
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyOptionSourceField {
    case class MyClass(@Source a: Option[JSource], b: Int)
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MySourceFieldWithDefault {
    case class MyClass(@Source a: JSource = JSource(JObject("c" -> JNull).asRootFocus), b: Int = 42)
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyOptionSourceFieldWithDefault {
    case class MyClass(@Source a: Option[JSource] = Some(JSource(JObject("c" -> JNull).asRootFocus)), b: Int = 77)
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
}

class SourceFieldDecoderTest extends DecoderTestBase {
  private val simpleFocus = json"""{"b": 747}""".asRootFocus
  private val simpleSource = JSource(simpleFocus, Map("b" -> simpleFocus.field("b").shouldSucceed))

  private val collisionFocus = json"""{"a": "oops", "b":  808}""".asRootFocus
  private val collisionSource = JSource(collisionFocus, Map("b" -> collisionFocus.field("b").shouldSucceed))

  describe("MySourceField") {
    import SourceFieldDecoderTest.MySourceField._

    it("should succeed with no collisions") {
      simpleFocus.decode[MyClass].shouldSucceed shouldBe MyClass(simpleSource, 747)
    }
    it("should succeed with collisions") {
      collisionFocus.decode[MyClass].shouldSucceed shouldBe MyClass(collisionSource, 808)
    }
  }

  describe("MyOptionSourceField") {
    import SourceFieldDecoderTest.MyOptionSourceField._

    it("should succeed with no collisions") {
      simpleFocus.decode[MyClass].shouldSucceed shouldBe MyClass(Some(simpleSource), 747)
    }
    it("should succeed with collisions") {
      collisionFocus.decode[MyClass].shouldSucceed shouldBe MyClass(Some(collisionSource), 808)
    }
  }

  describe("MySourceFieldWithDefault") {
    import SourceFieldDecoderTest.MySourceFieldWithDefault._

    it("should succeed with no collisions") {
      simpleFocus.decode[MyClass].shouldSucceed shouldBe MyClass(simpleSource, 747)
    }
    it("should succeed with collisions") {
      collisionFocus.decode[MyClass].shouldSucceed shouldBe MyClass(collisionSource, 808)
    }
  }

  describe("MyOptionSourceFieldWithDefault") {
    import SourceFieldDecoderTest.MyOptionSourceFieldWithDefault._

    it("should succeed with no collisions") {
      simpleFocus.decode[MyClass].shouldSucceed shouldBe MyClass(Some(simpleSource), 747)
    }
    it("should succeed with collisions") {
      collisionFocus.decode[MyClass].shouldSucceed shouldBe MyClass(Some(collisionSource), 808)
    }
  }

  it("source field map should use Scala names, not JSON names") {
    import SourceFieldDecoderTest.MySourceField.MyClass
    implicit val decoder: JObjectDecoder[MyClass] =
      deriveDecoderForCaseClass[MyClass](Config(fieldNameMapping = CaseTransformation(CamelCase, PascalCase)))

    inside(json"""{"B": 747}""".asRootFocus.decode[MyClass].shouldSucceed) {
      case MyClass(src, 747) =>
        src.fields.keySet shouldBe Set("b") // map contains "b" even though JSON field is "B"
    }
  }

  it("source field should not count towards strict usage check") {
    import SourceFieldDecoderTest.MySourceField.MyClass
    implicit val decoder: JObjectDecoder[MyClass] =
      deriveDecoderForCaseClass[MyClass](Config(allowUnknownFields = false))

    val f = json"""{"a": null, "b": 747}""".asRootFocus
    val fa = f.field("a").shouldSucceed

    f.decode[MyClass].shouldFailSingle shouldBe UnexpectedValue(fa)
  }
}
