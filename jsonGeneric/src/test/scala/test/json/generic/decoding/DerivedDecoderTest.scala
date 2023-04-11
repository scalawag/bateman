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

import cats.syntax.either._
import org.scalawag.bateman.json.generic.Discriminators.{CustomDiscriminator, forType}
import org.scalawag.bateman.json.generic.decoding.{InvalidDiscriminator, JSource}
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, PascalCase, SnakeCase}
import org.scalawag.bateman.json.generic.{Config, Source, semiauto}
import org.scalawag.bateman.json.literal.JsonStringContext
import org.scalawag.bateman.json.{
  JNumber,
  JObject,
  JObjectDecoder,
  JString,
  JsonTypeMismatch,
  MissingField,
  ProgrammerError,
  UnexpectedValue
}
import test.json.BatemanTestBase

class DerivedDecoderTest extends BatemanTestBase {

  it("should decode perfectly") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7, "b": "XXX", "c": true}""".asRootFocus

    implicit val dec: JObjectDecoder[X] = semiauto.unchecked.deriveDecoderForCaseClass[X]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", true).rightNec
  }

  it("should decode perfectly with auto") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7, "b": "XXX", "c": true}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", true).rightNec
  }

  it("should fail on mismatched type") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": "7", "b": "XXX", "c": true}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[X].decode(json)
    da shouldBe JsonTypeMismatch(json.fields.head, JNumber).leftNec
  }

  it("should fail on missing field") {
    import DerivedDecoderTest.Default._

    val json = json"""{"b": "XXX", "c": true}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[X].decode(json)
    da shouldBe MissingField(json, "a").leftNec
  }

  it("should decode missing fields with defaults") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "jack", false).rightNec
  }

  it("should decode a mixture of missing (but defaulted) fields") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7, "b": "XXX"}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", false).rightNec
  }

  it("should fail to decode fields with defaults when configured not to use them") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7, "c": true}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    implicit val config = Config(useDefaultsForMissingFields = false)
    val da = JObjectDecoder[X].decode(json)
    da shouldBe MissingField(json, "b").leftNec
  }

  it("should ignore extra fields") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7, "b": "XXX", "c": true, "d": {}}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", true).rightNec
  }

  it("should fail on extra fields when configured to") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7, "b": "XXX", "d": 5.67}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    implicit val config: Config = Config(allowUnknownFields = false)
    val da = JObjectDecoder[X].decode(json)
    da shouldBe UnexpectedValue(json.field("d").shouldSucceed).leftNec
  }

  it("should fail on extra fields when configured to (with field name mapping)") {
    import DerivedDecoderTest.LongNames._

    val json = json"""{"longer_name": "7", "d": 5.67}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    implicit val config: Config = Config(allowUnknownFields = false, fieldNameMapping = CamelCase to SnakeCase)
    val da = JObjectDecoder[YNamedClass].decode(json)
    da shouldBe UnexpectedValue(json.field("d").shouldSucceed).leftNec
  }

  it("should require a discriminator for trait decoding") {
    import DerivedDecoderTest.Default._

    val json = json"""{"a": 7}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._
    val da = JObjectDecoder[Z].decode(json)
    da shouldBe MissingField(json, "type").leftNec
  }

  it("should decode trait with discriminator (default)") {
    import DerivedDecoderTest.Default._

    val json = json"""{"type": "X", "a": 7}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[Z].decode(json)
    da shouldBe X(7, "jack", false).rightNec
  }

  it("should decode trait with discriminator (explicit)") {
    import DerivedDecoderTest.Default._

    val json = json"""{"type": "XX", "a": 7}""".asRootFocus

    implicit val xdec: JObjectDecoder[X] = semiauto.unchecked.deriveDecoderForCaseClass[X]()
    implicit val ydec: JObjectDecoder[Y] = semiauto.unchecked.deriveDecoderForCaseClass[Y]()
    implicit val dec: JObjectDecoder[Z] = semiauto.unchecked.deriveDecoderForTrait[Z](discriminator =
      CustomDiscriminator(
        forType[X]("XX"),
        forType[Y]("YY"),
      )
    )
    val da = JObjectDecoder[Z].decode(json)
    da.shouldSucceed shouldBe X(7, "jack", false)
  }

  it("should fail to decode if missing the concrete fields of the discriminated type") {
    import DerivedDecoderTest.Default._

    val json = json"""{"type": "X"}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    val da = JObjectDecoder[Z].decode(json)
    // The discriminator field is stripped from the object by the abstract decoder before the concrete decoder has a go.
    da shouldBe MissingField(json, "a").leftNec
  }

  it("should use name transformation for fields") {
    case class X(longerName: String)

    val json = json"""{"longer_name": "XXX"}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    implicit val config: Config = Config(fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
    val da = JObjectDecoder[X].decode(json)
    da shouldBe X("XXX").rightNec
  }

  it("should use name transformation for classes") {
    import DerivedDecoderTest.LongNames._

    val json = json"""{"type": "z_named_class"}""".asRootFocus

    import org.scalawag.bateman.json.generic.auto._

    implicit val config: Config = Config(classNameMapping = PascalCase to SnakeCase)
    val da = JObjectDecoder[XNamedClass].decode(json)
    da.shouldSucceed shouldBe ZNamedClass()
  }

  it("should detect invalid discriminator") {
    val json = json"""{"type": "x_named_class"}""".asRootFocus

    import DerivedDecoderTest.LongNames._
    import org.scalawag.bateman.json.generic.auto._

    implicit val config: Config = Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
    json.decode[XNamedClass] shouldBe InvalidDiscriminator(
      json.field("type").flatMap(_.asString).shouldSucceed,
      Set(JString("y_named_class"), JString("z_named_class"))
    ).leftNec
  }

  object Data {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._

    sealed trait XNamedClass
    case class YNamedClass(longerName: String) extends XNamedClass
    case class ZNamedClass(b: Int = 8) extends XNamedClass

    object XNamedClass {
      implicit def xdec: JObjectDecoder[XNamedClass] = deriveDecoderForTrait[XNamedClass]()
    }
    object YNamedClass {
      implicit def ydec: JObjectDecoder[YNamedClass] = deriveDecoderForCaseClass[YNamedClass]()
    }
    object ZNamedClass {
      implicit def zdec: JObjectDecoder[ZNamedClass] = deriveDecoderForCaseClass[ZNamedClass]()
    }
  }

  describe("source annotation") {
    it("should inject source into tagged case class field") {
      case class MyClass(b: Int, @Source src: JSource)

      val json = json"""{"b": 31}""".asRootFocus

      val decoded = semiauto.unchecked.deriveDecoderForCaseClass[MyClass]().decode(json)
      decoded.shouldSucceed shouldBe MyClass(
        31,
        JSource(json, Map("b" -> json.field("b").shouldSucceed))
      )
    }

    it("should inject source into optional tagged case class field") {
      case class MyClass(b: Int, @Source src: Option[JSource] = None)

      val json = json"""{"b": 31}""".asRootFocus

      val decoded = semiauto.unchecked.deriveDecoderForCaseClass[MyClass]().decode(json)
      decoded.shouldSucceed shouldBe MyClass(
        31,
        Some(JSource(json, Map("b" -> json.field("b").shouldSucceed)))
      )
    }

    it("should not inject source into a wrongly-typed field") {
      case class MyClass(b: Int, @Source src: String)

      val json = json"""{"b": 31}""".asRootFocus

      assertTypeError("""
        semiauto.unchecked.deriveDecoder[MyClass].decode(json)
      """)
    }

    it("should fail to compile unannotated source field") {
      case class MyClass(b: Int, src: JSource)

      assertTypeError(
        """semiauto.unchecked.deriveDecoderForCaseClass[MyClass]()"""
      )
    }
  }

  it("should ignore discriminator collisions on decoding") {
    import org.scalawag.bateman.json.generic.auto._
    import test.json.generic.decoding.DerivedDecoderTest.DiscriminatorCollision._

    json"""{"type": "Y","a": 8, "b": 12 }""".asRootFocus.decode[Z].shouldSucceed shouldBe Y(8, 12, "Y")
  }

  describe("config override") {
    import DerivedDecoderTest.LongNames._

    implicit val ydec: JObjectDecoder[YNamedClass] =
      semiauto.unchecked.deriveDecoderForCaseClass[YNamedClass]()
    implicit val zdec: JObjectDecoder[ZNamedClass] =
      semiauto.unchecked.deriveDecoderForCaseClass[ZNamedClass]()

    val json = json"""{"type": "???"}""".asRootFocus

    it("should replace the implicit config") {
      implicit val xdec: JObjectDecoder[XNamedClass] =
        semiauto.unchecked.deriveDecoderForTrait[XNamedClass](
          config = (_: Config).copy(classNameMapping = PascalCase to SnakeCase)
        )

      json.decode[XNamedClass].shouldFailSingle shouldBe InvalidDiscriminator(
        json.field("type").flatMap(_.asString).shouldSucceed,
        Set(JString("y_named_class"), JString("z_named_class"))
      )
    }

    it("should use the passed-in config") {
      implicit val xdec: JObjectDecoder[XNamedClass] =
        semiauto.unchecked.deriveDecoderForTrait[XNamedClass](
          config = Config(classNameMapping = PascalCase to SnakeCase)
        )

      json.decode[XNamedClass].shouldFailSingle shouldBe InvalidDiscriminator(
        json.field("type").flatMap(_.asString).shouldSucceed,
        Set(JString("y_named_class"), JString("z_named_class"))
      )
    }
  }

  describe("custom discriminators") {

    it("should fail with default discriminators") {
      import DerivedDecoderTest.Nested._

      val json = json"""{"type": "x", "a": 31}""".asRootFocus

      implicit val ydec: JObjectDecoder[Y.Inner] = semiauto.unchecked.deriveDecoderForCaseClass[Y.Inner]()
      implicit val zdec: JObjectDecoder[Z.Inner] = semiauto.unchecked.deriveDecoderForCaseClass[Z.Inner]()
      val ex = intercept[ProgrammerError] {
        semiauto.unchecked.deriveDecoderForTrait[X.Inner]()
      }
      ex.description should include("Inner")
    }

    it("should succeed with custom discriminators") {
      import DerivedDecoderTest.Nested._

      val json = json"""{"class": "z", "a": 31}""".asRootFocus

      implicit val ydec: JObjectDecoder[Y.Inner] = semiauto.unchecked.deriveDecoderForCaseClass[Y.Inner]()
      implicit val zdec: JObjectDecoder[Z.Inner] = semiauto.unchecked.deriveDecoderForCaseClass[Z.Inner]()
      implicit val xdec: JObjectDecoder[X.Inner] = semiauto.unchecked.deriveDecoderForTrait[X.Inner](
        "class",
        CustomDiscriminator(
          forType[Y.Inner]("y"),
          forType[Z.Inner]("z"),
        )
      )
      val da = JObjectDecoder[X.Inner].decode(json)
      da shouldBe Z.Inner(31).rightNec
    }
  }
}

object DerivedDecoderTest {
  object Default {
    sealed trait Z
    case class X(a: Int, b: String = "jack", c: Boolean = false) extends Z
    case class Y(a: Int, b: Int) extends Z
  }

  object LongNames {
    sealed trait XNamedClass
    case class YNamedClass(longerName: String) extends XNamedClass
    case class ZNamedClass(b: Int = 8) extends XNamedClass
  }

  object Nested {
    object X {
      sealed trait Inner
    }
    object Y {
      case class Inner(b: Int) extends X.Inner
    }
    object Z {
      case class Inner(a: Int) extends X.Inner
    }
  }

  object DiscriminatorCollision {
    sealed trait Z
    case class X(a: Int, b: String, c: Boolean) extends Z
    case class Y(a: Int, b: Int, `type`: String) extends Z
  }
}
