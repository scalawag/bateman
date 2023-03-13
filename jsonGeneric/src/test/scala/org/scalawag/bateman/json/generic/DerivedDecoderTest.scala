// bateman -- Copyright 2021 -- Justin Patterson
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

package org.scalawag.bateman.json.generic

import cats.data.NonEmptyChain
import cats.syntax.validated._
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, KebabCase, PascalCase, SnakeCase}
import org.scalawag.bateman.json.decoding.{
  JNumber,
  JObject,
  JObjectDecoder,
  JPointer,
  JString,
  JsonTypeMismatch,
  UnexpectedValue,
  UnspecifiedField
}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.generic.decoding.{CaseClassDecoder, InvalidDiscriminator, JSource, TraitDecoder}
import org.scalawag.bateman.json.{ParserTestUtils, ProgrammerError}
import shapeless.tag.@@

class DerivedDecoderTest extends AnyFunSpec with Matchers with ParserTestUtils {

  it("should decode perfectly") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7, "b": "XXX", "c": true}""")

//    import auto._

    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", true).validNec
  }

  it("should decode perfectly with auto") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7, "b": "XXX", "c": true}""")

    import auto._

    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", true).validNec
  }

  it("should fail on mismatched type") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": "7", "b": "XXX", "c": true}""")

    //    import auto._

    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe JsonTypeMismatch(json.fieldList.head.value, JNumber).invalidNec
  }

  it("should fail on missing field") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"b": "XXX", "c": true}""")

    //    import auto._

    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe UnspecifiedField(json, "a").invalidNec
  }

  it("should decode missing fields with defaults") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7}""")

    //    import auto._

    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "jack", false).validNec
  }

  it("should decode a mixture of missing (but defaulted) fields") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7, "b": "XXX"}""")

    //    import auto._

    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", false).validNec
  }

  it("should fail to decode fields with defaults when configured not to use them") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7, "c": true}""")

    //    import auto._

    implicit val config = Config(useDefaultsForMissingFields = false)
    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe UnspecifiedField(json, "b").invalidNec
  }

  it("should ignore extra fields") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7, "b": "XXX", "c": true, "d": {}}""")

    //    import auto._

    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe X(7, "XXX", true).validNec
  }

  it("should fail on extra fields when configured to") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7, "b": "XXX", "d": 5.67}""")

    //    import auto._

    implicit val config: Config = Config(allowUnknownFields = false)
    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe UnexpectedValue(json("d").getOrElse(???)).invalidNec
  }

  it("should fail on extra fields when configured to (with field name mapping)") {
    import DataTypes.LongNames._

    val json = parseAs[JObject]("""{"longer_name": "7", "d": 5.67}""")

    //    import auto._

    implicit val config: Config =
      Config(allowUnknownFields = false, fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
    implicit val dec: CaseClassDecoder[YNamedClass, Any] = semiauto.deriveDecoderForCaseClass[YNamedClass, Any]()
    val da = JObjectDecoder[YNamedClass].decode(json)
    da shouldBe UnexpectedValue(json("d").getOrElse(???)).invalidNec
  }

  it("should require a discriminator for trait decoding") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"a": 7}""")

    //    import auto._

    implicit val xdec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    implicit val ydec: CaseClassDecoder[Y, Any] = semiauto.deriveDecoderForCaseClass[Y, Any]()
    implicit val dec: TraitDecoder[Z, Any] = semiauto.deriveDecoderForTrait[Z, Any]()
    val da = JObjectDecoder[Z].decode(json)
    da shouldBe UnspecifiedField(json, "type").invalidNec
  }

  it("should decode trait with discriminator") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"type": "X", "a": 7}""")

    //    import auto._

    implicit val xdec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    implicit val ydec: CaseClassDecoder[Y, Any] = semiauto.deriveDecoderForCaseClass[Y, Any]()
    implicit val dec: TraitDecoder[Z, Any] = semiauto.deriveDecoderForTrait[Z, Any]()
    val da = JObjectDecoder[Z].decode(json)
    da shouldBe X(7, "jack", false).validNec
  }

  it("should fail to decode if missing the concrete fields of the discriminated type") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"type": "X"}""")

    //    import auto._

    implicit val xdec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    implicit val ydec: CaseClassDecoder[Y, Any] = semiauto.deriveDecoderForCaseClass[Y, Any]()
    implicit val dec: TraitDecoder[Z, Any] = semiauto.deriveDecoderForTrait[Z, Any]()
    val da = JObjectDecoder[Z].decode(json)
    // The discriminator field is stripped from the object by the abstract decoder before the concrete decoder has a go.
    da shouldBe UnspecifiedField(json, "a").invalidNec
  }

  it("should use name transformation for fields") {
    case class X(longerName: String)

    val json = parseAs[JObject]("""{"longer_name": "XXX"}""")

    //    import auto._

    implicit val config: Config = Config(fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
    implicit val dec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    val da = JObjectDecoder[X].decode(json)
    da shouldBe X("XXX").validNec
  }

  it("should use name transformation for classes") {
    import DataTypes.LongNames._

    val json = parseAs[JObject]("""{"type": "z_named_class"}""")

//    import auto._

    implicit val config: Config = Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
    implicit val ydec: CaseClassDecoder[YNamedClass, Any] =
      semiauto.deriveDecoderForCaseClass[YNamedClass, Any]()
    implicit val zdec: CaseClassDecoder[ZNamedClass, Any] =
      semiauto.deriveDecoderForCaseClass[ZNamedClass, Any]()
    implicit val xdec: TraitDecoder[XNamedClass, Any] = semiauto.deriveDecoderForTrait[XNamedClass, Any]()
    val da = JObjectDecoder[XNamedClass].decode(json)
    da shouldBe ZNamedClass().validNec
  }

  it("should use configured discriminator field") {
    import DataTypes.Default._

    val json = parseAs[JObject]("""{"foo": "X", "a": 7}""")

//    import auto._

    implicit val config: Config = Config(discriminatorField = "foo")
    implicit val xdec: CaseClassDecoder[X, Any] = semiauto.deriveDecoderForCaseClass[X, Any]()
    implicit val ydec: CaseClassDecoder[Y, Any] = semiauto.deriveDecoderForCaseClass[Y, Any]()
    implicit val dec: TraitDecoder[Z, Any] = semiauto.deriveDecoderForTrait[Z, Any]()
    val da = JObjectDecoder[Z].decode(json)
    da shouldBe X(7, "jack", false).validNec
  }

  it("should detect invalid discriminator") {
    val json = parseAs[JObject]("""{"type": "x_named_class"}""")

    import DataTypes.LongNames._
//    import auto._

    implicit val config: Config = Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
    implicit val ydec: CaseClassDecoder[YNamedClass, Any] =
      semiauto.deriveDecoderForCaseClass[YNamedClass, Any]()
    implicit val zdec: CaseClassDecoder[ZNamedClass, Any] =
      semiauto.deriveDecoderForCaseClass[ZNamedClass, Any]()
    implicit val xdec: TraitDecoder[XNamedClass, Any] = semiauto.deriveDecoderForTrait[XNamedClass, Any]()
    val da = JObjectDecoder[XNamedClass].decode(json)
    da shouldBe InvalidDiscriminator(
      json.apply("type").andThen(_.asString).getOrElse(???),
      Iterable("y_named_class", "z_named_class")
    ).invalidNec
  }

  object Data {
    import org.scalawag.bateman.json.generic.semiauto._

    sealed trait XNamedClass
    case class YNamedClass(longerName: String) extends XNamedClass
    case class ZNamedClass(b: Int = 8) extends XNamedClass

    object XNamedClass {
      implicit def xdec: TraitDecoder[XNamedClass, Any] = deriveDecoderForTrait[XNamedClass, Any]()
    }
    object YNamedClass {
      implicit def ydec: CaseClassDecoder[YNamedClass, Any] = deriveDecoderForCaseClass[YNamedClass, Any]()
    }
    object ZNamedClass {
      implicit def zdec: CaseClassDecoder[ZNamedClass, Any] = deriveDecoderForCaseClass[ZNamedClass, Any]()
    }
  }

  it("should detect invalid discriminator type") {
    import Data._

    val json = parseAs[JObject]("""{"type": 12}""")

    implicit val config: Config = Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
    val da = JObjectDecoder[XNamedClass].decode(json)
    da shouldBe JsonTypeMismatch(json.apply("type").getOrElse(???), JString).invalidNec
  }

  describe("source tag") {
    it("should inject source into tagged case class field") {
      case class MyClass(b: Int, src: JSource @@ SourceTag)

      val json = parseAs[JObject]("""{"b": 31}""")

      val decoded = semiauto.deriveDecoderForCaseClass[MyClass, Any]().decode(json)
      decoded.shouldSucceed shouldBe MyClass(31, JSource(json, Map("b" -> JPointer.Root / "b", "src" -> JPointer.Root)))
    }

    it("should inject source into optional tagged case class field") {
      case class MyClass(b: Int, src: Option[JSource] @@ SourceTag = None)

      val json = parseAs[JObject]("""{"b": 31}""")

      val decoded = semiauto.deriveDecoderForCaseClass[MyClass, Any]().decode(json)
      decoded.shouldSucceed shouldBe MyClass(
        31,
        Some(JSource(json, Map("b" -> JPointer.Root / "b", "src" -> JPointer.Root)))
      )
    }

    it("should not inject source into a wrongly-typed field") {
      case class MyClass(b: Int, src: String @@ SourceTag)

      val json = parseAs[JObject]("""{"b": 31}""")

      assertTypeError("""
        semiauto.deriveDecoder[MyClass, Any].decode(json)
      """)
    }

    it("should not insert into untagged fields (even if they're absent and the correct type for it)") {
      case class MyClass(b: Int, src: Option[JObject])

      val json = parseAs[JObject]("""{"b": 31}""")

      val da = semiauto.deriveDecoderForCaseClass[MyClass, Any]().decode(json)
      da shouldBe UnspecifiedField(json, "src").invalidNec
    }
  }

  describe("discriminator collisions") {
    sealed trait Z
    case class X(a: Int, b: String, c: Boolean) extends Z
    case class Y(a: Int, b: Int, `type`: Int) extends Z

    it("should fail to compile when there's a collision with the discriminator field") {
      import org.scalawag.bateman.json.generic.semiauto._
      assertTypeError("""
        deriveDecoder[Z, Any]
      """)
    }

    it("should fail to compile when there's a collision with a non-default discriminator field") {
      import org.scalawag.bateman.json.generic.semiauto._
      implicit val config: Config = Config.default.copy(discriminatorField = "a")
      assertTypeError("""
        deriveDecoder[Z, Any]
      """)
    }

    it("should compile if it's not an abstract encoder (so doesn't need a discriminator)") {
      import org.scalawag.bateman.json.generic.semiauto._
      deriveDecoderForCaseClass[Y, Any]
    }

    it("should compile if the discriminator is changed to avoid a collision") {
      import org.scalawag.bateman.json.generic.semiauto._
      implicit val config: Config = Config.default.copy(discriminatorField = "d")
      deriveDecoderForCaseClass[Y, Any]
    }
  }

  describe("config override") {

    it("should use the passed-in config") {
      import DataTypes.LongNames._

      implicit val ydec: CaseClassDecoder[YNamedClass, Any] =
        semiauto.deriveDecoderForCaseClass[YNamedClass, Any](config =
          Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
        )
      implicit val zdec: CaseClassDecoder[ZNamedClass, Any] =
        semiauto.deriveDecoderForCaseClass[ZNamedClass, Any](config =
          Config(classNameMapping = CaseTransformation(PascalCase, KebabCase))
        )
      implicit val xdec: TraitDecoder[XNamedClass, Any] = semiauto.deriveDecoderForTrait[XNamedClass, Any]("class")

      val json = parseAs[JObject]("""{"class": "???"}""")

      val da = JObjectDecoder[XNamedClass].decode(json)
      da shouldBe InvalidDiscriminator(
        json("class").andThen(_.asString).getOrElse(fail()),
        Iterable("y_named_class", "z-named-class")
      ).invalidNec

    }
  }

  describe("custom discriminators") {

    it("should fail with default discriminators") {
      import DataTypes.Nested._

      val json = parseAs[JObject]("""{"type": "x", "a": 31}""")

      implicit val ydec: CaseClassDecoder[Y.Inner, Any] = semiauto.deriveDecoderForCaseClass[Y.Inner, Any]()
      implicit val zdec: CaseClassDecoder[Z.Inner, Any] = semiauto.deriveDecoderForCaseClass[Z.Inner, Any]()
      val ex = intercept[ProgrammerError] {
        semiauto.deriveDecoderForTrait[X.Inner, Any]()
      }
      ex.description should include("Inner")
    }

    it("should succeed with custom discriminators") {
      import DataTypes.Nested._

      val json = parseAs[JObject]("""{"class": "z", "a": 31}""")

      implicit val ydec: CaseClassDecoder[Y.Inner, Any] = semiauto.deriveDecoderForCaseClass[Y.Inner, Any]("y")
      implicit val zdec: CaseClassDecoder[Z.Inner, Any] = semiauto.deriveDecoderForCaseClass[Z.Inner, Any]("z")
      implicit val xdec: TraitDecoder[X.Inner, Any] = semiauto.deriveDecoderForTrait[X.Inner, Any]("class")
      val da = JObjectDecoder[X.Inner].decode(json)
      da shouldBe Z.Inner(31).validNec
    }
  }
}

object DataTypes {
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
}
