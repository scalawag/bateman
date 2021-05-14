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

import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, SnakeCase}
import org.scalawag.bateman.json.encoding.{Encoder, JBoolean, JNumber, JObject, JObjectEncoder, JString}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ProgrammerError
import org.scalawag.bateman.json.decoding.JField
import org.scalawag.bateman.json.generic.encoding.{CaseClassEncoder, TraitEncoder}
import shapeless.tag.@@

import java.net.InetAddress

class DerivedEncoderTest extends AnyFunSpec with Matchers {

  it("should encode concretely") {
    import DerivedEncoderTest.Default._

    val x = X(8, "jack", false)

//    import auto._

    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x) shouldEqual JObject(
      "a" -> JNumber(8),
      "b" -> JString("jack"),
      "c" -> JBoolean(false)
    )
  }

  it("should semiauto very implicitly") {
    import DerivedEncoderTest.Default._

    val x = X(8, "jack", false)

    import org.scalawag.bateman.json.generic.semiauto._

    implicit val enc: Encoder[X, JObject] = deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x) shouldEqual JObject(
      "a" -> JNumber(8),
      "b" -> JString("jack"),
      "c" -> JBoolean(false)
    )
  }

  it("should semiauto more explicitly") {
    import DerivedEncoderTest.Default._

    val x = X(8, "jack", false)

    import org.scalawag.bateman.json.generic.semiauto._

    implicit val enc = deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x) shouldEqual JObject(
      "a" -> JNumber(8),
      "b" -> JString("jack"),
      "c" -> JBoolean(false)
    )
  }

  it("should semiauto from companion object with config") {
    case class X(myLongFieldName: Int)
    object X {
      import org.scalawag.bateman.json.generic.semiauto._
      private implicit val config = Config(fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
      implicit val encoder: Encoder[X, JObject] = deriveEncoderForCaseClass[X]()
    }

    val x = X(8)

    Encoder[X, JObject].encode(x) shouldEqual JObject(
      "my_long_field_name" -> JNumber(8)
    )
  }

  it("should encode abstractly") {
    import DerivedEncoderTest.Default._

    val x = X(8, "jack", false)

//    import auto._

    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    implicit val yenc = semiauto.deriveEncoderForCaseClass[Y]()
    implicit val zenc = semiauto.deriveEncoderForTrait[Z]()

    Encoder[Z, JObject].encode(x) shouldEqual JObject(
      "type" -> JString("X"),
      "a" -> JNumber(8),
      "b" -> JString("jack"),
      "c" -> JBoolean(false)
    )
  }

  it("should encode abstractly again") {
    import DerivedEncoderTest.Default._

    val x = Y(2, 1)

//    import auto._

    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    implicit val yenc = semiauto.deriveEncoderForCaseClass[Y]()
    implicit val zenc = semiauto.deriveEncoderForTrait[Z]()

    Encoder[Z, JObject].encode(x) shouldEqual JObject(
      "type" -> JString("Y"),
      "a" -> JNumber(2),
      "b" -> JNumber(1)
    )
  }

  it("should use field name transformations") {
    case class X(myLongFieldName: Int)
    val x = X(8)

//    import auto._

    implicit val config = Config(fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x) shouldEqual JObject(
      "my_long_field_name" -> JNumber(8)
    )
  }

  it("should use class name transformations") {
    import DerivedEncoderTest.LongNames._

    val x = MyLongClassName(8)

//    import auto._

    implicit val config = Config(classNameMapping = CaseTransformation(CamelCase, SnakeCase))
    implicit val cenc = semiauto.deriveEncoderForCaseClass[MyLongClassName]()
    implicit val tenc = semiauto.deriveEncoderForTrait[MyTrait]()
    Encoder[MyTrait, JObject].encode(x) shouldEqual JObject(
      "type" -> JString("my_long_class_name"),
      "myLongFieldName" -> JNumber(8)
    )
  }

  // TODO: make this fail to compile
  it("should check for name collisions with discriminator") {
    sealed trait A
    final case class B(`type`: Int) extends A
    val b = B(8)

    import auto._

    implicit val benc = semiauto.deriveEncoderForCaseClass[B]()
//    implicit val aenc = semiauto.deriveEncoderForTrait[A]()
//
//    val e = intercept[DiscriminatorFieldCollision] {
//      Encoder[A, JObject].encode(b)
//    }
//    e shouldBe DiscriminatorFieldCollision("type", "B")
  }

  it("should not encode fields with their default values") {
    final case class X(a: Int = 8, b: Int = 7, c: String = "blah")
    val x = X(b = 7)

//    import auto._
    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x) shouldEqual JObject()
  }

  it("should encode fields with their default values when requested") {
    final case class X(a: Int = 8, b: Int = 7, c: String = "blah")
    val x = X()

    import auto._

    implicit val config = Config(encodeDefaultValues = true)
//    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x).render shouldEqual JObject(
      "a" -> JNumber(8),
      "b" -> JNumber(7),
      "c" -> JString("blah")
    ).render
  }

  it("should preserve field ordering for encoding") {
    final case class X(b: Int = 7, c: Int, a: Int = 8)
    val x = X(a = 17, c = 11)

    import auto._

//    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x).render shouldEqual JObject(
      "c" -> JNumber(11),
      "a" -> JNumber(17)
    ).render
  }

  it("should ignore source-tagged fields for encoding") {
    final case class X(a: Int = 8, src: Option[JObject] @@ SourceTag = None)
    val x = X(17, Some(JObject()))

    import auto._

//    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x).render shouldEqual JObject(
      "a" -> JNumber(17),
    ).render
  }

  it("should ignore source-tagged fields even if they're inappropriate types") {
    final case class X(a: Int = 8, src: InetAddress @@ SourceTag)
    val x = X(17, InetAddress.getLocalHost)

    import auto._

//    implicit val xenc = semiauto.deriveEncoderForCaseClass[X]()
    Encoder[X, JObject].encode(x).render shouldEqual JObject(
      "a" -> JNumber(17),
    ).render
  }

  it("should fail to derive an encoder when a field type is not supported") {
    final case class X(a: Int, b: Boolean, c: String, d: java.net.InetAddress)
    assertTypeError("""
      import semiauto._
      deriveEncoder[X]()
    """)
  }

  describe("discriminator collisions") {
    sealed trait Z
    case class X(a: Int, b: String, c: Boolean) extends Z
    case class Y(a: Int, b: Int, `type`: Int) extends Z

    it("should fail to compile when there's a collision with the discriminator field") {
      import org.scalawag.bateman.json.generic.semiauto._
      assertTypeError("""
        deriveEncoder[Z]
      """)
    }

    it("should fail to compile when there's a collision with a non-default discriminator field") {
      import org.scalawag.bateman.json.generic.semiauto._
      implicit val config: Config = Config.default.copy(discriminatorField = "a")
      assertTypeError("""
        deriveEncoder[Z]
      """)
    }

    it("should compile if it's not an abstract encoder (so doesn't need a discriminator)") {
      import org.scalawag.bateman.json.generic.semiauto._
      semiauto.deriveEncoderForCaseClass[Y]()
    }

    it("should compile if the discriminator is changed to avoid a collision") {
      import org.scalawag.bateman.json.generic.semiauto._
      implicit val config: Config = Config.default.copy(discriminatorField = "d")
      semiauto.deriveEncoderForCaseClass[Y]()
    }
  }

  describe("custom discriminators") {

    it("should fail with default discriminators") {
      import DataTypes.Nested._

      val y = Y.Inner(31)

      implicit val yenc: CaseClassEncoder[Y.Inner] = semiauto.deriveEncoderForCaseClass[Y.Inner]()
      implicit val zenc: CaseClassEncoder[Z.Inner] = semiauto.deriveEncoderForCaseClass[Z.Inner]()
      val ex = intercept[ProgrammerError] {
        semiauto.deriveEncoderForTrait[X.Inner]()
      }
      ex.description should include("Inner")
    }

    it("should succeed with custom discriminators") {
      import DataTypes.Nested._

      val y = Y.Inner(31)

      implicit val yenc: CaseClassEncoder[Y.Inner] = semiauto.deriveEncoderForCaseClass[Y.Inner]("y")
      implicit val zenc: CaseClassEncoder[Z.Inner] = semiauto.deriveEncoderForCaseClass[Z.Inner]("z")
      implicit val xenc: TraitEncoder[X.Inner] = semiauto.deriveEncoderForTrait[X.Inner]("class")
      val da = JObjectEncoder[X.Inner].encode(y)
      da shouldBe JObject("class" -> JString("y"), "b" -> JNumber(31))
    }
  }
}

object DerivedEncoderTest {
  object Default {
    sealed trait Z
    case class X(a: Int, b: String, c: Boolean) extends Z
    case class Y(a: Int, b: Int) extends Z
  }

  object LongNames {
    sealed trait MyTrait
    final case class MyLongClassName(myLongFieldName: Int) extends MyTrait
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
