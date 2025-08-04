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

package test.json.generic.decoding

import org.scalawag.bateman.json.generic.decoding.{InvalidDiscriminator, JSource}
import org.scalawag.bateman.json.generic.naming.{PascalCase, SnakeCase}
import org.scalawag.bateman.json.generic.{Config, Source, semiauto}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.{JObjectDecoder, JString}
import test.json.BatemanTestBase

/** Tests that require Scala 2-only features (assertTypeError, DeriverConfigMagnet). */
class DerivedDecoderScala2Test extends BatemanTestBase {

  describe("source annotation (compile-time)") {
    it("should not inject source into a wrongly-typed field") {
      case class MyClass(b: Int, @Source src: String)

      val json = json"""{"b": 31}""".asRootFocus

      assertTypeError("""
        semiauto.deriveDecoder[MyClass].decode(json)
      """)
    }

    it("should fail to compile unannotated source field") {
      case class MyClass(b: Int, src: JSource)

      assertTypeError(
        """semiauto.deriveDecoderForCaseClass[MyClass]()"""
      )
    }
  }

  describe("config override") {
    import DerivedDecoderTest.LongNames._

    implicit val ydec: JObjectDecoder[YNamedClass] =
      semiauto.deriveDecoderForCaseClass[YNamedClass]()
    implicit val zdec: JObjectDecoder[ZNamedClass] =
      semiauto.deriveDecoderForCaseClass[ZNamedClass]()

    val json = json"""{"type": "???"}""".asRootFocus

    it("should replace the implicit config") {
      implicit val xdec: JObjectDecoder[XNamedClass] =
        semiauto.deriveDecoderForTrait[XNamedClass](
          config = (_: Config).copy(classNameMapping = PascalCase to SnakeCase)
        )

      json.decode[XNamedClass].shouldFailSingle shouldBe InvalidDiscriminator(
        json.field("type").flatMap(_.asString).shouldSucceed,
        Set(JString("y_named_class"), JString("z_named_class"))
      )
    }

    it("should use the passed-in config") {
      implicit val xdec: JObjectDecoder[XNamedClass] =
        semiauto.deriveDecoderForTrait[XNamedClass](
          config = Config(classNameMapping = PascalCase to SnakeCase)
        )

      json.decode[XNamedClass].shouldFailSingle shouldBe InvalidDiscriminator(
        json.field("type").flatMap(_.asString).shouldSucceed,
        Set(JString("y_named_class"), JString("z_named_class"))
      )
    }
  }
}
