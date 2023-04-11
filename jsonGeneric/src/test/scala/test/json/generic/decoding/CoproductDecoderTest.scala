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

import org.scalawag.bateman.json.generic.Discriminators._
import org.scalawag.bateman.json.generic.decoding.InvalidDiscriminator
import org.scalawag.bateman.json.generic.naming.{PascalCase, SnakeCase}
import org.scalawag.bateman.json.generic.{
  Config,
  DiscriminatorCollision,
  MissingDiscriminatorMapping,
  MultipleDiscriminatorMappings
}
import org.scalawag.bateman.json.literal.JsonStringContext
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{JBoolean, JNumber, MissingField}

import scala.reflect.classTag

object CoproductDecoderTest {
  sealed trait X
  final case class Y(a: Int) extends X
  final case class Z(a: String) extends X

  final case class Bogus(x: String)
}

import test.json.generic.decoding.CoproductDecoderTest._

class CoproductDecoderTest extends DecoderTestBase {
  describe("default config") {
    import org.scalawag.bateman.json.generic.auto._

    it("should decode abstractly") {
      json"""{"type":"Y","a":71}""".asRootFocus.decode[X].shouldSucceed shouldBe Y(71)
    }

    it("should reject with no discriminator") {
      val f = json"""{"a":71}""".asRootFocus
      f.decode[X].shouldFailSingle shouldBe MissingField(f, "type")
    }

    it("should reject if missing a field of the concrete decoder") {
      val f = json"""{"type":"Y"}""".asRootFocus
      f.decode[X].shouldFailSingle shouldBe MissingField(f, "a")
    }
  }

  describe("with case transformation") {
    import org.scalawag.bateman.json.generic.auto._
    implicit val config = Config(classNameMapping = PascalCase to SnakeCase)

    it("should decode abstractly") {
      json"""{"type":"y","a":71}""".asRootFocus.decode[X].shouldSucceed shouldBe Y(71)
    }
  }

  describe("with custom discriminator name") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val ydec = deriveDecoderForCaseClass[Y]()
    implicit val zdec = deriveDecoderForCaseClass[Z]()
    implicit val xdec = deriveDecoderForTrait[X]("ilk")

    it("should decode abstractly") {
      json"""{"ilk":"Y","a":71}""".asRootFocus.decode[X].shouldSucceed shouldBe Y(71)
    }
  }

  describe("with custom discriminator mapping") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._

    implicit val bdec = deriveDecoderForCaseClass[Bogus]()
    implicit val ydec = deriveDecoderForCaseClass[Y]()
    implicit val zdec = deriveDecoderForCaseClass[Z]()
    implicit val xdec = deriveDecoderForTrait[X](discriminator =
      CustomDiscriminator(
        forType[Y](1),
        forType[Z](true),
      )
    )

    it("should decode a Y") {
      json"""{"type":1,"a":71}""".asRootFocus.decode[X].shouldSucceed shouldBe Y(71)
    }

    it("should decode a Z") {
      json"""{"type":true,"a":"quux"}""".asRootFocus.decode[X].shouldSucceed shouldBe Z("quux")
    }

    it("should reject discriminator value") {
      implicit val xdec = deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y](1),
          forType[Z](true),
          forType[Bogus]("havoc"),
          forType[Bogus]("bedlam"),
        )
      )

      val f = json"""{"type":"foo","a":71}""".asRootFocus
      f.decode[X].shouldFailSingle shouldBe InvalidDiscriminator(
        f.field("type").shouldSucceed,
        // Note: this list should not list the unused (Bogus) nor care about the duplicate
        Set(JBoolean(true), JNumber(1))
      )
    }

    it("should reject duplicate discriminator types in mappings") {
      intercept[MultipleDiscriminatorMappings[Y]] {
        deriveDecoderForTrait[X](discriminator =
          CustomDiscriminator(
            forType[Y](1),
            forType[Y](false),
            forType[Z](true),
          )
        )
      } shouldBe MultipleDiscriminatorMappings[Y](
        List(JNumber(1), JBoolean(false))
      )
    }

    it("should reject duplicate discriminator values") {
      intercept[DiscriminatorCollision] {
        deriveDecoderForTrait[X](discriminator =
          CustomDiscriminator(
            forType[Y](1),
            forType[Z](1),
          )
        )
      } shouldBe DiscriminatorCollision(Map(JNumber(1) -> List(classTag[Y], classTag[Z])))
    }

    it("should allow duplicate discriminators when told") {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(duplicateValuesForbidden = false)(
          forType[Y](1),
          forType[Z](1),
        )
      )
      succeed
    }
  }

  it("should detect duplicate discriminator values") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val ydec = deriveDecoderForCaseClass[Y]()
    implicit val zdec = deriveDecoderForCaseClass[Z]()

    val ex = intercept[DiscriminatorCollision] {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y](1),
          forType[Z](1),
        )
      )
    }

    ex.discriminators shouldBe Map(1.toJAny -> List(classTag[Y], classTag[Z]))
  }

  it("should detect missing discriminator mappings") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val ydec = deriveDecoderForCaseClass[Y]()
    implicit val zdec = deriveDecoderForCaseClass[Z]()

    val ex = intercept[MissingDiscriminatorMapping[_]] {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y](1)
        )
      )
    }

    ex.forType shouldBe classTag[Z]
  }

  it("should detect duplicate discriminator mappings") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val ydec = deriveDecoderForCaseClass[Y]()
    implicit val zdec = deriveDecoderForCaseClass[Z]()

    val ex = intercept[MultipleDiscriminatorMappings[_]] {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y](1),
          forType[Y](2),
          forType[Z](1),
        )
      )
    }

    ex.forType shouldBe classTag[Y]
    ex.values shouldBe List(1.toJAny, 2.toJAny)
  }
}
