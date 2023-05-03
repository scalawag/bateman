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
import org.scalawag.bateman.json.{JBoolean, JNumber, MissingField, ProgrammerError, lens}
import test.json.BatemanTestBase

import scala.reflect.classTag

object CoproductEncoderTest {
  sealed trait X
  final case class Y(a: Int) extends X
  final case class Z(a: String) extends X

  final case class Bogus(x: String)

  sealed trait Collide
  final case class CollideA(foo: String) extends Collide
}

import test.json.generic.encoding.CoproductEncoderTest._

class CoproductEncoderTest extends BatemanTestBase {
  describe("default config") {
    import org.scalawag.bateman.json.generic.auto._

    it("should encode abstractly") {
      (Y(71): X).toJAny shouldBe json"""{"type":"Y","a":71}""".stripLocation
    }
  }

  describe("with case transformation") {
    import org.scalawag.bateman.json.generic.auto._
    implicit val config = Config(classNameMapping = PascalCase to SnakeCase)

    it("should encode abstractly") {
      (Y(71): X).toJAny shouldBe json"""{"type":"y","a":71}""".stripLocation
    }
  }

  describe("with custom discriminator name") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val yenc = deriveEncoderForCaseClass[Y]()
    implicit val zenc = deriveEncoderForCaseClass[Z]()
    implicit val xenc = deriveEncoderForTrait[X]("ilk")

    it("should encode abstractly") {
      (Y(71): X).toJAny shouldBe json"""{"ilk":"Y","a":71}""".stripLocation
    }
  }

  describe("with custom discriminator mapping") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._

    implicit val benc = deriveEncoderForCaseClass[Bogus]()
    implicit val yenc = deriveEncoderForCaseClass[Y]()
    implicit val zenc = deriveEncoderForCaseClass[Z]()
    implicit val xenc = deriveEncoderForTrait[X](discriminator =
      CustomDiscriminator(
        forType[Y](1),
        forType[Z](true),
      )
    )

    it("should encode a Y") {
      (Y(71): X).toJAny shouldBe json"""{"type":1,"a":71}""".stripLocation
    }

    it("should encode a Z") {
      (Z("quux"): X).toJAny shouldBe json"""{"type":true,"a":"quux"}""".stripLocation
    }

    it("should reject duplicate discriminator types in mappings") {
      intercept[MultipleDiscriminatorMappings[Y]] {
        deriveEncoderForTrait[X](discriminator =
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
        deriveEncoderForTrait[X](discriminator =
          CustomDiscriminator(
            forType[Y](1),
            forType[Z](1),
          )
        )
      } shouldBe DiscriminatorCollision(Map(JNumber(1) -> List(classTag[Y], classTag[Z])))
    }

    it("should allow duplicate discriminators when told") {
      deriveEncoderForTrait[X](discriminator =
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
    implicit val yenc = deriveEncoderForCaseClass[Y]()
    implicit val zenc = deriveEncoderForCaseClass[Z]()

    val ex = intercept[DiscriminatorCollision] {
      deriveEncoderForTrait[X](discriminator =
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
    implicit val yenc = deriveEncoderForCaseClass[Y]()
    implicit val zenc = deriveEncoderForCaseClass[Z]()

    val ex = intercept[MissingDiscriminatorMapping[_]] {
      deriveEncoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y](1)
        )
      )
    }

    ex.forType shouldBe classTag[Z]
  }

  it("should detect duplicate discriminator mappings") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val yenc = deriveEncoderForCaseClass[Y]()
    implicit val zenc = deriveEncoderForCaseClass[Z]()

    val ex = intercept[MultipleDiscriminatorMappings[_]] {
      deriveEncoderForTrait[X](discriminator =
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

  it("should throw on discriminator field collisions") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val aenc = deriveEncoderForCaseClass[CollideA]()
    implicit val enc = deriveEncoderForTrait[Collide]("foo")

    intercept[ProgrammerError] {
      (CollideA("bar"): Collide).toJAny
    }
  }

  it("should ignore benign collisions") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val aenc = deriveEncoderForCaseClass[CollideA]()
    implicit val enc = deriveEncoderForTrait[Collide]("foo")

    (CollideA("CollideA"): Collide).toJAny shouldBe json"""{"foo": "CollideA"}""".stripLocation
  }

  it("should throw on focus discriminators") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    implicit val aenc = deriveEncoderForCaseClass[CollideA]()
    implicit val enc = deriveEncoderForTrait[Collide](lens.focus)

    intercept[ProgrammerError] {
      (CollideA("bar"): Collide).toJAny
    }
  }
}
