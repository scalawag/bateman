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

import org.scalawag.bateman.json.generic.Discriminators._
import org.scalawag.bateman.json.generic.decoding.InvalidDiscriminator
import org.scalawag.bateman.json.generic.naming.{PascalCase, SnakeCase}
import org.scalawag.bateman.json.generic.{
  Config,
  DiscriminatorCollision,
  MissingDiscriminatorMapping,
  MultipleDiscriminatorMappings
}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{JBoolean, JNumber, JObjectDecoder, JString, MissingField}
import org.scalawag.bateman.json.lens.stringToLens
import org.scalawag.bateman.json.generic.semiauto._

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
    implicit val ydec: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]
    implicit val zdec: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]
    implicit val xdec: JObjectDecoder[X] = deriveDecoderForTrait[X]()

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
    implicit val config: Config = Config(classNameMapping = PascalCase to SnakeCase)
    implicit val ydec: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]
    implicit val zdec: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]
    implicit val xdec: JObjectDecoder[X] = deriveDecoderForTrait[X]()

    it("should decode abstractly") {
      json"""{"type":"y","a":71}""".asRootFocus.decode[X].shouldSucceed shouldBe Y(71)
    }
  }

  describe("with custom discriminator name") {
    implicit val ydec: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]
    implicit val zdec: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]
    implicit val xdec: JObjectDecoder[X] = deriveDecoderForTrait[X]("ilk")

    it("should decode abstractly") {
      json"""{"ilk":"Y","a":71}""".asRootFocus.decode[X].shouldSucceed shouldBe Y(71)
    }
  }

  describe("with custom discriminator mapping") {
    implicit val bdec: JObjectDecoder[Bogus] = deriveDecoderForCaseClass[Bogus]
    implicit val ydec: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]
    implicit val zdec: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]
    implicit val xdec: JObjectDecoder[X] = deriveDecoderForTrait[X](discriminator =
      CustomDiscriminator(
        forType[Y].apply[JObjectDecoder, Int](1),
        forType[Z].apply[JObjectDecoder, Boolean](true),
      )
    )

    it("should decode a Y") {
      json"""{"type":1,"a":71}""".asRootFocus.decode[X].shouldSucceed shouldBe Y(71)
    }

    it("should decode a Z") {
      json"""{"type":true,"a":"quux"}""".asRootFocus.decode[X].shouldSucceed shouldBe Z("quux")
    }

    it("should reject discriminator value") {
      implicit val xdec: JObjectDecoder[X] = deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y].apply[JObjectDecoder, Int](1),
          forType[Z].apply[JObjectDecoder, Boolean](true),
          forType[Bogus].apply[JObjectDecoder, String]("havoc"),
          forType[Bogus].apply[JObjectDecoder, String]("bedlam"),
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
            forType[Y].apply[JObjectDecoder, Int](1),
            forType[Y].apply[JObjectDecoder, Boolean](false),
            forType[Z].apply[JObjectDecoder, Boolean](true),
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
            forType[Y].apply[JObjectDecoder, Int](1),
            forType[Z].apply[JObjectDecoder, Int](1),
          )
        )
      } shouldBe DiscriminatorCollision(Map(JNumber(1) -> List(classTag[Y], classTag[Z])))
    }

    it("should allow duplicate discriminators when told") {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(duplicateValuesForbidden = false)(
          forType[Y].apply[JObjectDecoder, Int](1),
          forType[Z].apply[JObjectDecoder, Int](1),
        )
      )
      succeed
    }
  }

  it("should detect duplicate discriminator values") {
    implicit val ydec: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]
    implicit val zdec: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]

    val ex = intercept[DiscriminatorCollision] {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y].apply[JObjectDecoder, Int](1),
          forType[Z].apply[JObjectDecoder, Int](1),
        )
      )
    }

    ex.discriminators shouldBe Map(1.toJAny -> List(classTag[Y], classTag[Z]))
  }

  it("should detect missing discriminator mappings") {
    implicit val ydec: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]
    implicit val zdec: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]

    val ex = intercept[MissingDiscriminatorMapping[_]] {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y].apply[JObjectDecoder, Int](1)
        )
      )
    }

    ex.forType shouldBe classTag[Z]
  }

  it("should detect duplicate discriminator mappings") {
    implicit val ydec: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]
    implicit val zdec: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]

    val ex = intercept[MultipleDiscriminatorMappings[_]] {
      deriveDecoderForTrait[X](discriminator =
        CustomDiscriminator(
          forType[Y].apply[JObjectDecoder, Int](1),
          forType[Y].apply[JObjectDecoder, Int](2),
          forType[Z].apply[JObjectDecoder, Int](1),
        )
      )
    }

    ex.forType shouldBe classTag[Y]
    ex.values shouldBe List(1.toJAny, 2.toJAny)
  }
}
