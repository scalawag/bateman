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

package test.json.generic

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.{Config, DiscriminatorCollision, MissingDiscriminatorMapping}
import org.scalawag.bateman.json.generic.Discriminators._
import test.json.BatemanTestBase

import scala.reflect.{ClassTag, classTag}

object DiscriminatorsTest {
  sealed trait Animal
  case class Dog(name: String) extends Animal
  case class Cat(name: String) extends Animal

  sealed trait Terminated extends Animal
  case class Failed(name: String) extends Terminated
}

class DiscriminatorsTest extends BatemanTestBase {
  import DiscriminatorsTest._

  implicit val config: Config = Config.default
  implicit val dogEncoder: JAnyEncoder[Dog] = (_: Dog) => JString("dog-encoded")
  implicit val catEncoder: JAnyEncoder[Cat] = (_: Cat) => JString("cat-encoded")

  describe("SimpleClassNameDiscriminator") {
    val disc = SimpleClassNameDiscriminator[JAnyEncoder]

    it("should derive discriminator value from class name") {
      val mapping = disc.apply[Dog]
      mapping.value.render shouldBe JString("Dog").render
    }

    it("should not provide an explicit instance") {
      val mapping = disc.apply[Dog]
      mapping.explicit shouldBe None
    }

    it("should forbid duplicate values") {
      disc.duplicateValuesForbidden shouldBe true
    }
  }

  describe("CustomDiscriminator") {
    it("should use the provided discriminator value") {
      val disc = CustomDiscriminator[JAnyEncoder, Animal](
        DiscriminatorMapper[JAnyEncoder, Dog](JString("canine")),
        DiscriminatorMapper[JAnyEncoder, Cat](JString("feline")),
      )
      val mapping = disc.apply[Dog]
      mapping.value.render shouldBe JString("canine").render
    }

    it("should provide an explicit instance") {
      val disc = CustomDiscriminator[JAnyEncoder, Animal](
        DiscriminatorMapper[JAnyEncoder, Dog](JString("canine")),
        DiscriminatorMapper[JAnyEncoder, Cat](JString("feline")),
      )
      val mapping = disc.apply[Dog]
      mapping.explicit shouldBe Some(dogEncoder)
    }

    it("should throw MissingDiscriminatorMapping for unmapped type") {
      val disc = CustomDiscriminator[JAnyEncoder, Animal](
        DiscriminatorMapper[JAnyEncoder, Dog](JString("canine")),
      )
      a[MissingDiscriminatorMapping[_]] shouldBe thrownBy {
        disc.apply[Cat]
      }
    }

    it("should forbid duplicate values by default") {
      val disc = CustomDiscriminator[JAnyEncoder, Animal](
        DiscriminatorMapper[JAnyEncoder, Dog](JString("canine")),
        DiscriminatorMapper[JAnyEncoder, Cat](JString("feline")),
      )
      disc.duplicateValuesForbidden shouldBe true
    }

    it("should allow duplicate values when configured") {
      val disc = CustomDiscriminator[JAnyEncoder, Animal](duplicateValuesForbidden = false)(
        DiscriminatorMapper[JAnyEncoder, Dog](JString("same")),
        DiscriminatorMapper[JAnyEncoder, Cat](JString("same")),
      )
      disc.duplicateValuesForbidden shouldBe false
    }
  }

  describe("DiscriminatorMapper") {
    it("should match exact type") {
      val mapper = DiscriminatorMapper[JAnyEncoder, Dog](JString("canine"))
      mapper.apply[Dog] shouldBe defined
    }

    it("should match subtype via isAssignableFrom") {
      implicit val failedEncoder: JAnyEncoder[Failed] = (_: Failed) => JString("failed-encoded")
      implicit val terminatedEncoder: JAnyEncoder[Terminated] = (_: Terminated) => JString("terminated-encoded")

      val mapper = DiscriminatorMapper[JAnyEncoder, Terminated](JString("terminated"))
      mapper.apply[Failed] shouldBe defined
    }

    it("should not match unrelated type") {
      val mapper = DiscriminatorMapper[JAnyEncoder, Dog](JString("canine"))
      mapper.apply[Cat] shouldBe None
    }
  }

  describe("DiscriminatorCollision") {
    it("should detect duplicate discriminator values") {
      val dups: List[DiscriminatorMapping[JAnyEncoder, _]] = List(
        DiscriminatorMapping[JAnyEncoder, Dog](JString("same"), None),
        DiscriminatorMapping[JAnyEncoder, Cat](JString("same"), None),
      )
      a[DiscriminatorCollision] shouldBe thrownBy {
        DiscriminatorCollision.detect(dups)
      }
    }

    it("should not throw when no duplicates") {
      val noDups: List[DiscriminatorMapping[JAnyEncoder, _]] = List(
        DiscriminatorMapping[JAnyEncoder, Dog](JString("dog"), None),
        DiscriminatorMapping[JAnyEncoder, Cat](JString("cat"), None),
      )
      noException shouldBe thrownBy {
        DiscriminatorCollision.detect(noDups)
      }
    }

    it("should not throw when mappings share a value but route through the same explicit instance") {
      // Layered discriminators: a forType[IntermediateTrait](...) mapper matches every leaf
      // under the intermediate, and all matches carry the same explicit encoder reference.
      val sharedExplicit: JAnyEncoder[Animal] = (_: Animal) => JString("animal-encoded")
      val layered: List[DiscriminatorMapping[JAnyEncoder, _]] = List(
        DiscriminatorMapping[JAnyEncoder, Animal](JString("terminated"), Some(sharedExplicit)),
        DiscriminatorMapping[JAnyEncoder, Animal](JString("terminated"), Some(sharedExplicit)),
      )
      noException shouldBe thrownBy {
        DiscriminatorCollision.detect(layered)
      }
    }

    it("should detect when mappings share a value but route through different explicit instances") {
      val layered: List[DiscriminatorMapping[JAnyEncoder, _]] = List(
        DiscriminatorMapping[JAnyEncoder, Dog](JString("same"), Some((_: Dog) => JString("a"))),
        DiscriminatorMapping[JAnyEncoder, Cat](JString("same"), Some((_: Cat) => JString("b"))),
      )
      a[DiscriminatorCollision] shouldBe thrownBy {
        DiscriminatorCollision.detect(layered)
      }
    }

    it("should detect when mappings share a value and not all of them have explicit instances") {
      val layered: List[DiscriminatorMapping[JAnyEncoder, _]] = List(
        DiscriminatorMapping[JAnyEncoder, Dog](JString("same"), Some((_: Dog) => JString("a"))),
        DiscriminatorMapping[JAnyEncoder, Cat](JString("same"), None),
      )
      a[DiscriminatorCollision] shouldBe thrownBy {
        DiscriminatorCollision.detect(layered)
      }
    }
  }
}
