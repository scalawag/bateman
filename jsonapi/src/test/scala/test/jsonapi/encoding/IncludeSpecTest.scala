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

package test.jsonapi.encoding

import cats.data.NonEmptyChain
import cats.syntax.either._
import org.scalawag.bateman.json.JString
import org.scalawag.bateman.jsonapi.encoding.{IncludePathTooDeep, IncludeSpec, IncludeTooLong}
import org.scalawag.bateman.jsonapi.encoding.IncludeSpec.{Never, Opportunistically}
import test.json.BatemanTestBase

class IncludeSpecTest extends BatemanTestBase {
  describe("Always") {
    val spec = IncludeSpec.unsafe("a.b.c,x.y")

    it("should include a") {
      spec.descend("a") shouldBe an[IncludeSpec.Always]
    }
    it("should include a.b") {
      spec.descend("a").descend("b") shouldBe an[IncludeSpec.Always]
    }
    it("should include a.b.c") {
      spec.descend("a").descend("b").descend("c") shouldBe an[IncludeSpec.Always]
    }
    it("should include x") {
      spec.descend("x") shouldBe an[IncludeSpec.Always]
    }
    it("should include x.y") {
      spec.descend("x").descend("y") shouldBe an[IncludeSpec.Always]
    }
    it("should exclude a.c") {
      spec.descend("a").descend("c") shouldBe IncludeSpec.Never
    }
    it("should exclude x.y.z") {
      spec.descend("x").descend("y").descend("z") shouldBe IncludeSpec.Never
    }
    it("should exclude n") {
      spec.descend("n") shouldBe IncludeSpec.Never
    }
    it("should have explicit children at root") {
      spec.explicitChildren shouldBe Set("a", "x")
    }
    it("should have explicit children at a") {
      spec.descend("a").explicitChildren shouldBe Set("b")
    }
    it("should have explicit children at a.b") {
      spec.descend("a").descend("b").explicitChildren shouldBe Set("c")
    }
    it("should not have explicit children at a.b.c") {
      spec.descend("a").descend("c").descend("c").explicitChildren shouldBe empty
    }
  }

  describe("Never") {
    it("should exclude anything descending from here") {
      forAll { (s: JString) =>
        Never.descend(s.value) shouldBe Never
      }
    }
    it("should have no explicit children") {
      Never.explicitChildren shouldBe empty
    }
    it("should be parsed from an empty string") {
      IncludeSpec.unsafe("") shouldBe Never
    }
  }

  describe("Opportunistically") {
    it("should exclude anything descending from here") {
      forAll { (s: JString) =>
        Opportunistically.descend(s.value) shouldBe Opportunistically
      }
    }
    it("should have no explicit children") {
      Opportunistically.explicitChildren shouldBe empty
    }
  }

  describe("errors") {
    it("should fail if string is too long") {
      IncludeSpec("0123456789", lengthLimit = 8) shouldBe IncludeTooLong(8).leftNec
    }

    it("should fail if path is too deep") {
      IncludeSpec("a.b.c,d.e.f.g", depthLimit = 2) shouldBe NonEmptyChain(
        IncludePathTooDeep("a.b.c", 2),
        IncludePathTooDeep("d.e.f.g", 2),
      ).asLeft
    }

    it("should throw on errors") {
      intercept[IllegalArgumentException] {
        IncludeSpec.unsafe("0123456789", lengthLimit = 8)
      }
    }
  }
}
