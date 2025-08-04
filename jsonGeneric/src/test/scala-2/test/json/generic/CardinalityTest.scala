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

import cats.Traverse
import cats.syntax.traverse._
import org.scalawag.bateman.json.{NotNull, Nullable, Single}
import org.scalawag.bateman.json.generic.Cardinality
import test.json.BatemanTestBase

class CardinalityTest extends BatemanTestBase {

  describe("Cardinality") {
    it("should provide a Traverse instance for Single (Id)") {
      val card = implicitly[Cardinality[Single]]
      card.traverse should not be null
    }

    it("should provide a Traverse instance for Nullable (Option)") {
      val card = implicitly[Cardinality[Nullable]]
      card.traverse should not be null
    }

    it("should provide a Traverse instance for List") {
      val card = implicitly[Cardinality[List]]
      card.traverse should not be null
    }

    it("should derive Traverse from Cardinality implicitly") {
      // The traverseForCardinality implicit should allow Traverse ops on types with a Cardinality
      val traverse: Traverse[List] = Cardinality.traverseForCardinality[List]
      traverse should not be null
    }

    it("should traverse Single values") {
      implicit val t: Traverse[Single] = implicitly[Cardinality[Single]].traverse
      val result: Option[Single[Int]] = (42: Single[Int]).traverse(x => Option(x + 1))
      result shouldBe Some(43)
    }

    it("should traverse Nullable values") {
      implicit val t: Traverse[Nullable] = implicitly[Cardinality[Nullable]].traverse
      val result: Option[Nullable[Int]] = (NotNull(42): Nullable[Int]).traverse(x => Option(x + 1))
      result shouldBe Some(NotNull(43))
    }

    it("should traverse List values") {
      implicit val t: Traverse[List] = implicitly[Cardinality[List]].traverse
      val result: Option[List[Int]] = List(1, 2, 3).traverse(x => Option(x + 1))
      result shouldBe Some(List(2, 3, 4))
    }
  }
}
