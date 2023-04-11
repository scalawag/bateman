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

package test.json.focus

import cats.syntax.either._
import org.scalawag.bateman.json.MissingIndex
import org.scalawag.bateman.json.focus.weak._
import test.json.BatemanTestBase

class JFocusJArrayWeakOpsTest extends BatemanTestBase {

  describe("items") {

    it("should return foci to all item values") {
      forAll(genJFocus(genJArray)) { f =>
        val ff = f.items
        ff.map(_.value) shouldBe f.value.items
        ff.map(_.value) shouldBe f.value.items
        ff.map(_.pointer) shouldBe Stream.from(0).take(f.value.items.length).map(f.pointer.item(_))
        ff.map(_.root) shouldBe f.value.items.map(_ => f.root)
      }
    }

  }

  describe("itemOption(Int)") {

    it("should find some item") {
      forAll(genJFocus(genNonEmptyJArray)) { f =>
        f.itemOption(0) shouldBe Some(f.items.head)
      }
    }

    it("should find no item") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        f.itemOption(0) shouldBe None
      }
    }

  }

  describe("item(Int)") {

    it("should find the item by index") {
      forAll(genJFocus(genNonEmptyJArray)) { f =>
        f.item(0) shouldBe f.items.head.rightNec
      }
    }

    it("should fail on absent item") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        f.item(0) shouldBe MissingIndex(f, 0).leftNec
      }
    }

  }

  describe("append") {

    it("should append a item to an array") {
      forAll(genJFocus(genNonEmptyJArray), genJAny) { (in, value) =>
        val out = in.append(value)
        out.value.items shouldBe in.value.insert(in.value.items.length, value).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }

  describe("prepend") {

    it("should prepend a item to an array") {
      forAll(genJFocus(genNonEmptyJArray), genJAny) { (in, value) =>
        val out = in.prepend(value)
        out.value.items shouldBe in.value.insert(0, value).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }
}
