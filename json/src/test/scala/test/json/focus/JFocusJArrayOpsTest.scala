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

package test.json.focus

import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import test.json.BatemanTestBase

class JFocusJArrayOpsTest extends BatemanTestBase {
  val json: JRootFocus[JArray] = parseAs[JArray]("""
    [
      "a",
      ["foo", "quux"],
      null,
      17
    ]
  """)

  describe("items") {
    it("should return foci to all item values") {
      val ff: List[JItemFocus[JAny, JRootFocus[JArray]]] = json.items
      ff.map(_.value) shouldBe json.value.items
      ff.map(_.pointer) shouldBe json.value.items.indices.map(json.pointer.item)
      ff.map(_.root) shouldBe json.value.items.map(_ => json.root)
    }

    it("should return foci to all item values (property)") {
      forAll(genJFocus(genJArray)) { f =>
        val ff = f.items
        ff.map(_.value) shouldBe f.value.items
        ff.map(_.value) shouldBe f.value.items
        ff.map(_.pointer) shouldBe (0 until f.value.items.length).map(f.pointer.item(_))
        ff.map(_.root) shouldBe f.value.items.map(_ => f.root)
      }
    }
  }

  describe("itemOption(Int)") {

    it("should find some item") {
      val f: Option[JItemFocus[JAny, JRootFocus[JArray]]] = json.itemOption(2)
      f shouldBe Some(json.items(2))
    }

    it("should find no item (high)") {
      val f: Option[JItemFocus[JAny, JRootFocus[JArray]]] = json.itemOption(8)
      f shouldBe None
    }

    it("should find no item (low)") {
      val f: Option[JItemFocus[JAny, JRootFocus[JArray]]] = json.itemOption(-1)
      f shouldBe None
    }

    it("should find some item (property)") {
      forAll(genJFocus(genNonEmptyJArray)) { f =>
        f.itemOption(0) shouldBe Some(f.items.head)
      }
    }

    it("should find no item (property)") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        f.itemOption(0) shouldBe None
      }
    }
  }

  describe("item(Int)") {

    it("should find a item") {
      val f: JResult[JItemFocus[JAny, JRootFocus[JArray]]] = json.item(1)
      f shouldBe json.items(1).rightNec
    }

    it("should find no item (high)") {
      val f: JResult[JItemFocus[JAny, JRootFocus[JArray]]] = json.item(8)
      f shouldBe MissingIndex(json, 8).leftNec
    }

    it("should find no item (low)") {
      val f: JResult[JItemFocus[JAny, JRootFocus[JArray]]] = json.item(-1)
      f shouldBe MissingIndex(json, -1).leftNec
    }

    it("should find the item by index (property)") {
      forAll(genJFocus(genNonEmptyJArray)) { f =>
        f.item(0) shouldBe f.items.head.rightNec
      }
    }

    it("should fail on absent item (property)") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        f.item(0) shouldBe MissingIndex(f, 0).leftNec
      }
    }
  }

  describe("append") {
    it("should append an item to the array") {
      val out: JRootFocus[JArray] = json.append("new")
      out.value.items shouldBe json.value.append(JString("new")).items
      out.root.value.shouldHaveNoLocations
    }

    it("should append an item to an array (property)") {
      forAll(genJFocus(genNonEmptyJArray), genJAny) { (in, value) =>
        val out = in.append(value)
        out.value.items shouldBe in.value.insert(in.value.items.length, value).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("prepend") {
    it("should prepend an item to the array") {
      val out: JRootFocus[JArray] = json.prepend("new")
      out.value.items shouldBe json.value.prepend(JString("new")).items
      out.root.value.shouldHaveNoLocations
    }

    it("should prepend an item to an array (property)") {
      forAll(genJFocus(genNonEmptyJArray), genJAny) { (in, value) =>
        val out = in.prepend(value)
        out.value.items shouldBe in.value.insert(0, value).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("updated") {
    it("should update an item at the given index") {
      val out: JRootFocus[JArray] = json.updated(1, "replaced")
      out.value.items shouldBe json.value.updated(1, JString("replaced")).items
      out.root.value.shouldHaveNoLocations
    }

    it("should update an item at the given index (property)") {
      forAll(genJFocus(genNonEmptyJArray), genJAny) { (in, value) =>
        val out = in.updated(0, value)
        out.value.items shouldBe in.value.updated(0, value).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("delete") {
    it("should delete an item at the given index") {
      val out: JRootFocus[JArray] = json.delete(0)
      out.value.items shouldBe json.value.delete(0).items
      out.root.value.shouldHaveNoLocations
    }

    it("should delete an item at the given index (property)") {
      forAll(genJFocus(genNonEmptyJArray)) { in =>
        val out = in.delete(0)
        out.value.items shouldBe in.value.delete(0).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("insert") {
    it("should insert an item at the given index") {
      val out: JRootFocus[JArray] = json.insert(1, "inserted")
      out.value.items shouldBe json.value.insert(1, JString("inserted")).items
      out.root.value.shouldHaveNoLocations
    }

    it("should insert an item at the given index (property)") {
      forAll(genJFocus(genNonEmptyJArray), genJAny) { (in, value) =>
        val out = in.insert(0, value)
        out.value.items shouldBe in.value.insert(0, value).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("++") {
    it("should concatenate two arrays") {
      val other = parseAs[JArray]("""["x", "y"]""")
      val out: JRootFocus[JArray] = json ++ other.value
      out.value.items shouldBe (json.value ++ other.value).items
      out.root.value.shouldHaveNoLocations
    }

    it("should concatenate two arrays (property)") {
      forAll(genJFocus(genNonEmptyJArray), genJArray) { (in, other) =>
        val out = in.++(other)
        out.value.items shouldBe (in.value ++ other).items
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

}
