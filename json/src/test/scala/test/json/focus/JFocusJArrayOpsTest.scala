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

  }

}
