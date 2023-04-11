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

import cats.data.NonEmptyChain
import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import test.json.BatemanTestBase

class JFocusJObjectOpsTest extends BatemanTestBase {
  val json: JRootFocus[JObject] = parseAs[JObject]("""
    {
      "a": {
        "b": {
          "c": ["foo", "quux"]
        }
      },
      "b": null,
      "b": 17
    }
  """)

  describe("fields") {
    it("should return foci to all field values") {
      val ff: List[JFieldFocus[JAny, JRootFocus[JObject]]] = json.fields
      ff.map(_.value) shouldBe json.value.fieldList.map(_.value)
      ff.map(_.pointer) shouldBe json.value.fieldList.map(_.name.value).map(json.pointer.field)
      ff.map(_.root) shouldBe json.value.fieldList.map(_ => json.root)
    }
  }

  describe("fields(String)") {
    def testCase(name: String): Unit = {
      val ff: List[JFieldFocus[JAny, JRootFocus[JObject]]] = json.fields(name)

      val namedFields = json.value.fieldList.filter(_.name.value == name)
      ff.map(_.value) shouldBe namedFields.map(_.value)
      ff.map(_.value) shouldBe namedFields.map(_.value)
      ff.map(_.pointer) shouldBe namedFields.map(_ => json.pointer.field(name))
      ff.map(_.root) shouldBe namedFields.map(_ => json.root)
    }

    it("should return foci to no named field values") {
      testCase("c")
    }

    it("should return foci to all named field values") {
      testCase("b")
    }

    it("should return focus to one named field value") {
      testCase("a")
    }
  }

  describe("fieldOption(String)") {

    it("should find some field") {
      val f: JResult[Option[JFieldFocus[JAny, JRootFocus[JObject]]]] = json.fieldOption("a")
      f shouldBe Some(json.fields.head).rightNec
    }

    it("should find no field") {
      val f: JResult[Option[JFieldFocus[JAny, JRootFocus[JObject]]]] = json.fieldOption("c")
      f shouldBe None.rightNec
    }

    it("should fail on duplicate fields") {
      val f: JResult[Option[JFieldFocus[JAny, JRootFocus[JObject]]]] = json.fieldOption("b")
      f shouldBe DuplicateField(json, NonEmptyChain.fromSeq(json.fields("b")).get).leftNec
    }
  }

  describe("field(String)") {

    it("should find some field") {
      val f: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = json.field("a")
      f shouldBe json.fields.head.rightNec
    }

    it("should find no field") {
      val f: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = json.field("c")
      f shouldBe MissingField(json, "c").leftNec
    }

    it("should fail on duplicate fields") {
      val f: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = json.field("b")
      f shouldBe DuplicateField(json, NonEmptyChain.fromSeq(json.fields("b")).get).leftNec
    }
  }

  describe("field(Int)") {

    it("should find a field") {
      val f: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = json.field(1)
      f shouldBe json.fields(1).rightNec
    }

    it("should find no field") {
      val f: JResult[JFieldFocus[JAny, JRootFocus[JObject]]] = json.field(8)
      f shouldBe MissingFieldIndex(json, 8).leftNec
    }

  }

}
