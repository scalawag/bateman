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
import org.scalawag.bateman.json.{DuplicateField, JField, MissingField}
import org.scalawag.bateman.json.focus.weak._
import test.json.BatemanTestBase

class JFocusJObjectWeakOpsTest extends BatemanTestBase {

  describe("fields") {

    it("should return foci to all field values") {
      forAll(genJFocus(genJObject)) { f =>
        val ff = f.fields
        ff.map(_.value) shouldBe f.value.fieldList.map(_.value)
        ff.map(_.value) shouldBe f.value.fieldList.map(_.value)
        ff.map(_.pointer) shouldBe f.value.fieldList.map(_.name.value).map(f.pointer.field)
        ff.map(_.root) shouldBe f.value.fieldList.map(_ => f.root)
      }
    }

  }

  describe("fields(String)") {

    it("should return foci to all named field values") {
      forAll(genJFocus(genJObjectWithDuplicateFields, 6)) { f =>
        val Some(name) = getDuplicateFieldName(f.value)
        val ff = f.fields(name)

        val namedFields = f.value.fieldList.filter(_.name.value == name)
        ff.map(_.value) shouldBe namedFields.map(_.value)
        ff.map(_.value) shouldBe namedFields.map(_.value)
        ff.map(_.pointer) shouldBe namedFields.map(_ => f.pointer.field(name))
        ff.map(_.root) shouldBe namedFields.map(_ => f.root)
      }
    }

  }

  describe("fieldOption(String)") {

    it("should find some field") {
      forAll(genJFocus(genNonEmptyJObject)) { f =>
        val name = f.value.fieldList.head.name.value
        f.fieldOption(name) shouldBe Some(f.fields.head).rightNec
      }
    }

    it("should find no field") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        f.fieldOption("name") shouldBe None.rightNec
      }
    }

    it("should fail on duplicate fields") {
      forAll(genJFocus(genJObjectWithDuplicateFields, 6)) { f =>
        val Some(name) = getDuplicateFieldName(f.value)
        f.fieldOption(name) shouldBe DuplicateField(f, NonEmptyChain.fromSeq(f.fields(name)).get).leftNec
      }
    }

  }

  describe("field(String)") {

    it("should find the field by index") {
      forAll(genJFocus(genNonEmptyJObject)) { f =>
        val name = f.value.fieldList.head.name.value
        f.field(name) shouldBe f.fields.head.rightNec
      }
    }

    it("should fail on absent field") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        f.field("name") shouldBe MissingField(f, "name").leftNec
      }
    }

    it("should fail on duplicate fields") {
      forAll(genJFocus(genJObjectWithDuplicateFields, 6)) { f =>
        val Some(name) = getDuplicateFieldName(f.value)
        f.field(name) shouldBe DuplicateField(f, NonEmptyChain.fromSeq(f.fields(name)).get).leftNec
      }
    }

  }

  describe("append") {

    it("should append a field to an object") {
      forAll(genJFocus(genNonEmptyJObject), genJString, genJAny) { (in, name, value) =>
        val out = in.append(name.value, value)
        out.value.fieldList shouldBe
          in.value.insert(in.value.fieldList.length, JField(name, value)).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }

  describe("prepend") {

    it("should prepend a field to an object") {
      forAll(genJFocus(genNonEmptyJObject), genJString, genJAny) { (in, name, value) =>
        val out = in.prepend(name.value, value)
        out.value.fieldList shouldBe in.value.insert(0, JField(name, value)).stripLocation.fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }
}
