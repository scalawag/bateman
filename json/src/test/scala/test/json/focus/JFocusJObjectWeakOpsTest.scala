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

import cats.data.NonEmptyChain
import cats.syntax.either._
import org.scalawag.bateman.json.{DuplicateField, JField, MissingField}
import org.scalawag.bateman.json.lens._
import test.json.BatemanTestBase

class JFocusJObjectOpsWeakTest extends BatemanTestBase {

  describe("fields") {

    it("should return foci to all field values") {
      forAll(genJFocus(genJObject)) { f =>
        val ff = f.fields
        ff.map(_.value) shouldBe f.value.fieldList.map(_.value)
        ff.map(_.pointer) shouldBe f.value.fieldList.map(_.name.value).map(f.pointer.field)
        ff.map(_.root) shouldBe f.value.fieldList.map(_ => f.root)
      }
    }

  }

  describe("fields(String)") {

    it("should return foci to all named field values") {
      forAll(genJFocus(genJObjectWithDuplicateFields, 6)) { f =>
        val name = getDuplicateFieldName(f.value).value
        val ff = f.fields(name)

        val namedFields = f.value.fieldList.filter(_.name.value == name)
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
        val name = getDuplicateFieldName(f.value).value
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
        val name = getDuplicateFieldName(f.value).value
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

  describe("updated") {

    it("should update the value of a field at the given index") {
      forAll(genJFocus(genNonEmptyJObject), genJAny) { (in, value) =>
        val out = in.updated(0, value)
        out.value.fieldList shouldBe in.value.updated(0, value).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }

  describe("delete") {

    it("should delete a field at the given index") {
      forAll(genJFocus(genNonEmptyJObject)) { in =>
        val out = in.delete(0)
        out.value.fieldList shouldBe in.value.delete(0).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }

  describe("insert") {

    it("should insert a field at the given index") {
      forAll(genJFocus(genNonEmptyJObject), genJString, genJAny) { (in, name, value) =>
        val out = in.insert(0, JField(name, value))
        out.value.fieldList shouldBe in.value.insert(0, JField(name, value)).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

    it("should insert a field by name and value at the given index") {
      forAll(genJFocus(genNonEmptyJObject), genJString, genJAny) { (in, name, value) =>
        val out = in.insert(0, name.value, value)
        out.value.fieldList shouldBe in.value.insert(0, name.value, value).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }

  describe("++") {

    it("should concatenate two objects") {
      forAll(genJFocus(genNonEmptyJObject), genJObject) { (in, other) =>
        val out = in.++(other)
        out.value.fieldList shouldBe (in.value ++ other).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }

  describe("overwriteTo") {

    it("should write a new field through a lens") {
      forAll(genJFocus(genNonEmptyJObject), genJAny) { (in, value) =>
        val out = in.overwriteTo(field("__new__"), value)
        out.field("__new__").map(_.value) shouldBe value.stripLocation.rightNec
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

    it("should overwrite an existing field") {
      forAll(genJFocus(genNonEmptyJObject), genJAny) { (in, value) =>
        val name = in.value.fieldList.head.name.value
        val out = in.overwriteTo(field(name), value)
        out.field(name).map(_.value) shouldBe value.stripLocation.rightNec
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

  }

  describe("writeTo") {

    it("should write a new field through a lens") {
      forAll(genJFocus(genNonEmptyJObject), genJAny) { (in, value) =>
        val out = in.writeTo("__new__", value)
        out.map(_.field("__new__").map(_.value)) shouldBe value.stripLocation.rightNec.rightNec
        out.map(_.pointer) shouldBe in.pointer.rightNec
        out.map(_.root.value.shouldHaveNoLocations)
      }
    }

  }
}
