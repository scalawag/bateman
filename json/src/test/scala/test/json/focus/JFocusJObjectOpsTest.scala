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
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import org.scalawag.bateman.json.lens._
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

    it("should return foci to all field values (property)") {
      forAll(genJFocus(genJObject)) { f =>
        val ff = f.fields
        ff.map(_.value) shouldBe f.value.fieldList.map(_.value)
        ff.map(_.pointer) shouldBe f.value.fieldList.map(_.name.value).map(f.pointer.field)
        ff.map(_.root) shouldBe f.value.fieldList.map(_ => f.root)
      }
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

    it("should return foci to all named field values (property)") {
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

    it("should find some field (property)") {
      forAll(genJFocus(genNonEmptyJObject)) { f =>
        val name = f.value.fieldList.head.name.value
        f.fieldOption(name) shouldBe Some(f.fields.head).rightNec
      }
    }

    it("should find no field (property)") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        f.fieldOption("name") shouldBe None.rightNec
      }
    }

    it("should fail on duplicate fields (property)") {
      forAll(genJFocus(genJObjectWithDuplicateFields, 6)) { f =>
        val name = getDuplicateFieldName(f.value).value
        f.fieldOption(name) shouldBe DuplicateField(f, NonEmptyChain.fromSeq(f.fields(name)).get).leftNec
      }
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

    it("should find the field by index (property)") {
      forAll(genJFocus(genNonEmptyJObject)) { f =>
        val name = f.value.fieldList.head.name.value
        f.field(name) shouldBe f.fields.head.rightNec
      }
    }

    it("should fail on absent field (property)") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        f.field("name") shouldBe MissingField(f, "name").leftNec
      }
    }

    it("should fail on duplicate fields (property)") {
      forAll(genJFocus(genJObjectWithDuplicateFields, 6)) { f =>
        val name = getDuplicateFieldName(f.value).value
        f.field(name) shouldBe DuplicateField(f, NonEmptyChain.fromSeq(f.fields(name)).get).leftNec
      }
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

  describe("append") {
    it("should append a field to the object") {
      val out: JRootFocus[JObject] = json.append("z", JString("new"))
      out.value.fieldList shouldBe json.value.append("z", JString("new")).fieldList
      out.root.value.shouldHaveNoLocations
    }

    it("should append a field to an object (property)") {
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
    it("should prepend a field to the object") {
      val out: JRootFocus[JObject] = json.prepend("z", JString("new"))
      out.value.fieldList shouldBe json.value.prepend("z", JString("new")).fieldList
      out.root.value.shouldHaveNoLocations
    }

    it("should prepend a field to an object (property)") {
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
      val out: JRootFocus[JObject] = json.updated(0, JString("replaced"))
      out.value.fieldList shouldBe json.value.updated(0, JString("replaced")).fieldList
      out.root.value.shouldHaveNoLocations
    }

    it("should update the value of a field at the given index (property)") {
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
      val out: JRootFocus[JObject] = json.delete(0)
      out.value.fieldList shouldBe json.value.delete(0).fieldList
      out.root.value.shouldHaveNoLocations
    }

    it("should delete a field at the given index (property)") {
      forAll(genJFocus(genNonEmptyJObject)) { in =>
        val out = in.delete(0)
        out.value.fieldList shouldBe in.value.delete(0).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("insert") {
    it("should insert a JField at the given index") {
      val f = JField(JString("z"), JString("inserted"))
      val out: JRootFocus[JObject] = json.insert(1, f)
      out.value.fieldList shouldBe json.value.insert(1, f).fieldList
      out.root.value.shouldHaveNoLocations
    }

    it("should insert a field by name and value at the given index") {
      val out: JRootFocus[JObject] = json.insert(1, "z", JString("inserted"))
      out.value.fieldList shouldBe json.value.insert(1, "z", JString("inserted")).fieldList
      out.root.value.shouldHaveNoLocations
    }

    it("should insert a JField at the given index (property)") {
      forAll(genJFocus(genNonEmptyJObject), genJString, genJAny) { (in, name, value) =>
        val out = in.insert(0, JField(name, value))
        out.value.fieldList shouldBe in.value.insert(0, JField(name, value)).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

    it("should insert a field by name and value at the given index (property)") {
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
      val other = parseAs[JObject]("""{"x": 1, "y": 2}""")
      val out: JRootFocus[JObject] = json ++ other.value
      out.value.fieldList shouldBe (json.value ++ other.value).fieldList
      out.root.value.shouldHaveNoLocations
    }

    it("should concatenate two objects (property)") {
      forAll(genJFocus(genNonEmptyJObject), genJObject) { (in, other) =>
        val out = in.++(other)
        out.value.fieldList shouldBe (in.value ++ other).fieldList
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("overwriteTo") {
    it("should write a value through a lens") {
      val out: JRootFocus[JObject] = json.overwriteTo("x", "new")
      out.field("x").map(_.value) shouldBe JString("new").rightNec
      out.root.value.shouldHaveNoLocations
    }

    it("should overwrite an existing value") {
      val out: JRootFocus[JObject] = json.overwriteTo("a", "replaced")
      out.field("a").map(_.value) shouldBe JString("replaced").rightNec
      out.root.value.shouldHaveNoLocations
    }

    it("should create nested structure") {
      val out: JRootFocus[JObject] = json.overwriteTo("x" ~> "y", "deep")
      out.field("x").flatMap(_.asObject).flatMap(_.field("y")).map(_.value) shouldBe JString("deep").rightNec
      out.root.value.shouldHaveNoLocations
    }

    it("should prepend when prepend = true") {
      val out: JRootFocus[JObject] = json.overwriteTo("z", "first", prepend = true)
      out.value.fieldList.head.name.value shouldBe "z"
      out.root.value.shouldHaveNoLocations
    }

    it("should write a new field through a lens (property)") {
      forAll(genJFocus(genNonEmptyJObject), genJAny) { (in, value) =>
        val out = in.overwriteTo(field("__new__"), value)
        out.field("__new__").map(_.value) shouldBe value.stripLocation.rightNec
        out.pointer shouldBe in.pointer
        out.root.value.shouldHaveNoLocations
      }
    }

    it("should overwrite an existing field (property)") {
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
    it("should write a value through a lens") {
      val out: JResult[JRootFocus[JObject]] = json.writeTo("x", "new")
      out.map(_.field("x").map(_.value)) shouldBe JString("new").rightNec.rightNec
      out.map(_.root.value.shouldHaveNoLocations)
    }

    it("should write to a nested path") {
      val out: JResult[JRootFocus[JObject]] = json.writeTo("a" ~> "b" ~> "d", "new")
      out.map(_.field("a").flatMap(_.asObject).flatMap(_.field("b")).flatMap(_.asObject).flatMap(_.field("d")).map(_.value)) shouldBe JString("new").rightNec.rightNec
      out.map(_.root.value.shouldHaveNoLocations)
    }

    it("should write a new field through a lens (property)") {
      forAll(genJFocus(genNonEmptyJObject), genJAny) { (in, value) =>
        val out = in.writeTo("__new__", value)
        out.map(_.field("__new__").map(_.value)) shouldBe value.stripLocation.rightNec.rightNec
        out.map(_.pointer) shouldBe in.pointer.rightNec
        out.map(_.root.value.shouldHaveNoLocations)
      }
    }
  }
}
