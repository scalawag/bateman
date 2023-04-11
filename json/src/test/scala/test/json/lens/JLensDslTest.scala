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

package test.json.lens

import cats.syntax.either._
import cats.syntax.traverse._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalacheck.Gen
import test.json.BatemanTestBase
import org.scalawag.bateman.json.focus.weak._

class JLensDslTest extends BatemanTestBase {
  private val json = parseAs[JObject]("""
    {
      "a": {
        "g": 4,
        "f": "thing",
        "b": true
      },
      "b": 6,
      "g": [
        {
          "c": 8
        },
        "foo",
        []
      ],
      "deep": [
        [
          { "a": 4 },
          { "a": true },
          { "a": "g" }
        ],
        [
          { "a": 17 },
          { "a": 83 },
          { "a": 56.78e4 }
        ]
      ]
    }
  """)

  def jarray = json.field("g").flatMap(_.asArray).shouldSucceed.value.asRootFocus

  describe("focus") {
    val lens = focus

    it("should return the same focus") {
      forAll(genJFocus(genJAny)) { f =>
        lens(f) shouldBe f.rightNec
      }
    }

    it("should toString as 'focus'") {
      lens.toString shouldBe "focus"
    }
  }

  describe("root") {
    val lens = root

    it("should return the same focus for root foci") {
      forAll(genJFocus(genJAny, 0)) { f =>
        lens(f) shouldBe f.rightNec
      }
    }

    it("should return the root focus for deep foci") {
      forAll(genJFocus(genJAny)) { f =>
        lens(f) shouldBe f.root.rightNec
      }
    }

    it("should toString as 'root'") {
      lens.toString shouldBe "root"
    }
  }

  describe("narrow") {
    val lens: CreatableJLens[JAny, JString] = narrow[JString]

    it("should narrow the focus with an implicit decoder") {
      forAll(genJFocus(genJString)) { f =>
        lens(f) shouldBe f.rightNec
      }
    }

    it("should fail when the focus is not a JString") {
      forAll(genJFocus(genJAny)) { f =>
        inside(f.value) {
          case s: JString =>
            lens(f) shouldBe f.rightNec
          case _ =>
            lens(f) shouldBe JsonTypeMismatch(f, JString).leftNec
        }
      }
    }

    it("should toString as 'narrow[JString]'") {
      lens.toString shouldBe "narrow[JString]"
    }
  }

  describe("field") {

    it("should extract a field from the focused object") {
      forAll(genJFocus(genNonEmptyJObject)) { f =>
        // Get a valid field name from the arbitrary object
        val name = f.value.fieldList.head.name.value
        field(name)(f) shouldBe f.fields.head.rightNec
      }
    }

    it("should fail when the focus is not an object") {
      forAll(genJFocus(genJAny)) { f =>
        inside(f.value) {
          case _: JObject =>
            succeed
          case _ =>
            field("name")(f) shouldBe JsonTypeMismatch(f, JObject).leftNec
        }
      }
    }

    it("should fail when the named field doesn't exist") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        field("name")(f) shouldBe MissingField(f, "name").leftNec
      }
    }

    it("should implicitly convert a bare String to a field lens") {
      val lens1: CreatableJLens[JAny, JAny] = "name"
      val lens2: CreatableJLens[JAny, JAny] = field("name")
      lens1 shouldBe lens2
    }

    it("should extract None for a nonexistent field (use case)") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        val lens = field("name").?
        lens(f) shouldBe None.rightNec
      }
    }

    it("should extract Some for an optional field (use case)") {
      forAll(genJFocus(genNonEmptyJObject)) { f =>
        // Get a valid field name from the arbitrary object
        val name = f.value.fieldList.head.name.value
        val lens = field(name).?
        lens(f) shouldBe Some(f.fields.head).rightNec
      }
    }

    it("should toString as '\"name\"'") {
      field("name").toString shouldBe "\"name\""
    }

  }

  describe("fields(name)") {

    it("should extract multiple fields from the focused object") {
      forAll(genJFocus(genJObjectWithDuplicateFields)) { f =>
        val Some(name) = getDuplicateFieldName(f.value)
        fields(name)(f).shouldSucceed.foci shouldBe f.fields(name)
      }
    }

    it("should fail when the focus is not an object") {
      forAll(genJFocus(genJAny)) { f =>
        inside(f.value) {
          case _: JObject =>
            succeed
          case _ =>
            fields("name")(f) shouldBe JsonTypeMismatch(f, JObject).leftNec
        }
      }
    }

    it("should extract Nil when the named field doesn't exist") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        fields("name")(f).shouldSucceed.foci shouldBe Nil
      }
    }

    it("should convert field.all to fields") {
      val lens1: ListJLens[JAny, JAny] = "name".all
      val lens2: ListJLens[JAny, JAny] = fields("name")
      lens1 shouldBe lens2
    }

    it("should convert field.** to fields") {
      val lens1: ListJLens[JAny, JAny] = ("name": FieldJLens).**
      val lens2: ListJLens[JAny, JAny] = fields("name")
      lens1 shouldBe lens2
    }

    it("should toString as '\"name\".**'") {
      fields("name").toString shouldBe "\"name\".**"
    }

  }

  describe("fields") {

    it("should extract all fields from the focused array") {
      forAll(genJFocus(genJObject)) { f =>
        fields(f).shouldSucceed.foci shouldBe f.fields
      }
    }

    it("should extract no fields from an empty object") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        fields(f).shouldSucceed.foci shouldBe Nil
      }
    }

    it("should fail when the focus is not an object") {
      forAll(genJFocus(genJAny)) { f =>
        inside(f.value) {
          case _: JObject =>
            succeed
          case _ =>
            fields(f) shouldBe JsonTypeMismatch(f, JObject).leftNec
        }
      }
    }

    it("should implicitly convert a bare ** to an fields lens") {
      val lens1: ListJLens[JAny, JAny] = **
      val lens2: ListJLens[JAny, JAny] = fields
      lens1 shouldBe lens2
    }

    it("should toString as '**'") {
      fields.toString shouldBe "**"
    }

  }

  describe("item") {

    it("should extract an item from the focused array") {
      // Get an arbitrary array and an arbitrary index into it
      val genArrayAndIndex =
        for {
          f <- genJFocus(genNonEmptyJArray)
          i <- Gen.choose(0, f.value.items.size - 1)
        } yield (f, i)

      forAll(genArrayAndIndex) {
        case (f, i) =>
          item(i)(f) shouldBe f.items(i).rightNec
      }
    }

    it("should fail when the focus is not an array") {
      forAll(genJFocus(genJAny)) { f =>
        inside(f.value) {
          case _: JArray =>
            succeed
          case _ =>
            item(0)(f) shouldBe JsonTypeMismatch(f, JArray).leftNec
        }
      }
    }

    it("should fail when the specified item doesn't exist") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        item(-1)(f) shouldBe MissingIndex(f, -1).leftNec
      }
    }

    it("should implicitly convert a bare Int to an item lens") {
      val lens1: IdJLens[JAny, JAny] = 17
      val lens2: IdJLens[JAny, JAny] = item(17)
      lens1 shouldBe lens2
    }

    it("should extract None for a nonexistent item (use case)") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        val lens = item(-1).?
        lens(f) shouldBe None.rightNec
      }
    }

    it("should extract Some for an optional item (use case)") {
      forAll(genJFocus(genNonEmptyJArray)) { f =>
        // Get a valid item name from the arbitrary array
        val lens = item(0).?
        lens(f) shouldBe Some(f.items.head).rightNec
      }
    }

    it("should toString as '23'") {
      item(23).toString shouldBe "23"
    }

  }

  describe("items") {

    it("should extract all items from the focused array") {
      forAll(genJFocus(genJArray)) { f =>
        items(f).shouldSucceed.foci shouldBe f.items
      }
    }

    it("should extract no items from an empty array") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        items(f).shouldSucceed.foci shouldBe Nil
      }
    }

    it("should fail when the focus is not an array") {
      forAll(genJFocus(genJAny)) { f =>
        inside(f.value) {
          case _: JArray =>
            succeed
          case _ =>
            items(f) shouldBe JsonTypeMismatch(f, JArray).leftNec
        }
      }
    }

    it("should implicitly convert a bare * to an items lens") {
      val lens1: ListJLens[JAny, JAny] = *
      val lens2: ListJLens[JAny, JAny] = items
      lens1 shouldBe lens2
    }

    it("should toString as '*'") {
      items.toString shouldBe "*"
    }

  }

  describe("composition") {

    describe("CreatableJLens ~> CreatableJLens") {

      it("should work") {
        val lens: CreatableJLens[JAny, JAny] = field("a") ~> field("g")
        lens(json) shouldBe json.field("a").flatMap(_.asObject).flatMap(_.field("g"))
      }

      it("should fail fast if LHS fails") {
        val lens: CreatableJLens[JAny, JAny] = field("x") ~> field("g")
        lens(json) shouldBe MissingField(json, "x").leftNec
      }

      it("should fail if RHS fails") {
        val lens: CreatableJLens[JAny, JAny] = field("a") ~> field("x")
        lens(json) shouldBe MissingField(json.field("a").flatMap(_.asObject).shouldSucceed, "x").leftNec
      }

      it("should toString properly") {
        val lens: CreatableJLens[JAny, JAny] = field("a") ~> field("g")
        lens.toString shouldBe """"a" ~> "g""""
      }

    }

    describe("CreatableJLens ~> IdJLens") {

      it("should work") {
        val lens: IdJLens[JAny, JAny] = field("g") ~> item(1)
        lens(json) shouldBe json.field("g").flatMap(_.asArray).flatMap(_.item(1))
      }

      it("should fail fast if LHS fails") {
        val lens: IdJLens[JAny, JAny] = field("x") ~> item(1)
        lens(json) shouldBe MissingField(json, "x").leftNec
      }

      it("should fail if RHS fails") {
        val lens: IdJLens[JAny, JAny] = field("g") ~> item(99)
        lens(json) shouldBe MissingIndex(json.field("g").flatMap(_.asArray).shouldSucceed, 99).leftNec
      }

      it("should toString properly") {
        val lens: IdJLens[JAny, JAny] = field("g") ~> 99
        lens.toString shouldBe """"g" ~> 99"""
      }

    }

    describe("CreatableJLens ~> OptionJLens") {

      it("should work") {
        val lens: OptionJLens[JAny, JAny] = field("g") ~> item(1).?
        lens(json) shouldBe json.field("g").flatMap(_.asArray).map(_.itemOption(1))
      }

      it("should fail fast if LHS fails") {
        val lens: OptionJLens[JAny, JAny] = field("x") ~> item(1).?
        lens(json) shouldBe MissingField(json, "x").leftNec
      }

      it("should fail if RHS fails") {
        val lens: OptionJLens[JAny, JAny] = field("g") ~> field("a").?
        lens(json) shouldBe JsonTypeMismatch(json.field("g").shouldSucceed, JObject).leftNec
      }

      it("should toString properly") {
        val lens: OptionJLens[JAny, JAny] = field("g") ~> 99.?
        lens.toString shouldBe """"g" ~> 99.?"""
      }

    }

    describe("CreatableJLens ~> ListJLens") {

      it("should work") {
        val lens: ListJLens[JAny, JAny] = field("g") ~> *
        lens(json).map(_.foci) shouldBe json.field("g").flatMap(_.asArray).map(_.items)
      }

      it("should fail fast if LHS fails") {
        val lens: ListJLens[JAny, JAny] = field("x") ~> *
        lens(json) shouldBe MissingField(json, "x").leftNec
      }

      it("should fail if RHS fails") {
        val lens: ListJLens[JAny, JAny] = field("g") ~> **
        lens(json) shouldBe JsonTypeMismatch(json.field("g").shouldSucceed, JObject).leftNec
      }

      it("should toString properly") {
        val lens: ListJLens[JAny, JAny] = field("g") ~> **
        lens.toString shouldBe """"g" ~> **"""
      }

    }

    describe("IdJLens ~> CreatableJLens") {
      it("should work") {
        val lens: IdJLens[JAny, JAny] = 0 ~> "c"
        lens(jarray) shouldBe jarray.item(0).flatMap(_.asObject).flatMap(_.field("c"))
      }

      it("should fail fast if LHS fails") {
        val lens: IdJLens[JAny, JAny] = 99 ~> "c"
        lens(jarray) shouldBe MissingIndex(jarray, 99).leftNec
      }

      it("should fail if RHS fails") {
        val lens: IdJLens[JAny, JAny] = 0 ~> "x"
        lens(jarray) shouldBe MissingField(jarray.items.head.asObject.shouldSucceed, "x").leftNec
      }

      it("should toString properly") {
        val lens: IdJLens[JAny, JAny] = 0 ~> "c"
        lens.toString shouldBe """0 ~> "c""""
      }

    }

    describe("IdJLens ~> IdJLens") {

      it("should work") {
        val lens: IdJLens[JAny, JAny] = 0 ~> 0
        lens(jarray) shouldBe jarray.item(0).flatMap(_.asArray).flatMap(_.item(0))
      }

      it("should fail fast if LHS fails") {
        val lens: IdJLens[JAny, JAny] = 99 ~> 0
        lens(jarray) shouldBe MissingIndex(jarray, 99).leftNec
      }

      it("should fail if RHS fails") {
        val lens: IdJLens[JAny, JAny] = 0 ~> 99
        lens(jarray) shouldBe JsonTypeMismatch(jarray.items.head, JArray).leftNec
      }

      it("should toString properly") {
        val lens: IdJLens[JAny, JAny] = 0 ~> 1
        lens.toString shouldBe """0 ~> 1"""
      }

    }

    describe("IdJLens ~> OptionJLens") {

      it("should work") {
        val lens: OptionJLens[JAny, JAny] = 0 ~> 1.?
        lens(jarray) shouldBe jarray.item(0).flatMap(_.asArray).map(_.itemOption(1))
      }

      it("should fail fast if LHS fails") {
        val lens: OptionJLens[JAny, JAny] = 99 ~> 0.?
        lens(jarray) shouldBe MissingIndex(jarray, 99).leftNec
      }

      it("should fail if RHS fails") {
        val lens: OptionJLens[JAny, JAny] = 1 ~> 0.?
        lens(jarray) shouldBe JsonTypeMismatch(jarray.items(1), JArray).leftNec
      }

      it("should toString properly") {
        val lens: OptionJLens[JAny, JAny] = 0 ~> 1.?
        lens.toString shouldBe """0 ~> 1.?"""
      }

    }

    describe("IdJLens ~> ListJLens") {

      it("should work") {
        val lens: ListJLens[JAny, JAny] = 2 ~> *
        lens(jarray).shouldSucceed.foci shouldBe jarray.items(2).asArray.shouldSucceed.items
      }

      it("should fail fast if LHS fails") {
        val lens: ListJLens[JAny, JAny] = 99 ~> *
        lens(jarray) shouldBe MissingIndex(jarray, 99).leftNec
      }

      it("should fail if RHS fails") {
        val lens: ListJLens[JAny, JAny] = 1 ~> *
        lens(jarray) shouldBe JsonTypeMismatch(jarray.items(1), JArray).leftNec
      }

      it("should toString properly") {
        val lens: ListJLens[JAny, JAny] = 0 ~> *
        lens.toString shouldBe """0 ~> *"""
      }

    }

    describe("OptionJLens ~> CreatableJLens") {

      it("should work") {
        val lens: OptionJLens[JAny, JAny] = "a".? ~> "g"
        lens(json) shouldBe json.fieldOption("a").flatMap(_.traverse(_.asObject.flatMap(_.field("g"))))
      }

      it("should fail fast if LHS fails") {
        val lens: OptionJLens[JAny, JAny] = 0.? ~> "c"
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: OptionJLens[JAny, JAny] = "g".? ~> "x"
        lens(json) shouldBe JsonTypeMismatch(json.field("g").shouldSucceed, JObject).leftNec
      }

      it("should toString properly") {
        val lens: OptionJLens[JAny, JAny] = "a".? ~> "c"
        lens.toString shouldBe """"a".? ~> "c""""
      }

    }

    describe("OptionJLens ~> IdJLens") {

      it("should work") {
        val lens: OptionJLens[JAny, JAny] = "g".? ~> 0
        lens(json) shouldBe json.fieldOption("g").flatMap(_.traverse(_.asArray.flatMap(_.item(0))))
      }

      it("should fail fast if LHS fails") {
        val lens: OptionJLens[JAny, JAny] = 0.? ~> 0
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: OptionJLens[JAny, JAny] = "g".? ~> 99
        lens(json) shouldBe MissingIndex(json.field("g").flatMap(_.asArray).shouldSucceed, 99).leftNec
      }

      it("should toString properly") {
        val lens: OptionJLens[JAny, JAny] = "g".? ~> 1
        lens.toString shouldBe """"g".? ~> 1"""
      }

    }

    describe("OptionJLens ~> OptionJLens") {

      it("should work") {
        val lens: OptionJLens[JAny, JAny] = "g".? ~> 1.?
        lens(json) shouldBe json.fieldOption("g").flatMap(_.flatTraverse(_.asArray.map(_.itemOption(1))))
      }

      it("should fail fast if LHS fails") {
        val lens: OptionJLens[JAny, JAny] = 0.? ~> 0.?
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: OptionJLens[JAny, JAny] = "a".? ~> 0.?
        lens(json) shouldBe JsonTypeMismatch(json.field("a").shouldSucceed, JArray).leftNec
      }

      it("should toString properly") {
        val lens: OptionJLens[JAny, JAny] = 0.? ~> 1.?
        lens.toString shouldBe """0.? ~> 1.?"""
      }

    }

    describe("OptionJLens ~> ListJLens") {

      it("should work") {
        val lens: ListJLens[JAny, JAny] = "g".? ~> *
        lens(json).map(_.foci) shouldBe json.fieldOption("g").flatMap(_.toList.flatTraverse(_.asArray.map(_.items)))
      }

      it("should fail fast if LHS fails") {
        val lens: ListJLens[JAny, JAny] = 0.? ~> *
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: ListJLens[JAny, JAny] = "a".? ~> *
        lens(json) shouldBe JsonTypeMismatch(json.fields.head, JArray).leftNec
      }

      it("should toString properly") {
        val lens: ListJLens[JAny, JAny] = "a".? ~> *
        lens.toString shouldBe """"a".? ~> *"""
      }

    }

    describe("ListJLens ~> CreatableJLens") {

      it("should work") {
        val lens: ListJLens[JAny, JAny] = focus ~> "a".** ~> "g"
        lens(json).map(_.foci) shouldBe json.fields("a").traverse(_.asObject.flatMap(_.field("g")))
      }

      it("should fail fast if LHS fails") {
        val lens: ListJLens[JAny, JAny] = * ~> "c"
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: ListJLens[JAny, JAny] = "a".** ~> "x"
        lens(json) shouldBe MissingField(json.fields.head.asObject.shouldSucceed, "x").leftNec
      }

      it("should toString properly") {
        val lens: ListJLens[JAny, JAny] = "a".** ~> "c"
        lens.toString shouldBe """"a".** ~> "c""""
      }

    }

    describe("ListJLens ~> IdJLens") {

      it("should work") {
        val lens: ListJLens[JAny, JAny] = "g".** ~> 0
        lens(json).map(_.foci) shouldBe json.fields("g").traverse(_.asArray.flatMap(_.item(0)))
      }

      it("should fail fast if LHS fails") {
        val lens: ListJLens[JAny, JAny] = * ~> 0
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: ListJLens[JAny, JAny] = "a".** ~> 0
        lens(json) shouldBe JsonTypeMismatch(json.field("a").shouldSucceed, JArray).leftNec
      }

      it("should toString properly") {
        val lens: ListJLens[JAny, JAny] = "g".** ~> 1
        lens.toString shouldBe """"g".** ~> 1"""
      }

    }

    describe("ListJLens ~> OptionJLens") {

      it("should work") {
        val lens: ListJLens[JAny, JAny] = "g".** ~> 1.?
        lens(json).map(_.foci) shouldBe json.fields("g").flatTraverse(_.asArray.map(_.itemOption(1).toList))
      }

      it("should fail fast if LHS fails") {
        val lens: ListJLens[JAny, JAny] = * ~> 0.?
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: ListJLens[JAny, JAny] = "a".** ~> 0.?
        lens(json) shouldBe JsonTypeMismatch(json.field("a").shouldSucceed, JArray).leftNec
      }

      it("should toString properly") {
        val lens: ListJLens[JAny, JAny] = ** ~> 1.?
        lens.toString shouldBe """** ~> 1.?"""
      }

    }

    describe("ListJLens ~> ListJLens") {

      it("should work") {
        val lens: ListJLens[JAny, JAny] = "g".** ~> *
        lens(json).map(_.foci) shouldBe json.fieldOption("g").flatMap(_.toList.flatTraverse(_.asArray.map(_.items)))
      }

      it("should fail fast if LHS fails") {
        val lens: ListJLens[JAny, JAny] = * ~> *
        lens(json) shouldBe JsonTypeMismatch(json, JArray).leftNec
      }

      it("should fail if RHS fails") {
        val lens: ListJLens[JAny, JAny] = "a".** ~> *
        lens(json) shouldBe JsonTypeMismatch(json.fields.head, JArray).leftNec
      }

      it("should toString properly") {
        val lens: ListJLens[JAny, JAny] = ** ~> *
        lens.toString shouldBe """** ~> *"""
      }

    }

  }

  describe("optionality") {

    it("should allow optional on CreatableJLens") {
      val lens: CreatableJLens[JAny, JAny] = "x"
      lens.?(json) shouldBe None.rightNec
    }

    it("should allow optional on CompositeCreatableJLens") {
      val lens: CreatableJLens[JAny, JAny] = "x" ~> "x"
      lens.?(json) shouldBe None.rightNec
    }

    it("should allow optional on IdJLens") {
      val lens: IdJLens[JAny, JAny] = 99
      lens.?(jarray) shouldBe None.rightNec
    }

    it("should allow optional on CompositeIdJLens") {
      val lens: IdJLens[JAny, JAny] = 99 ~> 99
      lens.?(jarray) shouldBe None.rightNec
    }

    it("should allow optional on OptionJLens") {
      val lens: OptionJLens[JAny, JAny] = "d".?
      lens.?(json) shouldBe None.rightNec
    }

    it("should allow optional on CompositeOptionJLens") {
      val lens: OptionJLens[JAny, JAny] = "x" ~> "d".?
      lens.?(json) shouldBe None.rightNec
    }

    it("should allow optional on ListJLens") {
      val lens: ListJLens[JAny, JAny] = "x".**
      lens.?(json).shouldSucceed.foci shouldBe Nil
    }

    it("should allow optional on CompositeListJLens") {
      val lens: ListJLens[JAny, JAny] = "x" ~> *
      lens.?(json).shouldSucceed.foci shouldBe Nil
    }

    it("should ignore shallow missing fields in a composite lens") {
      val lens = ("x" ~> 0).?
      lens(json) shouldBe None.rightNec
    }

    it("should ignore shallow missing indices in a composite lens") {
      val lens = ("g" ~> 99 ~> "d").?
      lens(json) shouldBe None.rightNec
    }

    it("should toString simple optional") {
      99.?.toString shouldBe "99.?"
    }

    it("should toString composite optional") {
      ("g" ~> 99 ~> "d").?.toString shouldBe """("g" ~> 99 ~> "d").?"""
    }
  }

  describe("deep lenses (use cases)") {

    it("should dig items deeply") {
      val lens = ("deep" ~> * ~> *)
      lens(json).map(_.foci) shouldBe
        json
          .field("deep")
          .flatMap(_.asArray.map(_.items))
          .flatMap(_.flatTraverse(_.asArray.map(_.items)))
    }

    it("should dig fields deeply") {
      val lens = ("deep" ~> * ~> * ~> "a")
      lens(json).map(_.foci) shouldBe
        json
          .field("deep")
          .flatMap(_.asArray.map(_.items))
          .flatMap(_.flatTraverse(_.asArray.map(_.items)))
          .flatMap(_.traverse(_.asObject.flatMap(_.field("a"))))
    }

    it("should not work too deeply") {
      (focus ~> "deep" ~> * ~> * ~> * ~> "a")(json).shouldFail
    }

  }

}
