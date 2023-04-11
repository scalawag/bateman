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

package test.jsonapi.generic.encoding

import cats.syntax.either._
import org.scalactic.source.Position
import org.scalatest.Assertion
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CaseTransformation, PascalCase, SnakeCase}
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields.Explicit
import org.scalawag.bateman.jsonapi.encoding._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import HListIncludedRelationshipWithLidEncoderTest._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.jsonapi.syntax._

object HListIncludedRelationshipWithLidEncoderTest {
  case class MyRef(@Attribute b: Int)

  case class MyIdRelationship(@IncludedRelationship a: MyRef)
  case class MyNullableRelationship(@IncludedRelationship a: Nullable[MyRef])
  case class MyListRelationship(@IncludedRelationship a: List[MyRef])
  case class MyOptionIdRelationship(@IncludedRelationship a: Option[MyRef])
  case class MyOptionNullableRelationship(@IncludedRelationship a: Option[Nullable[MyRef]])
  case class MyOptionListRelationship(@IncludedRelationship a: Option[List[MyRef]])
  case class MyIdRelationshipDefaulted(@IncludedRelationship a: MyRef = MyRef(1))
  case class MyNullableRelationshipDefaulted(@IncludedRelationship a: Nullable[MyRef] = NotNull(MyRef(2)))
  case class MyNullableRelationshipDefaultedNull(@IncludedRelationship a: Nullable[MyRef] = Null)
  case class MyListRelationshipDefaulted(@IncludedRelationship a: List[MyRef] = List(MyRef(3), MyRef(4)))
  case class MyListRelationshipDefaultedNil(@IncludedRelationship a: List[MyRef] = Nil)
  case class MyOptionIdRelationshipDefaulted(@IncludedRelationship a: Option[MyRef] = Some(MyRef(5)))
  case class MyOptionIdRelationshipDefaultedNone(@IncludedRelationship a: Option[MyRef] = None)
  case class MyOptionNullableRelationshipDefaulted(
      @IncludedRelationship a: Option[Nullable[MyRef]] = Some(NotNull(MyRef(6)))
  )
  case class MyOptionNullableRelationshipDefaultedNone(@IncludedRelationship a: Option[Nullable[MyRef]] = None)
  case class MyOptionNullableRelationshipDefaultedSomeNull(
      @IncludedRelationship a: Option[Nullable[MyRef]] = Some(Null)
  )
  case class MyOptionListRelationshipDefaulted(
      @IncludedRelationship a: Option[List[MyRef]] = Some(List(MyRef(7), MyRef(8), MyRef(9)))
  )
  case class MyOptionListRelationshipDefaultedNone(@IncludedRelationship a: Option[List[MyRef]] = None)
  case class MyOptionListRelationshipDefaultedSomeNil(@IncludedRelationship a: Option[List[MyRef]] = Some(Nil))

  case class MyLongFieldName(@IncludedRelationship multipleWordsHere: MyRef)
  case class MyTwoFields(@IncludedRelationship a: MyRef, @IncludedRelationship b: MyRef)
  case class MyTwoFieldsDefault(@IncludedRelationship a: Nullable[MyRef] = Null, @IncludedRelationship b: MyRef)
}

class HListIncludedRelationshipWithLidEncoderTest extends HListEncoderTestBase {
  describe("exhaustive combinations") {
    import org.scalawag.bateman.jsonapi.generic.auto._

    implicit class EncodedJObjectOps(enc: JObject) {
      def relationshipsShouldBeAbsent: Assertion =
        enc.asRootFocus(data ~> relationships.?).shouldSucceed shouldBe None

      def relativeShouldBeNull: Assertion =
        enc.asRootFocus(data ~> relationship("a") ~> data).shouldSucceed.value shouldBe JNull

      def relationshipShouldBeEmpty: Assertion =
        enc.asRootFocus(data ~> relationship("a") ~> data ~> *).shouldSucceed.values shouldBe empty

      def includedShouldBeAbsent: Assertion =
        enc.asRootFocus(included.?).shouldSucceed shouldBe None

      def includedB: Int =
        enc
          .asRootFocus(data ~> relationship("a") ~> data ~> includedRef ~> attribute("b") ~> narrow[JNumber])
          .shouldSucceed
          .value
          .toBigDecimal
          .toIntExact

      def includedBs: List[Int] =
        enc
          .asRootFocus(data ~> relationship("a") ~> data ~> * ~> includedRef ~> attribute("b") ~> narrow[JNumber])
          .shouldSucceed
          .values
          .map(_.toBigDecimal.toIntExact)
    }

    describe("MyIdRelationship") {
      it("should simply encode") {
        val enc = MyIdRelationship(MyRef(1)).toDocument
        enc.includedB shouldBe 1
      }
    }

    describe("MyNullableRelationship") {
      it("should encode NotNull") {
        val enc = MyNullableRelationship(NotNull(MyRef(2))).toDocument
        enc.includedB shouldBe 2
      }

      it("should encode Null") {
        val enc = MyNullableRelationship(Null).toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }
    }

    describe("MyListRelationship") {
      it("should encode zero relatives") {
        val enc = MyListRelationship(Nil).toDocument
        enc.relationshipShouldBeEmpty
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyListRelationship(List(MyRef(3))).toDocument
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyListRelationship(List(MyRef(4), MyRef(5))).toDocument
        enc.includedBs shouldBe List(4, 5)
      }
    }

    describe("MyOptionIdRelationship") {
      it("should encode Some") {
        val enc = MyOptionIdRelationship(Some(MyRef(1))).toDocument
        enc.includedB shouldBe 1
      }

      it("should encode None") {
        val enc = MyOptionIdRelationship(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionNullableRelationship") {
      it("should encode Some[NotNull]") {
        val enc = MyOptionNullableRelationship(Some(NotNull(MyRef(2)))).toDocument
        enc.includedB shouldBe 2
      }

      it("should encode Some[Null]") {
        val enc = MyOptionNullableRelationship(Some(Null)).toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }

      it("should encode None") {
        val enc = MyOptionNullableRelationship(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionListRelationship") {
      it("should encode None") {
        val enc = MyOptionListRelationship(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode zero relatives") {
        val enc = MyOptionListRelationship(Some(Nil)).toDocument
        enc.relationshipShouldBeEmpty
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyOptionListRelationship(Some(List(MyRef(4)))).toDocument
        enc.includedBs shouldBe List(4)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationship(Some(List(MyRef(4), MyRef(5)))).toDocument
        enc.includedBs shouldBe List(4, 5)
      }
    }

    describe("MyIdRelationshipDefaulted") {
      it("should encode non-default value") {
        val enc = MyIdRelationshipDefaulted(MyRef(2)).toDocument
        enc.includedB shouldBe 2
      }

      it("should encode non-empty default value") {
        val enc = MyIdRelationshipDefaulted().toDocument
        enc.includedB shouldBe 1
      }
    }

    describe("MyNullableRelationshipDefaulted") {
      it("should encode non-default value") {
        val enc = MyNullableRelationshipDefaulted(NotNull(MyRef(2))).toDocument
        enc.includedB shouldBe 2
      }

      it("should encode non-empty default value") {
        val enc = MyNullableRelationshipDefaulted().toDocument
        enc.includedB shouldBe 2
      }

      it("should still encode empty default value if configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyNullableRelationshipDefaulted().toDocument
        enc.includedB shouldBe 2
      }

      it("should encode Null") {
        val enc = MyNullableRelationshipDefaulted(Null).toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }
    }

    describe("MyNullableRelationshipDefaultedNull") {
      it("should encode non-default value") {
        val enc = MyNullableRelationshipDefaultedNull(NotNull(MyRef(2))).toDocument
        enc.includedB shouldBe 2
      }

      it("should not encode empty default value") {
        val enc = MyNullableRelationshipDefaultedNull().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode empty default value if configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyNullableRelationshipDefaultedNull(Null).toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }
    }

    describe("MyListRelationshipDefaulted") {
      it("should encode zero relatives") {
        val enc = MyListRelationshipDefaulted(Nil).toDocument
        enc.relationshipShouldBeEmpty
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyListRelationshipDefaulted(List(MyRef(3))).toDocument
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyListRelationshipDefaulted(List(MyRef(4), MyRef(5))).toDocument
        enc.includedBs shouldBe List(4, 5)
      }

      it("should encode non-empty default value") {
        val enc = MyListRelationshipDefaulted().toDocument
        enc.includedBs shouldBe List(3, 4)
      }

      it("should still encode empty default value if configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyListRelationshipDefaulted().toDocument
        enc.includedBs shouldBe List(3, 4)
      }
    }

    describe("MyListRelationshipDefaultedNil") {
      it("should encode one relative") {
        val enc = MyListRelationshipDefaultedNil(List(MyRef(3))).toDocument
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyListRelationshipDefaultedNil(List(MyRef(4), MyRef(5))).toDocument
        enc.includedBs shouldBe List(4, 5)
      }

      it("should not encode zero relatives (empty default)") {
        val enc = MyListRelationshipDefaultedNil().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode zero relatives (empty default) if configured to") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyListRelationshipDefaultedNil().toDocument
        enc.relationshipShouldBeEmpty
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionIdRelationshipDefaulted") {
      it("should encode Some") {
        val enc = MyOptionIdRelationshipDefaulted(Some(MyRef(1))).toDocument
        enc.includedB shouldBe 1
      }

      it("should encode None") {
        val enc = MyOptionIdRelationshipDefaulted(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode non-empty default") {
        val enc = MyOptionIdRelationshipDefaulted().toDocument
        enc.includedB shouldBe 5
      }

      it("should encode zero relatives (empty default) if configured to") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionIdRelationshipDefaulted().toDocument
        enc.includedB shouldBe 5
      }
    }

    describe("MyOptionIdRelationshipDefaultedNone") {
      it("should encode Some") {
        val enc = MyOptionIdRelationshipDefaultedNone(Some(MyRef(1))).toDocument
        enc.includedB shouldBe 1
      }

      it("should not encode None") {
        val enc = MyOptionIdRelationshipDefaultedNone(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should not encode None even when it's the default, empty and we're configured to") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionIdRelationshipDefaultedNone().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionNullableRelationshipDefaulted") {
      it("should encode Some[NotNull]") {
        val enc = MyOptionNullableRelationshipDefaulted(Some(NotNull(MyRef(2)))).toDocument
        enc.includedB shouldBe 2
      }

      it("should encode Some[Null]") {
        val enc = MyOptionNullableRelationshipDefaulted(Some(Null)).toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }

      it("should encode None") {
        val enc = MyOptionNullableRelationshipDefaulted(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode non-empty default") {
        val enc = MyOptionNullableRelationshipDefaulted().toDocument
        enc.includedB shouldBe 6
      }

      it("should still encode non-empty default with configuration option") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionNullableRelationshipDefaulted().toDocument
        enc.includedB shouldBe 6
      }
    }

    describe("MyOptionNullableRelationshipDefaultedNone") {
      it("should encode Some[NotNull]") {
        val enc = MyOptionNullableRelationshipDefaultedNone(Some(NotNull(MyRef(2)))).toDocument
        enc.includedB shouldBe 2
      }

      it("should encode Some[Null]") {
        val enc = MyOptionNullableRelationshipDefaultedNone(Some(Null)).toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }

      it("should not encode None") {
        val enc = MyOptionNullableRelationshipDefaultedNone().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should not encode default None with configuration option") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionNullableRelationshipDefaultedNone().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionNullableRelationshipDefaultedSomeNull") {
      it("should encode Some[NotNull]") {
        val enc = MyOptionNullableRelationshipDefaultedSomeNull(Some(NotNull(MyRef(2)))).toDocument
        enc.includedB shouldBe 2
      }

      it("should not encode None") {
        val enc = MyOptionNullableRelationshipDefaultedSomeNull(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should not encode Some[Null] (empty default)") {
        val enc = MyOptionNullableRelationshipDefaultedSomeNull().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode empty default None when configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionNullableRelationshipDefaultedSomeNull().toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionListRelationshipDefaulted") {
      it("should not encode None") {
        val enc = MyOptionListRelationshipDefaulted(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode zero relatives") {
        val enc = MyOptionListRelationshipDefaulted(Some(Nil)).toDocument
        enc.relationshipShouldBeEmpty
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyOptionListRelationshipDefaulted(Some(List(MyRef(3)))).toDocument
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationshipDefaulted(Some(List(MyRef(4), MyRef(5)))).toDocument
        enc.includedBs shouldBe List(4, 5)
      }

      it("should encode non-empty default value") {
        val enc = MyOptionListRelationshipDefaulted().toDocument
        enc.includedBs shouldBe List(7, 8, 9)
      }

      it("should still encode empty default value if configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionListRelationshipDefaulted().toDocument
        enc.includedBs shouldBe List(7, 8, 9)
      }
    }

    describe("MyOptionListRelationshipDefaultedNone") {
      it("should not encode None") {
        val enc = MyOptionListRelationshipDefaultedNone(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should not encode empty default value of None") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionListRelationshipDefaultedNone().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode zero relatives") {
        val enc = MyOptionListRelationshipDefaultedNone(Some(Nil)).toDocument
        enc.relationshipShouldBeEmpty
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyOptionListRelationshipDefaultedNone(Some(List(MyRef(3)))).toDocument
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationshipDefaultedNone(Some(List(MyRef(4), MyRef(5)))).toDocument
        enc.includedBs shouldBe List(4, 5)
      }
    }

    describe("MyOptionListRelationshipDefaultedSomeNil") {
      it("should not encode None") {
        val enc = MyOptionListRelationshipDefaultedSomeNil(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyOptionListRelationshipDefaultedSomeNil(Some(List(MyRef(3)))).toDocument
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationshipDefaultedSomeNil(Some(List(MyRef(4), MyRef(5)))).toDocument
        enc.includedBs shouldBe List(4, 5)
      }

      it("should not encode zero relatives (empty default)") {
        val enc = MyOptionListRelationshipDefaultedSomeNil().toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode zero relatives (empty default) if configured to") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionListRelationshipDefaultedSomeNil().toDocument
        enc.relationshipShouldBeEmpty
        enc.includedShouldBeAbsent
      }
    }
  }

  describe("name transformations") {
    it("should transform class names when instructed to") {
      implicit val config = Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
      import org.scalawag.bateman.jsonapi.generic.auto._

      val enc = MyLongFieldName(MyRef(2)).toDocument

      enc.asRootFocus(data ~> resourceType).value.shouldSucceed.value shouldBe "my_long_field_name"
    }

    it("should transform field names when instructed to") {
      implicit val config = Config(fieldNameMapping = CaseTransformation(PascalCase, SnakeCase))
      import org.scalawag.bateman.jsonapi.generic.auto._

      val enc = MyLongFieldName(MyRef(2)).toDocument

      enc
        .asRootFocus(
          data ~> relationship("multiple_words_here") ~> data ~> includedRef ~> attribute("b") ~> narrow[JNumber]
        )
        .shouldSucceed
        .value
        .toBigDecimal
        .toIntExact
    }
  }

  describe("fieldsSpec") {
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should include relationship field when told to explicitly") {
      val enc = MyTwoFields(MyRef(1), MyRef(2))
        .toDocument(IncludeSpec.Opportunistically, FieldsSpec(Map("MyTwoFields" -> Explicit("a"))))
        .shouldSucceed

      enc
        .asRootFocus(data ~> relationship("a") ~> data ~> includedRef ~> attribute("b") ~> narrow[JNumber])
        .shouldSucceed
        .value
        .toBigDecimal
        .toIntExact shouldBe 1

      enc
        .asRootFocus(data ~> relationship("b").?)
        .shouldSucceed shouldBe None

    }

    it("should include relationship fields when told to implicitly") {
      val enc = MyTwoFields(MyRef(1), MyRef(2))
        .toDocument(IncludeSpec.Opportunistically, FieldsSpec.All)

      enc
        .asRootFocus(data ~> relationship("a") ~> data ~> includedRef ~> attribute ("b") ~> narrow[JNumber])
        .shouldSucceed
        .value
        .toBigDecimal
        .toIntExact shouldBe 1

      enc
        .asRootFocus(data ~> relationship("b") ~> data ~> includedRef ~> attribute ("b") ~> narrow[JNumber])
        .shouldSucceed
        .value
        .toBigDecimal
        .toIntExact shouldBe 2
    }

    it("should exclude relationship fields when told to implicitly") {
      val enc = MyTwoFields(MyRef(1), MyRef(2)).toDocument(IncludeSpec.Opportunistically, FieldsSpec.None)

      enc.asRootFocus(data ~> relationships.?).shouldSucceed shouldBe None
      enc.asRootFocus(included.?).shouldSucceed shouldBe None
    }

    it("should include relationship explicitly even when it's the default") {
      val enc = MyTwoFieldsDefault(Null, MyRef(2))
        .toDocument(IncludeSpec.Opportunistically, FieldsSpec(Map("MyTwoFieldsDefault" -> Explicit("a"))))
        .shouldSucceed

      enc.asRootFocus(data ~> relationship("a") ~> data).shouldSucceed.value shouldBe JNull
      enc.asRootFocus(data ~> relationship("b").?).shouldSucceed shouldBe None
      enc.asRootFocus(included.?).shouldSucceed shouldBe None
    }

    it("should fail for invalid include path") {
      MyIdRelationship(MyRef(1)).toDocument(IncludeSpec.unsafe("b"), FieldsSpec.All) shouldBe
        InvalidIncludePath("b").leftNec
    }

    it("should fail for non-relationship include path") {
      MyRef(1).toDocument(IncludeSpec.unsafe("a"), FieldsSpec.All) shouldBe
        InvalidIncludePath("a").leftNec
    }
  }
}
