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
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CaseTransformation, PascalCase, SnakeCase}
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields.Explicit
import org.scalawag.bateman.jsonapi.encoding._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import HListIncludedRelationshipEncoderTest._
import org.scalawag.bateman.jsonapi.syntax._

object HListIncludedRelationshipEncoderTest {
  case class MyRef(@Id a: String, @Attribute b: Int)

  case class MyIdRelationship(@IncludedRelationship a: MyRef)
  case class MyNullableRelationship(@IncludedRelationship a: Nullable[MyRef])
  case class MyListRelationship(@IncludedRelationship a: List[MyRef])
  case class MyOptionIdRelationship(@IncludedRelationship a: Option[MyRef])
  case class MyOptionNullableRelationship(@IncludedRelationship a: Option[Nullable[MyRef]])
  case class MyOptionListRelationship(@IncludedRelationship a: Option[List[MyRef]])
  case class MyIdRelationshipDefaulted(@IncludedRelationship a: MyRef = MyRef("A", 1))
  case class MyNullableRelationshipDefaulted(@IncludedRelationship a: Nullable[MyRef] = NotNull(MyRef("B", 2)))
  case class MyNullableRelationshipDefaultedNull(@IncludedRelationship a: Nullable[MyRef] = Null)
  case class MyListRelationshipDefaulted(@IncludedRelationship a: List[MyRef] = List(MyRef("C", 3), MyRef("D", 4)))
  case class MyListRelationshipDefaultedNil(@IncludedRelationship a: List[MyRef] = Nil)
  case class MyOptionIdRelationshipDefaulted(@IncludedRelationship a: Option[MyRef] = Some(MyRef("E", 5)))
  case class MyOptionIdRelationshipDefaultedNone(@IncludedRelationship a: Option[MyRef] = None)
  case class MyOptionNullableRelationshipDefaulted(
      @IncludedRelationship a: Option[Nullable[MyRef]] = Some(NotNull(MyRef("F", 6)))
  )
  case class MyOptionNullableRelationshipDefaultedNone(@IncludedRelationship a: Option[Nullable[MyRef]] = None)
  case class MyOptionNullableRelationshipDefaultedSomeNull(
      @IncludedRelationship a: Option[Nullable[MyRef]] = Some(Null)
  )
  case class MyOptionListRelationshipDefaulted(
      @IncludedRelationship a: Option[List[MyRef]] = Some(List(MyRef("G", 7), MyRef("H", 8), MyRef("I", 9)))
  )
  case class MyOptionListRelationshipDefaultedNone(@IncludedRelationship a: Option[List[MyRef]] = None)
  case class MyOptionListRelationshipDefaultedSomeNil(@IncludedRelationship a: Option[List[MyRef]] = Some(Nil))

  case class MyLongFieldName(@IncludedRelationship multipleWordsHere: MyRef)
  case class MyTwoFields(@IncludedRelationship a: MyRef, @IncludedRelationship b: MyRef)
  case class MyTwoFieldsDefault(@IncludedRelationship a: Nullable[MyRef] = Null, @IncludedRelationship b: MyRef)
}

class HListIncludedRelationshipEncoderTest extends HListEncoderTestBase {
  describe("exhaustive combinations") {
    import org.scalawag.bateman.jsonapi.generic.auto._

    implicit class EncodedJObjectOps(enc: JObject) {
      def relationshipsShouldBeAbsent: Assertion =
        enc.asRootFocus(data ~> relationships.?).shouldSucceed.foci shouldBe None

      def relativeShouldBeNull: Assertion =
        enc.asRootFocus(data ~> relationship("a") ~> data).shouldSucceed.value shouldBe JNull
      def relativeId: String =
        enc.asRootFocus(data ~> relationship("a") ~> data ~> id).shouldSucceed.value.value
      def relativeIds: List[String] =
        enc.asRootFocus(data ~> relationship("a") ~> data ~> * ~> id).shouldSucceed.values.map(_.value)

      def includedShouldBeAbsent: Assertion =
        enc.asRootFocus(included.?).shouldSucceed.foci shouldBe None

      def includedIds: List[String] =
        enc.asRootFocus(included ~> * ~> id).shouldSucceed.values.map(_.value)

      def includedBs: List[Int] =
        enc
          .asRootFocus(included ~> * ~> attribute("b") ~> narrow[JNumber])
          .shouldSucceed
          .values
          .map(_.toBigDecimal.toIntExact)
    }

    describe("MyIdRelationship") {
      it("should simply encode") {
        val enc = MyIdRelationship(MyRef("A", 1)).toDocument
        enc.relativeId shouldBe "A"
        enc.includedIds shouldBe List("A")
        enc.includedBs shouldBe List(1)
      }
    }

    describe("MyNullableRelationship") {
      it("should encode NotNull") {
        val enc = MyNullableRelationship(NotNull(MyRef("B", 2))).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
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
        enc.relativeIds shouldBe Nil
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyListRelationship(List(MyRef("C", 3))).toDocument
        enc.relativeIds shouldBe List("C")
        enc.includedIds shouldBe List("C")
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyListRelationship(List(MyRef("D", 4), MyRef("E", 5))).toDocument
        enc.relativeIds shouldBe List("D", "E")
        enc.includedIds shouldBe List("D", "E")
        enc.includedBs shouldBe List(4, 5)
      }
    }

    describe("MyOptionIdRelationship") {
      it("should encode Some") {
        val enc = MyOptionIdRelationship(Some(MyRef("A", 1))).toDocument
        enc.relativeId shouldBe "A"
        enc.includedIds shouldBe List("A")
        enc.includedBs shouldBe List(1)
      }

      it("should encode None") {
        val enc = MyOptionIdRelationship(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionNullableRelationship") {
      it("should encode Some[NotNull]") {
        val enc = MyOptionNullableRelationship(Some(NotNull(MyRef("B", 2)))).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
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
        enc.relativeIds shouldBe Nil
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyOptionListRelationship(Some(List(MyRef("D", 4)))).toDocument
        enc.relativeIds shouldBe List("D")
        enc.includedIds shouldBe List("D")
        enc.includedBs shouldBe List(4)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationship(Some(List(MyRef("D", 4), MyRef("E", 5)))).toDocument
        enc.relativeIds shouldBe List("D", "E")
        enc.includedIds shouldBe List("D", "E")
        enc.includedBs shouldBe List(4, 5)
      }
    }

    describe("MyIdRelationshipDefaulted") {
      it("should encode non-default value") {
        val enc = MyIdRelationshipDefaulted(MyRef("B", 2)).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
      }

      it("should encode non-empty default value") {
        val enc = MyIdRelationshipDefaulted().toDocument
        enc.relativeId shouldBe "A"
        enc.includedIds shouldBe List("A")
        enc.includedBs shouldBe List(1)
      }
    }

    describe("MyNullableRelationshipDefaulted") {
      it("should encode non-default value") {
        val enc = MyNullableRelationshipDefaulted(NotNull(MyRef("B", 2))).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
      }

      it("should encode non-empty default value") {
        val enc = MyNullableRelationshipDefaulted().toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
      }

      it("should still encode empty default value if configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyNullableRelationshipDefaulted().toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
      }

      it("should encode Null") {
        val enc = MyNullableRelationshipDefaulted(Null).toDocument
        enc.relativeShouldBeNull
        enc.includedShouldBeAbsent
      }
    }

    describe("MyNullableRelationshipDefaultedNull") {
      it("should encode non-default value") {
        val enc = MyNullableRelationshipDefaultedNull(NotNull(MyRef("B", 2))).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
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
        enc.relativeIds shouldBe Nil
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyListRelationshipDefaulted(List(MyRef("C", 3))).toDocument
        enc.relativeIds shouldBe List("C")
        enc.includedIds shouldBe List("C")
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyListRelationshipDefaulted(List(MyRef("D", 4), MyRef("E", 5))).toDocument
        enc.relativeIds shouldBe List("D", "E")
        enc.includedIds shouldBe List("D", "E")
        enc.includedBs shouldBe List(4, 5)
      }

      it("should encode non-empty default value") {
        val enc = MyListRelationshipDefaulted().toDocument
        enc.relativeIds shouldBe List("C", "D")
        enc.includedIds shouldBe List("C", "D")
        enc.includedBs shouldBe List(3, 4)
      }

      it("should still encode empty default value if configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyListRelationshipDefaulted().toDocument
        enc.relativeIds shouldBe List("C", "D")
        enc.includedIds shouldBe List("C", "D")
        enc.includedBs shouldBe List(3, 4)
      }
    }

    describe("MyListRelationshipDefaultedNil") {
      it("should encode one relative") {
        val enc = MyListRelationshipDefaultedNil(List(MyRef("C", 3))).toDocument
        enc.relativeIds shouldBe List("C")
        enc.includedIds shouldBe List("C")
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyListRelationshipDefaultedNil(List(MyRef("D", 4), MyRef("E", 5))).toDocument
        enc.relativeIds shouldBe List("D", "E")
        enc.includedIds shouldBe List("D", "E")
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
        enc.relativeIds shouldBe Nil
        enc.includedShouldBeAbsent
      }
    }

    describe("MyOptionIdRelationshipDefaulted") {
      it("should encode Some") {
        val enc = MyOptionIdRelationshipDefaulted(Some(MyRef("A", 1))).toDocument
        enc.relativeId shouldBe "A"
        enc.includedIds shouldBe List("A")
        enc.includedBs shouldBe List(1)
      }

      it("should encode None") {
        val enc = MyOptionIdRelationshipDefaulted(None).toDocument
        enc.relationshipsShouldBeAbsent
        enc.includedShouldBeAbsent
      }

      it("should encode non-empty default") {
        val enc = MyOptionIdRelationshipDefaulted().toDocument
        enc.relativeId shouldBe "E"
        enc.includedIds shouldBe List("E")
        enc.includedBs shouldBe List(5)
      }

      it("should encode zero relatives (empty default) if configured to") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionIdRelationshipDefaulted().toDocument
        enc.relativeId shouldBe "E"
        enc.includedIds shouldBe List("E")
        enc.includedBs shouldBe List(5)
      }
    }

    describe("MyOptionIdRelationshipDefaultedNone") {
      it("should encode Some") {
        val enc = MyOptionIdRelationshipDefaultedNone(Some(MyRef("A", 1))).toDocument
        enc.relativeId shouldBe "A"
        enc.includedIds shouldBe List("A")
        enc.includedBs shouldBe List(1)
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
        val enc = MyOptionNullableRelationshipDefaulted(Some(NotNull(MyRef("B", 2)))).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
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
        enc.relativeId shouldBe "F"
        enc.includedIds shouldBe List("F")
        enc.includedBs shouldBe List(6)
      }

      it("should still encode non-empty default with configuration option") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionNullableRelationshipDefaulted().toDocument
        enc.relativeId shouldBe "F"
        enc.includedIds shouldBe List("F")
        enc.includedBs shouldBe List(6)
      }
    }

    describe("MyOptionNullableRelationshipDefaultedNone") {
      it("should encode Some[NotNull]") {
        val enc = MyOptionNullableRelationshipDefaultedNone(Some(NotNull(MyRef("B", 2)))).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
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
        val enc = MyOptionNullableRelationshipDefaultedSomeNull(Some(NotNull(MyRef("B", 2)))).toDocument
        enc.relativeId shouldBe "B"
        enc.includedIds shouldBe List("B")
        enc.includedBs shouldBe List(2)
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
        enc.relativeIds shouldBe Nil
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyOptionListRelationshipDefaulted(Some(List(MyRef("C", 3)))).toDocument
        enc.relativeIds shouldBe List("C")
        enc.includedIds shouldBe List("C")
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationshipDefaulted(Some(List(MyRef("D", 4), MyRef("E", 5)))).toDocument
        enc.relativeIds shouldBe List("D", "E")
        enc.includedIds shouldBe List("D", "E")
        enc.includedBs shouldBe List(4, 5)
      }

      it("should encode non-empty default value") {
        val enc = MyOptionListRelationshipDefaulted().toDocument
        enc.relativeIds shouldBe List("G", "H", "I")
        enc.includedIds shouldBe List("G", "H", "I")
        enc.includedBs shouldBe List(7, 8, 9)
      }

      it("should still encode empty default value if configured") {
        implicit val config: Config = Config(encodeDefaultValues = true)
        val enc = MyOptionListRelationshipDefaulted().toDocument
        enc.relativeIds shouldBe List("G", "H", "I")
        enc.includedIds shouldBe List("G", "H", "I")
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
        enc.relativeIds shouldBe Nil
        enc.includedShouldBeAbsent
      }

      it("should encode one relative") {
        val enc = MyOptionListRelationshipDefaultedNone(Some(List(MyRef("C", 3)))).toDocument
        enc.relativeIds shouldBe List("C")
        enc.includedIds shouldBe List("C")
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationshipDefaultedNone(Some(List(MyRef("D", 4), MyRef("E", 5)))).toDocument
        enc.relativeIds shouldBe List("D", "E")
        enc.includedIds shouldBe List("D", "E")
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
        val enc = MyOptionListRelationshipDefaultedSomeNil(Some(List(MyRef("C", 3)))).toDocument
        enc.relativeIds shouldBe List("C")
        enc.includedIds shouldBe List("C")
        enc.includedBs shouldBe List(3)
      }

      it("should encode two relatives") {
        val enc = MyOptionListRelationshipDefaultedSomeNil(Some(List(MyRef("D", 4), MyRef("E", 5)))).toDocument
        enc.relativeIds shouldBe List("D", "E")
        enc.includedIds shouldBe List("D", "E")
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
        enc.relativeIds shouldBe Nil
        enc.includedShouldBeAbsent
      }
    }
  }

  describe("full output validation") {
    import org.scalawag.bateman.jsonapi.generic.auto._

    def encodeTest[A: ResourceEncoder](a: A, expected: JObject)(implicit position: Position): Unit =
      it(s"should encode $a properly") {
        a.toDocument.render shouldBe expected.render
      }

    encodeTest(
      MyIdRelationship(MyRef("A", 1)),
      json"""
        {
          "data": {
            "type": "MyIdRelationship",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRef",
                  "id": "A"
                }
              }
            }
          },
          "included": [
            {
              "type": "MyRef",
              "id": "A",
              "attributes": {
                "b": 1
              }
            }
          ]
        }
      """
    )

    encodeTest(
      MyNullableRelationship(NotNull(MyRef("B", 2))),
      json"""
        {
          "data": {
            "type": "MyNullableRelationship",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRef",
                  "id": "B"
                }
              }
            }
          },
          "included": [
            {
              "type": "MyRef",
              "id": "B",
              "attributes": {
                "b": 2
              }
            }
          ]
        }
      """
    )

    encodeTest(
      MyNullableRelationship(Null),
      json"""
        {
          "data": {
            "type": "MyNullableRelationship",
            "relationships": {
              "a": {
                "data": null
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyListRelationship(List(MyRef("D", 4), MyRef("E", 5))),
      json"""
        {
          "data": {
            "type": "MyListRelationship",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRef",
                    "id": "D"
                  },
                  {
                    "type": "MyRef",
                    "id": "E"
                  }
                ]
              }
            }
          },
          "included": [
            {
              "type": "MyRef",
              "id": "D",
              "attributes": {
                "b": 4
              }
            },
            {
              "type": "MyRef",
              "id": "E",
              "attributes": {
                "b": 5
              }
            }
          ]
        }
      """
    )
  }

  describe("name transformations") {
    it("should transform class names when instructed to") {
      implicit val config = Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
      import org.scalawag.bateman.jsonapi.generic.auto._

      MyLongFieldName(MyRef("B", 2)) shouldEncodeTo json"""
        {
          "data": {
            "type": "my_long_field_name",
            "relationships": {
              "multipleWordsHere": {
                "data": {
                  "type": "my_ref",
                  "id": "B"
                }
              }
            }
          },
          "included": [
            {
              "type": "my_ref",
              "id": "B",
              "attributes": {
                "b": 2
              }
            }
          ]
        }
      """
    }

    it("should transform field names when instructed to") {
      implicit val config = Config(fieldNameMapping = CaseTransformation(PascalCase, SnakeCase))
      import org.scalawag.bateman.jsonapi.generic.auto._

      MyLongFieldName(MyRef("B", 2)) shouldEncodeTo json"""
        {
          "data": {
            "type": "MyLongFieldName",
            "relationships": {
              "multiple_words_here": {
                "data": {
                  "type": "MyRef",
                  "id": "B"
                }
              }
            }
          },
          "included": [
            {
              "type": "MyRef",
              "id": "B",
              "attributes": {
                "b": 2
              }
            }
          ]
        }
      """
    }
  }

  describe("fieldsSpec") {
    import org.scalawag.bateman.jsonapi.generic.auto._

    def testCase[A: ResourceEncoder](a: A, fieldsSpec: FieldsSpec, expected: JObject): Unit =
      a.toDocument(IncludeSpec.Opportunistically, fieldsSpec).shouldSucceed.render shouldBe expected.render

    it("should include relationship field when told to explicitly") {
      testCase(
        MyTwoFields(MyRef("A", 1), MyRef("B", 2)),
        FieldsSpec(Map("MyTwoFields" -> Explicit("a"))),
        json"""
          {
            "data": {
              "type": "MyTwoFields",
              "relationships": {
                "a": {
                  "data": {
                    "type": "MyRef",
                    "id": "A"
                  }
                }
              }
            },
            "included": [
              {
                "type": "MyRef",
                "id": "A",
                "attributes": {
                  "b": 1
                }
              }
            ]
          }
        """
      )
    }

    it("should include relationship fields when told to implicitly") {
      testCase(
        MyTwoFields(MyRef("A", 1), MyRef("B", 2)),
        FieldsSpec.All,
        json"""
          {
            "data": {
              "type": "MyTwoFields",
              "relationships": {
                "a": {
                  "data": {
                    "type": "MyRef",
                    "id": "A"
                  }
                },
                "b": {
                  "data": {
                    "type": "MyRef",
                    "id": "B"
                  }
                }
              }
            },
            "included": [
              {
                "type": "MyRef",
                "id": "A",
                "attributes": {
                  "b": 1
                }
              },
              {
                "type": "MyRef",
                "id": "B",
                "attributes": {
                  "b": 2
                }
              }
            ]
          }
        """
      )
    }

    it("should exclude relationship field when told to implicitly") {
      testCase(
        MyTwoFields(MyRef("A", 1), MyRef("B", 2)),
        FieldsSpec.None,
        json"""
          {
            "data": {
              "type": "MyTwoFields"
            }
          }
        """
      )
    }

    it("should include relationship explicitly even when it's the default") {
      testCase(
        MyTwoFieldsDefault(Null, MyRef("B", 2)),
        FieldsSpec(Map("MyTwoFieldsDefault" -> Explicit("a"))),
        json"""
          {
            "data": {
              "type": "MyTwoFieldsDefault",
              "relationships": {
                "a": {
                  "data": null
                }
              }
            }
          }
        """
      )
    }

    it("should fail for invalid include path") {
      MyIdRelationship(MyRef("A", 1)).toDocument(IncludeSpec.unsafe("b"), FieldsSpec.All) shouldBe
        InvalidIncludePath("b").leftNec
    }

    it("should fail for non-relationship include path") {
      MyRef("A", 1).toDocument(IncludeSpec.unsafe("a"), FieldsSpec.All) shouldBe
        InvalidIncludePath("a").leftNec
    }
  }
}
