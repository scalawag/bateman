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
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.syntax._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CaseTransformation, PascalCase, SnakeCase}
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields.Explicit
import org.scalawag.bateman.jsonapi.encoding.{IncludeSpec, InvalidIncludePath, UnavailableIncludePath}
import org.scalawag.bateman.jsonapi.encoding.{FieldsSpec, ResourceEncoder}
import HListRelationshipEncoderTest._
import org.scalawag.bateman.jsonapi.generic.Annotations._

object HListRelationshipEncoderTest {
  case class MyRefId(@Id a: String)

  case class MyIdRelationship(@Relationship a: MyRefId)
  case class MyNullableRelationship(@Relationship a: Nullable[MyRefId])
  case class MyListRelationship(@Relationship a: List[MyRefId])
  case class MyOptionIdRelationship(@Relationship a: Option[MyRefId])
  case class MyOptionNullableRelationship(@Relationship a: Option[Nullable[MyRefId]])
  case class MyOptionListRelationship(@Relationship a: Option[List[MyRefId]])
  case class MyIdRelationshipDefaulted(@Relationship a: MyRefId = MyRefId("A"))
  case class MyNullableRelationshipDefaulted(@Relationship a: Nullable[MyRefId] = NotNull(MyRefId("B")))
  case class MyListRelationshipDefaulted(@Relationship a: List[MyRefId] = List(MyRefId("C"), MyRefId("D")))
  case class MyOptionIdRelationshipDefaulted(@Relationship a: Option[MyRefId] = Some(MyRefId("E")))
  case class MyOptionNullableRelationshipDefaulted(
      @Relationship a: Option[Nullable[MyRefId]] = Some(NotNull(MyRefId("F")))
  )
  case class MyOptionNullableRelationshipDefaultedNone(@Relationship a: Option[Nullable[MyRefId]] = None)
  case class MyOptionNullableRelationshipDefaultedSomeNull(
      @Relationship a: Option[Nullable[MyRefId]] = Some(Null)
  )
  case class MyOptionListRelationshipDefaulted(
      @Relationship a: Option[List[MyRefId]] = Some(List(MyRefId("G"), MyRefId("H"), MyRefId("I")))
  )
  case class MyOptionListRelationshipDefaultedNone(@Relationship a: Option[List[MyRefId]] = None)
  case class MyOptionListRelationshipDefaultedSomeNil(@Relationship a: Option[List[MyRefId]] = Some(Nil))

  case class MyLongFieldName(@Relationship multipleWordsHere: MyRefId)
  case class MyTwoFields(@Relationship a: MyRefId, @Relationship b: MyRefId)
  case class MyTwoFieldsDefault(@Relationship a: MyRefId = MyRefId("A"), @Relationship b: MyRefId)
}

class HListRelationshipEncoderTest extends HListEncoderTestBase {
  describe("default config positive cases") {
    import org.scalawag.bateman.jsonapi.generic.auto._

    def encodeTest[A: ResourceEncoder](a: A, expected: JObject)(implicit position: Position): Unit =
      it(s"should encode $a properly") {
        a.toDocument.render shouldBe expected.render
      }

    encodeTest(
      MyIdRelationship(MyRefId("A")),
      json"""
        {
          "data": {
            "type": "MyIdRelationship",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "A"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyNullableRelationship(NotNull(MyRefId("B"))),
      json"""
        {
          "data": {
            "type": "MyNullableRelationship",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "B"
                }
              }
            }
          }
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
      MyListRelationship(Nil),
      json"""
        {
          "data": {
            "type": "MyListRelationship",
            "relationships": {
              "a": {
                "data": []
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyListRelationship(List(MyRefId("C"))),
      json"""
        {
          "data": {
            "type": "MyListRelationship",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "C"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyListRelationship(List(MyRefId("D"), MyRefId("E"))),
      json"""
        {
          "data": {
            "type": "MyListRelationship",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "D"
                  },
                  {
                    "type": "MyRefId",
                    "id": "E"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionIdRelationship(Some(MyRefId("A"))),
      json"""
        {
          "data": {
            "type": "MyOptionIdRelationship",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "A"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionNullableRelationship(Some(NotNull(MyRefId("B")))),
      json"""
        {
          "data": {
            "type": "MyOptionNullableRelationship",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "B"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionNullableRelationship(Some(Null)),
      json"""
        {
          "data": {
            "type": "MyOptionNullableRelationship",
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
      MyOptionListRelationship(Some(Nil)),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationship",
            "relationships": {
              "a": {
                "data": []
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionListRelationship(Some(List(MyRefId("C")))),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationship",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "C"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionListRelationship(Some(List(MyRefId("D"), MyRefId("E")))),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationship",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "D"
                  },
                  {
                    "type": "MyRefId",
                    "id": "E"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionIdRelationship(None),
      json"""
        {
          "data": {
            "type": "MyOptionIdRelationship"
          }
        }
      """
    )

    encodeTest(MyOptionNullableRelationship(None), json"""
      {
        "data": {
          "type": "MyOptionNullableRelationship"
        }
      }
    """)

    encodeTest(
      MyOptionListRelationship(None),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationship"
          }
        }
      """
    )

    encodeTest(
      MyIdRelationshipDefaulted(MyRefId("A")),
      json"""
        {
          "data": {
            "type": "MyIdRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "A"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyIdRelationshipDefaulted(MyRefId("AA")),
      json"""
        {
          "data": {
            "type": "MyIdRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "AA"
                }
              }
            }
          }
        }
    """
    )

    encodeTest(
      MyNullableRelationshipDefaulted(NotNull(MyRefId("B"))),
      json"""
        {
          "data": {
            "type": "MyNullableRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "B"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyNullableRelationshipDefaulted(NotNull(MyRefId("BB"))),
      json"""
        {
          "data": {
            "type": "MyNullableRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "BB"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyNullableRelationshipDefaulted(Null),
      json"""
        {
          "data": {
            "type": "MyNullableRelationshipDefaulted",
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
      MyListRelationshipDefaulted(Nil),
      json"""
        {
          "data": {
            "type": "MyListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": []
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyListRelationshipDefaulted(List(MyRefId("C"))),
      json"""
        {
          "data": {
            "type": "MyListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "C"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyListRelationshipDefaulted(List(MyRefId("D"), MyRefId("E"))),
      json"""
        {
          "data": {
            "type": "MyListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "D"
                  },
                  {
                    "type": "MyRefId",
                    "id": "E"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyListRelationshipDefaulted(List(MyRefId("C"), MyRefId("D"))),
      json"""
        {
          "data": {
            "type": "MyListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "C"
                  },
                  {
                    "type": "MyRefId",
                    "id": "D"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionIdRelationshipDefaulted(Some(MyRefId("A"))),
      json"""
        {
          "data": {
            "type": "MyOptionIdRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "A"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionIdRelationshipDefaulted(Some(MyRefId("E"))),
      json"""
        {
          "data": {
            "type": "MyOptionIdRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "E"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionNullableRelationshipDefaulted(Some(NotNull(MyRefId("B")))),
      json"""
        {
          "data": {
            "type": "MyOptionNullableRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "B"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionNullableRelationshipDefaultedNone(),
      json"""
        {
          "data": {
            "type": "MyOptionNullableRelationshipDefaultedNone"
          }
        }
      """
    )

    encodeTest(
      MyOptionNullableRelationshipDefaultedSomeNull(),
      json"""
        {
          "data": {
            "type": "MyOptionNullableRelationshipDefaultedSomeNull"
          }
        }
      """
    )

    encodeTest(
      MyOptionNullableRelationshipDefaulted(Some(NotNull(MyRefId("F")))),
      json"""
        {
          "data": {
            "type": "MyOptionNullableRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "F"
                }
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionNullableRelationshipDefaulted(Some(Null)),
      json"""
        {
          "data": {
            "type": "MyOptionNullableRelationshipDefaulted",
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
      MyOptionListRelationshipDefaulted(Some(Nil)),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": []
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionListRelationshipDefaulted(Some(List(MyRefId("C")))),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "C"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionListRelationshipDefaulted(Some(List(MyRefId("D"), MyRefId("E")))),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "D"
                  },
                  {
                    "type": "MyRefId",
                    "id": "E"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionListRelationshipDefaulted(Some(List(MyRefId("G"), MyRefId("H"), MyRefId("I")))),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": [
                  {
                    "type": "MyRefId",
                    "id": "G"
                  },
                  {
                    "type": "MyRefId",
                    "id": "H"
                  },
                  {
                    "type": "MyRefId",
                    "id": "I"
                  }
                ]
              }
            }
          }
        }
      """
    )

    encodeTest(
      MyOptionIdRelationshipDefaulted(None),
      json"""
      {
        "data": {
          "type": "MyOptionIdRelationshipDefaulted"
        }
      }
    """
    )

    encodeTest(
      MyOptionNullableRelationshipDefaulted(None),
      json"""
      {
        "data": {
          "type": "MyOptionNullableRelationshipDefaulted"
        }
      }
    """
    )

    encodeTest(
      MyOptionListRelationshipDefaulted(None),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationshipDefaulted"
          }
        }
      """
    )

    encodeTest(
      MyOptionListRelationshipDefaultedNone(),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationshipDefaultedNone"
          }
        }
      """
    )
    encodeTest(
      MyOptionListRelationshipDefaultedSomeNil(),
      json"""
        {
          "data": {
            "type": "MyOptionListRelationshipDefaultedSomeNil"
          }
        }
      """
    )
  }

  describe("alternative config") {
    it("should encode default values when instructed to") {
      implicit val config = Config(encodeDefaultValues = true)
      import org.scalawag.bateman.jsonapi.generic.auto._

      MyIdRelationshipDefaulted(MyRefId("A")) shouldEncodeTo json"""
        {
          "data": {
            "type": "MyIdRelationshipDefaulted",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRefId",
                  "id": "A"
                }
              }
            }
          }
        }
      """
    }

    it("should transform class names when instructed to") {
      implicit val config = Config(classNameMapping = CaseTransformation(PascalCase, SnakeCase))
      import org.scalawag.bateman.jsonapi.generic.auto._

      MyLongFieldName(MyRefId("B")) shouldEncodeTo json"""
        {
          "data": {
            "type": "my_long_field_name",
            "relationships": {
              "multipleWordsHere": {
                "data": {
                  "type": "my_ref_id",
                  "id": "B"
                }
              }
            }
          }
        }
      """
    }

    it("should transform field names when instructed to") {
      implicit val config = Config(fieldNameMapping = CaseTransformation(PascalCase, SnakeCase))
      import org.scalawag.bateman.jsonapi.generic.auto._

      MyLongFieldName(MyRefId("B")) shouldEncodeTo json"""
        {
          "data": {
            "type": "MyLongFieldName",
            "relationships": {
              "multiple_words_here": {
                "data": {
                  "type": "MyRefId",
                  "id": "B"
                }
              }
            }
          }
        }
      """
    }
  }

  describe("fieldsSpec") {
    import org.scalawag.bateman.jsonapi.generic.auto._

    def testCase[A: ResourceEncoder](a: A, fieldsSpec: FieldsSpec, expected: JObject): Unit =
      a.toDocument(IncludeSpec.Never, fieldsSpec).shouldSucceed.render shouldBe expected.render

    it("should include relationship field when told to explicitly") {
      testCase(
        MyTwoFields(MyRefId("A"), MyRefId("B")),
        FieldsSpec(Map("MyTwoFields" -> Explicit("a"))),
        json"""
          {
            "data": {
              "type": "MyTwoFields",
              "relationships": {
                "a": {
                  "data": {
                    "type": "MyRefId",
                    "id": "A"
                  }
                }
              }
            }
          }
        """
      )
    }

    it("should include relationship fields when told to implicitly") {
      testCase(
        MyTwoFields(MyRefId("A"), MyRefId("B")),
        FieldsSpec.All,
        json"""
          {
            "data": {
              "type": "MyTwoFields",
              "relationships": {
                "a": {
                  "data": {
                    "type": "MyRefId",
                    "id": "A"
                  }
                },
                "b": {
                  "data": {
                    "type": "MyRefId",
                    "id": "B"
                  }
                }
              }
            }
          }
        """
      )
    }

    it("should exclude relationship field when told to implicitly") {
      testCase(
        MyTwoFields(MyRefId("A"), MyRefId("B")),
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
        MyTwoFieldsDefault(MyRefId("A"), MyRefId("B")),
        FieldsSpec(Map("MyTwoFieldsDefault" -> Explicit("a"))),
        json"""
          {
            "data": {
              "type": "MyTwoFieldsDefault",
              "relationships": {
                "a": {
                  "data": {
                    "type": "MyRefId",
                    "id": "A"
                  }
                }
              }
            }
          }
        """
      )
    }

    it("should fail for invalid include path") {
      MyIdRelationship(MyRefId("A")).toDocument(IncludeSpec.unsafe("b"), FieldsSpec.All) shouldBe
        InvalidIncludePath("b").leftNec
    }

    it("should fail for non-included include path") {
      MyIdRelationship(MyRefId("A")).toDocument(IncludeSpec.unsafe("a"), FieldsSpec.All) shouldBe
        UnavailableIncludePath("a").leftNec
    }

    it("should fail for non-relationship include path") {
      MyRefId("A").toDocument(IncludeSpec.unsafe("a"), FieldsSpec.All) shouldBe
        InvalidIncludePath("a").leftNec
    }
  }
}
