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

package test.jsonapi.generic.decoding

import cats.data.NonEmptyChain
import cats.syntax.parallel._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.MissingIncludedResourceObject
import org.scalawag.bateman.jsonapi.encoding.Inclusions
import org.scalawag.bateman.jsonapi.generic.Annotations._
import HListIncludedRelationshipWithLidDecoderTest._
import org.scalawag.bateman.jsonapi.generic.semiauto.unchecked._
import org.scalawag.bateman.jsonapi.lens._

object HListIncludedRelationshipWithLidDecoderTest {
  case class MyRefObj(@Attribute b: String)

  object MyRefObj {
    implicit val refDecoder: JObjectDecoder[MyRefObj] = deriveResourceDecoderForCaseClass[MyRefObj]()
  }

  object MyIdRelationship {
    case class MyClass(@IncludedRelationship a: MyRefObj)
  }

  object MyNullableRelationship {
    case class MyClass(@IncludedRelationship a: Nullable[MyRefObj])
  }

  object MyListRelationship {
    case class MyClass(@IncludedRelationship a: List[MyRefObj])
  }

  object MyOptionIdRelationship {
    case class MyClass(@IncludedRelationship a: Option[MyRefObj])
  }

  object MyOptionNullableRelationship {
    case class MyClass(@IncludedRelationship a: Option[Nullable[MyRefObj]])
  }

  object MyOptionListRelationship {
    case class MyClass(@IncludedRelationship a: Option[List[MyRefObj]])
  }

  object MyIdRelationshipDefaulted {
    case class MyClass(@IncludedRelationship a: MyRefObj = MyRefObj("Ab"))
  }

  object MyNullableRelationshipDefaulted {
    case class MyClass(@IncludedRelationship a: Nullable[MyRefObj] = NotNull(MyRefObj("Bb")))
  }

  object MyListRelationshipDefaulted {
    case class MyClass(@IncludedRelationship a: List[MyRefObj] = List(MyRefObj("Cb"), MyRefObj("Db")))
  }

  object MyOptionIdRelationshipDefaulted {
    case class MyClass(@IncludedRelationship a: Option[MyRefObj] = Some(MyRefObj("Eb")))
  }

  object MyOptionNullableRelationshipDefaulted {
    case class MyClass(@IncludedRelationship a: Option[Nullable[MyRefObj]] = Some(NotNull(MyRefObj("Fb"))))
  }

  object MyOptionListRelationshipDefaulted {
    case class MyClass(
        @IncludedRelationship a: Option[List[MyRefObj]] = Some(
          List(MyRefObj("Gb"), MyRefObj("Hb"), MyRefObj("Ib"))
        )
    )
  }
}

class HListIncludedRelationshipWithLidDecoderTest extends HListDecoderTestBase {
  private val resourceIsAnArray = Input(jsona"[]", focus)

  private val resourceIsAnEmptyObject = Input(json"""
    {
      "type": "MyClass"
    }
  """, focus ~> narrow[JObject])

  private val relationshipsIsAString = Input(json"""
    {
      "type": "MyClass",
      "relationships": "foo"
    }
  """, focus ~> "relationships")

  private val relationshipsIsAnEmptyObject = Input(json"""
    {
      "type": "MyClass",
      "relationships": {}
    }
  """, relationships)

  private val relationshipIsAString = Input(json"""
    {
      "type": "MyClass",
      "relationships": {
        "a": true
      }
    }
  """, relationships ~> "a")

  private val relationshipIsNull = Input(json"""
    {
      "type": "MyClass",
      "relationships": {
        "a": null
      }
    }
  """, relationships ~> "a")

  private val relationshipIsAnEmptyObject = Input(json"""
    {
      "type": "MyClass",
      "relationships": {
        "a": {}
      }
    }
  """, relationship("a"))

  private val relationshipDataIsBoolean = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": true
            }
          }
        }
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JBoolean],
    data
  )

  private val relationshipDataIsNull = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": null
            }
          }
        }
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JNull],
    data
  )

  private val relationshipDatumIsMissingId = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": {
                "type": "MyRefObj"
              }
            }
          }
        }
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JObject],
    data
  )

  private val relationshipDatumIsIncluded = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": {
                "type": "MyRefObj",
                "lid": "J"
              }
            }
          }
        },
        "included": [
          {
            "type": "MyRefObj",
            "lid": "J",
            "attributes": {
              "b": "Jb"
            }
          }
        ]
      }
    """,
    data ~> relationship("a") ~> data,
    data
  )

  private val relationshipDataAreEmpty = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": []
            }
          }
        }
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JArray],
    data
  )

  private val relationshipDataAreMissingIds = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": [
                {
                  "type": "MyRefObj"
                },
                {
                  "type": "MyRefObj"
                }
              ]
            }
          }
        }
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JArray],
    data
  )

  private val relationshipDataAreIncluded = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": [
                {
                  "type": "MyRefObj",
                  "lid": "K"
                },
                {
                  "type": "MyRefObj",
                  "lid": "L"
                }
              ]
            }
          }
        },
        "included": [
          {
            "type": "MyRefObj",
            "lid": "K",
            "attributes": {
              "b": "Kb"
            }
          },
          {
            "type": "MyRefObj",
            "lid": "L",
            "attributes": {
              "b": "Lb"
            }
          }
        ]
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JArray],
    data
  )

  private val relationshipDataHasSomeBadTypes = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": [
                null,
                {
                  "type": "MyRefObj",
                  "lid": "M"
                },
                true
              ]
            }
          }
        },
        "included": [
          {
            "type": "MyRefObj",
            "lid": "M",
            "attributes": {
              "b": "Mb"
            }
          }
        ]
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JArray],
    data
  )

  private val documentIncludesAreMissing = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": {
                "type": "MyRefObj",
                "lid": "N"
              }
            }
          }
        }
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JObject],
    data
  )

  private val documentIncludesAreAnObject = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": {
                "type": "MyRefObj",
                "lid": "N"
              }
            }
          }
        },
        "included" : {}
      }
    """,
    focus ~> "included",
    data
  )

  private val relationshipDatumIsNotIncluded = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": {
                "type": "MyRefObj",
                "lid": "N"
              }
            }
          }
        },
        "included": []
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JObject],
    data
  )
  private val missingIncludedObjectRef =
    relationshipDatumIsNotIncluded
      .json(data ~> relationship("a") ~> data)
      .decode[Inclusions.Key]
      .shouldSucceed

  private val relationshipDataAreNotIncluded = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": [
                {
                  "type": "MyRefObj",
                  "lid": "O"
                },
                {
                  "type": "MyRefObj",
                  "lid": "P"
                }
              ]
            }
          }
        },
        "included": []
      }
    """,
    data ~> relationship("a") ~> data,
    data
  )

  def invalidJsonApiStructure[A: JObjectDecoder](): Unit = {
    resourceIsAnArray.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipsIsAString.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipIsAString.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipIsNull.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipIsAnEmptyObject.failsWith[A](MissingField(_, "data"))
  }

  def casesForCardinalityId[A: JObjectDecoder](): Unit = {
    relationshipDataIsBoolean.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipDataIsNull.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipDataAreEmpty.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipDataAreMissingIds.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipDataAreIncluded.failsWith[A](JsonTypeMismatch(_, JObject))
    relationshipDataHasSomeBadTypes.failsWith[A](JsonTypeMismatch(_, JObject))
    documentIncludesAreMissing.failsWith[A](MissingIncludedResourceObject(_, missingIncludedObjectRef))
    documentIncludesAreAnObject.failsWith[A](JsonTypeMismatch(_, JArray))
    relationshipDatumIsMissingId.failsWith[A](MissingField(_, "id", "lid"))
    relationshipDatumIsNotIncluded.failsWith[A](MissingIncludedResourceObject(_, missingIncludedObjectRef))
    relationshipDataAreNotIncluded.failsWith[A](JsonTypeMismatch(_, JObject))
  }

  def casesForCardinalityNullable[A: JObjectDecoder](): Unit = {
    relationshipDataIsBoolean.failsWith[A](JsonTypeMismatch(_, JNull, JObject))
    relationshipDataAreEmpty.failsWith[A](JsonTypeMismatch(_, JNull, JObject))
    relationshipDataAreMissingIds.failsWith[A](JsonTypeMismatch(_, JNull, JObject))
    relationshipDataAreIncluded.failsWith[A](JsonTypeMismatch(_, JNull, JObject))
    relationshipDataHasSomeBadTypes.failsWith[A](JsonTypeMismatch(_, JNull, JObject))
    documentIncludesAreMissing.failsWith[A](MissingIncludedResourceObject(_, missingIncludedObjectRef))
    documentIncludesAreAnObject.failsWith[A](JsonTypeMismatch(_, JArray))
    relationshipDatumIsMissingId.failsWith[A](MissingField(_, "id", "lid"))
    relationshipDatumIsNotIncluded.failsWith[A](MissingIncludedResourceObject(_, missingIncludedObjectRef))
    relationshipDataAreNotIncluded.failsWith[A](JsonTypeMismatch(_, JNull, JObject))
  }

  def casesForCardinalityList[A: JObjectDecoder](): Unit = {
    relationshipDataIsBoolean.failsWith[A](JsonTypeMismatch(_, JArray))
    relationshipDataIsNull.failsWith[A](JsonTypeMismatch(_, JArray))
    relationshipDatumIsMissingId.failsWith[A](JsonTypeMismatch(_, JArray))
    relationshipDatumIsIncluded.failsWith[A](JsonTypeMismatch(_, JArray))

    def badRefType(in: JFocus[JAny]) =
      JsonTypeMismatch(in(root ~> data ~> relationship("a") ~> data).shouldSucceed, JArray)

    def missingIncludedError(in: JFocus[JAny]) =
      (in.asObject, in.decode[Inclusions.Key]).parMapN(MissingIncludedResourceObject.apply).shouldSucceed

    def missingIncludedObjectFromArrayRef(in: JFocus[JAny]) =
      NonEmptyChain(
        missingIncludedError(in(0).shouldSucceed),
        missingIncludedError(in(1).shouldSucceed)
      )

    documentIncludesAreMissing.failsWith[A](badRefType)
    documentIncludesAreAnObject.failsWith[A](badRefType)
    relationshipDatumIsNotIncluded.failsWith[A](JsonTypeMismatch(_, JArray))
    relationshipDataAreNotIncluded.failsWithMultiple[A](missingIncludedObjectFromArrayRef)
  }

  describe("MyIdRelationship") {
    import MyIdRelationship._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityId[MyClass]()
    resourceIsAnEmptyObject.failsWith[MyClass](MissingField(_, "relationships"))
    relationshipsIsAnEmptyObject.failsWith[MyClass](MissingField(_, "a"))
    relationshipDatumIsIncluded.succeedsWith(MyClass(MyRefObj("Jb")))
  }

  describe("MyNullableRelationship") {
    import MyNullableRelationship._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityNullable[MyClass]()
    resourceIsAnEmptyObject.failsWith[MyClass](MissingField(_, "relationships"))
    relationshipsIsAnEmptyObject.failsWith[MyClass](MissingField(_, "a"))
    relationshipDataIsNull.succeedsWith(MyClass(Null))
    relationshipDatumIsIncluded.succeedsWith(MyClass(NotNull(MyRefObj("Jb"))))
  }

  describe("MyListRelationship") {
    import MyListRelationship._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityList[MyClass]()
    resourceIsAnEmptyObject.failsWith[MyClass](MissingField(_, "relationships"))
    relationshipsIsAnEmptyObject.failsWith[MyClass](MissingField(_, "a"))
    relationshipDataAreEmpty.succeedsWith(MyClass(Nil))
    relationshipDataAreMissingIds.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id", "lid"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id", "lid")
      )
    }
    relationshipDataAreIncluded.succeedsWith(MyClass(List(MyRefObj("Kb"), MyRefObj("Lb"))))
    relationshipDataHasSomeBadTypes.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }

  describe("MyOptionIdRelationship") {
    import MyOptionIdRelationship._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityId[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipDatumIsIncluded.succeedsWith(MyClass(Some(MyRefObj("Jb"))))
  }

  describe("MyOptionNullableRelationship") {
    import MyOptionNullableRelationship._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityNullable[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipDataIsNull.succeedsWith(MyClass(Some(Null)))
    relationshipDatumIsIncluded.succeedsWith(MyClass(Some(NotNull(MyRefObj("Jb")))))
  }

  describe("MyOptionListRelationship") {
    import MyOptionListRelationship._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityList[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipDataAreEmpty.succeedsWith(MyClass(Some(Nil)))
    relationshipDataAreMissingIds.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id", "lid"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id", "lid")
      )
    }
    relationshipDataAreIncluded.succeedsWith(MyClass(Some(List(MyRefObj("Kb"), MyRefObj("Lb")))))
    relationshipDataHasSomeBadTypes.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }

  describe("MyIdRelationshipDefaulted") {
    import MyIdRelationshipDefaulted._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityId[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(MyRefObj("Ab")))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(MyRefObj("Ab")))
    relationshipDatumIsIncluded.succeedsWith(MyClass(MyRefObj("Jb")))
  }

  describe("MyNullableRelationshipDefaulted") {
    import MyNullableRelationshipDefaulted._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityNullable[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(NotNull(MyRefObj("Bb"))))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(NotNull(MyRefObj("Bb"))))
    relationshipDataIsNull.succeedsWith(MyClass(Null))
    relationshipDatumIsIncluded.succeedsWith(MyClass(NotNull(MyRefObj("Jb"))))
  }

  describe("MyListRelationshipDefaulted") {
    import MyListRelationshipDefaulted._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityList[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(List(MyRefObj("Cb"), MyRefObj("Db"))))
    relationshipsIsAnEmptyObject.succeedsWith(
      MyClass(List(MyRefObj("Cb"), MyRefObj("Db")))
    )
    relationshipDataAreEmpty.succeedsWith(MyClass(Nil))
    relationshipDataAreMissingIds.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id", "lid"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id", "lid")
      )
    }
    relationshipDataAreIncluded.succeedsWith(
      MyClass(List(MyRefObj("Kb"), MyRefObj("Lb")))
    )
    relationshipDataHasSomeBadTypes.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }

  describe("MyOptionIdRelationshipDefaulted") {
    import MyOptionIdRelationshipDefaulted._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityId[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipDatumIsIncluded.succeedsWith(MyClass(Some(MyRefObj("Jb"))))
  }

  describe("MyOptionNullableRelationshipDefaulted") {
    import MyOptionNullableRelationshipDefaulted._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityNullable[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipDataIsNull.succeedsWith(MyClass(Some(Null)))
    relationshipDatumIsIncluded.succeedsWith(MyClass(Some(NotNull(MyRefObj("Jb")))))
  }

  describe("MyOptionListRelationshipDefaulted") {
    import MyOptionListRelationshipDefaulted._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    invalidJsonApiStructure[MyClass]()
    casesForCardinalityList[MyClass]()
    resourceIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipsIsAnEmptyObject.succeedsWith(MyClass(None))
    relationshipDataAreEmpty.succeedsWith(MyClass(Some(Nil)))
    relationshipDataAreMissingIds.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id", "lid"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id", "lid")
      )
    }
    relationshipDataAreIncluded.succeedsWith(MyClass(Some(List(MyRefObj("Kb"), MyRefObj("Lb")))))
    relationshipDataHasSomeBadTypes.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }
}
