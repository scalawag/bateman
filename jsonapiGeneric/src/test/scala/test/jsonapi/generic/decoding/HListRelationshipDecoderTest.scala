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
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.jsonapi.generic.Annotations.{Id, Relationship}
import HListRelationshipDecoderTest._
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.jsonapi.lens._

object HListRelationshipDecoderTest {

  case class MyRefId(@Id a: String)

//  implicit val decoderForMyRefId = deriveResourceDecoderForCaseClass[MyRefId]()

  object MyIdRelationship {
    case class MyClass(@Relationship a: MyRefId)
  }

  object MyNullableRelationship {
    case class MyClass(@Relationship a: Nullable[MyRefId])
  }

  object MyListRelationship {
    case class MyClass(@Relationship a: List[MyRefId])
  }

  object MyOptionIdRelationship {
    case class MyClass(@Relationship a: Option[MyRefId])
  }

  object MyOptionNullableRelationship {
    case class MyClass(@Relationship a: Option[Nullable[MyRefId]])
  }

  object MyOptionListRelationship {
    case class MyClass(@Relationship a: Option[List[MyRefId]])
  }

  object MyIdRelationshipDefaulted {
    case class MyClass(@Relationship a: MyRefId = MyRefId("A"))
  }

  object MyNullableRelationshipDefaulted {
    case class MyClass(@Relationship a: Nullable[MyRefId] = NotNull(MyRefId("B")))
  }

  object MyListRelationshipDefaulted {
    case class MyClass(@Relationship a: List[MyRefId] = List(MyRefId("C"), MyRefId("D")))
  }

  object MyOptionIdRelationshipDefaulted {
    case class MyClass(@Relationship a: Option[MyRefId] = Some(MyRefId("E")))
  }

  object MyOptionNullableRelationshipDefaulted {
    case class MyClass(@Relationship a: Option[Nullable[MyRefId]] = Some(NotNull(MyRefId("F"))))
  }

  object MyOptionListRelationshipDefaulted {
    case class MyClass(@Relationship a: Option[List[MyRefId]] = Some(List(MyRefId("G"), MyRefId("H"), MyRefId("I"))))
  }
}

class HListRelationshipDecoderTest extends HListDecoderTestBase {
  private val nonObjectResource = Input(jsona"[]", focus)

  private val emptyResource = Input(json"""
    {
      "type": "MyClass"
    }
  """, focus ~> narrow[JObject])

  private val nonObjectRelationships = Input(json"""
    {
      "type": "MyClass",
      "relationships": "foo"
    }
  """, focus ~> "relationships")

  private val emptyRelationships = Input(json"""
    {
      "type": "MyClass",
      "relationships": {}
    }
  """, relationships)

  private val booleanRelationship = Input(json"""
    {
      "type": "MyClass",
      "relationships": {
        "a": true
      }
    }
  """, relationships ~> "a")

  private val nullRelationship = Input(json"""
    {
      "type": "MyClass",
      "relationships": {
        "a": null
      }
    }
  """, relationships ~> "a")

  private val emptyRelationship = Input(json"""
    {
      "type": "MyClass",
      "relationships": {
        "a": {}
      }
    }
  """, relationship("a"))

  private val booleanRelationshipData = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data": true
          }
        }
      }
    """,
    relationship("a") ~> data ~> narrow[JBoolean]
  )

  private val nullRelationshipData = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data": null
          }
        }
      }
    """,
    relationship("a") ~> data ~> narrow[JNull]
  )

  private val missingIdRelationshipDataObject = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data":{
              "type": "MyRefId"
            }
          }
        }
      }
    """,
    relationship("a") ~> data ~> narrow[JObject]
  )

  private val idedRelationshipDataObject = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data": {
              "type": "MyRefId",
              "id": "J"
            }
          }
        }
      }
    """,
    relationship("a") ~> data
  )

  private val emptyRelationshipData = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data": []
          }
        }
      }
    """,
    relationship("a") ~> data ~> narrow[JArray]
  )

  private val emptyRelationshipDataObjects = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data": [
              {
                "type": "MyRefId"
              },
              {}
            ]
          }
        }
      }
    """,
    relationship("a") ~> data ~> narrow[JArray]
  )

  private val idedRelationshipDataObjects = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data": [
              {
                "type": "MyRefId",
                "id": "K"
              },
              {
                "type": "MyRefId",
                "id": "L"
              }
            ]
          }
        }
      }
    """,
    relationship("a") ~> data ~> narrow[JArray]
  )

  private val mixedRelationshipDataObjects = Input(
    json"""
      {
        "type": "MyClass",
        "relationships": {
          "a": {
            "data": [
              null,
              {
                "type": "MyRefId",
                "id": "M"
              },
              true
            ]
          }
        }
      }
    """,
    relationship("a") ~> data ~> narrow[JArray]
  )

  describe("MyIdRelationship") {
    import MyIdRelationship._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "relationships"))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.failsWith[MyClass](MissingField(_, "a"))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(MyRefId("J")))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
  }

  describe("MyNullableRelationship") {
    import MyNullableRelationship._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "relationships"))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.failsWith[MyClass](MissingField(_, "a"))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    nullRelationshipData.succeedsWith(MyClass(Null))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(NotNull(MyRefId("J"))))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
  }

  describe("MyListRelationship") {
    import MyListRelationship._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "relationships"))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.failsWith[MyClass](MissingField(_, "a"))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    missingIdRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    idedRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    emptyRelationshipData.succeedsWith(MyClass(Nil))
    emptyRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "type"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id")
      )
    }
    idedRelationshipDataObjects.succeedsWith(MyClass(List(MyRefId("K"), MyRefId("L"))))
    mixedRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }

  describe("MyOptionIdRelationship") {
    import MyOptionIdRelationship._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(None))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(Some(MyRefId("J"))))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
  }

  describe("MyOptionNullableRelationship") {
    import MyOptionNullableRelationship._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(None))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    nullRelationshipData.succeedsWith(MyClass(Some(Null)))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(Some(NotNull(MyRefId("J")))))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
  }

  describe("MyOptionListRelationship") {
    import MyOptionListRelationship._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(None))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    missingIdRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    idedRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    emptyRelationshipData.succeedsWith(MyClass(Some(Nil)))
    emptyRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "type"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id")
      )
    }
    idedRelationshipDataObjects.succeedsWith(MyClass(Some(List(MyRefId("K"), MyRefId("L")))))
    mixedRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }

  describe("MyIdRelationshipDefaulted") {
    import MyIdRelationshipDefaulted._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(MyRefId("A")))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(MyRefId("A")))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(MyRefId("J")))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
  }

  describe("MyNullableRelationshipDefaulted") {
    import MyNullableRelationshipDefaulted._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(NotNull(MyRefId("B"))))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(NotNull(MyRefId("B"))))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    nullRelationshipData.succeedsWith(MyClass(Null))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(NotNull(MyRefId("J"))))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
  }

  describe("MyListRelationshipDefaulted") {
    import MyListRelationshipDefaulted._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(List(MyRefId("C"), MyRefId("D"))))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(List(MyRefId("C"), MyRefId("D"))))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    missingIdRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    idedRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    emptyRelationshipData.succeedsWith(MyClass(Nil))
    emptyRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "type"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id")
      )
    }
    idedRelationshipDataObjects.succeedsWith(MyClass(List(MyRefId("K"), MyRefId("L"))))
    mixedRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }

  describe("MyOptionIdRelationshipDefaulted") {
    import MyOptionIdRelationshipDefaulted._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(None))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(Some(MyRefId("J"))))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JObject))
  }

  describe("MyOptionNullableRelationshipDefaulted") {
    import MyOptionNullableRelationshipDefaulted._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(None))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    nullRelationshipData.succeedsWith(MyClass(Some(Null)))
    missingIdRelationshipDataObject.failsWith[MyClass](MissingField(_, "id"))
    idedRelationshipDataObject.succeedsWith(MyClass(Some(NotNull(MyRefId("J")))))
    emptyRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    emptyRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    idedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
    mixedRelationshipDataObjects.failsWith[MyClass](JsonTypeMismatch(_, JNull, JObject))
  }

  describe("MyOptionListRelationshipDefaulted") {
    import MyOptionListRelationshipDefaulted._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectRelationships.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationships.succeedsWith(MyClass(None))
    booleanRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    nullRelationship.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyRelationship.failsWith[MyClass](MissingField(_, "data"))
    booleanRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullRelationshipData.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    missingIdRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    idedRelationshipDataObject.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    emptyRelationshipData.succeedsWith(MyClass(Some(Nil)))
    emptyRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        MissingField(in(focus ~> 0 ~> narrow[JObject]).shouldSucceed, "id"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "type"),
        MissingField(in(focus ~> 1 ~> narrow[JObject]).shouldSucceed, "id")
      )
    }
    idedRelationshipDataObjects.succeedsWith(MyClass(Some(List(MyRefId("K"), MyRefId("L")))))
    mixedRelationshipDataObjects.failsWithMultiple[MyClass] { in =>
      NonEmptyChain(
        JsonTypeMismatch(in(focus ~> 0).shouldSucceed, JObject),
        JsonTypeMismatch(in(focus ~> 2).shouldSucceed, JObject)
      )
    }
  }
}
