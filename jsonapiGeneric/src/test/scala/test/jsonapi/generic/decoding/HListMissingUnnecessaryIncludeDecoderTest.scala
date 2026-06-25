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

package test.jsonapi.generic.decoding

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.MissingIncludedResourceObject
import org.scalawag.bateman.jsonapi.generic.semiauto.unchecked._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.jsonapi.encoding.Inclusions
import HListMissingUnnecessaryIncludeDecoderTest._

// Ported from the 0.3.3 fix (commit dfe1496d): "Fix bug where missing (but unnecessary) included resource object
// is flagged as an error." When a relationship's included resource object is absent from the document but the
// related type requires no fields of its own (all attributes optional/defaulted), decoding should still succeed by
// building the related object from its resource identifier. When the related type DOES require fields, the missing
// include must still be reported as a MissingIncludedResourceObject.
object HListMissingUnnecessaryIncludeDecoderTest {
  // Related object with no required fields beyond its id.
  case class OptionalRefObj(@Id a: String, @Attribute b: Option[String] = None)

  object OptionalRefObj {
    implicit val refDecoder: JObjectDecoder[OptionalRefObj] =
      deriveResourceDecoderForCaseClass[OptionalRefObj]("RefObj")
  }

  // Related object with a required attribute.
  case class RequiredRefObj(@Id a: String, @Attribute b: String)

  object RequiredRefObj {
    implicit val refDecoder: JObjectDecoder[RequiredRefObj] =
      deriveResourceDecoderForCaseClass[RequiredRefObj]("RefObj")
  }

  object Optional {
    case class MyClass(@IncludedRelationship a: OptionalRefObj)
  }

  object Required {
    case class MyClass(@IncludedRelationship a: RequiredRefObj)
  }
}

class HListMissingUnnecessaryIncludeDecoderTest extends HListDecoderTestBase {
  private val includesAreMissing = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": {
                "type": "RefObj",
                "id": "N"
              }
            }
          }
        }
      }
    """,
    data ~> relationship("a") ~> data ~> narrow[JObject],
    data
  )

  private val includesAreEmpty = Input(
    json"""
      {
        "data": {
          "type": "MyClass",
          "relationships": {
            "a": {
              "data": {
                "type": "RefObj",
                "id": "N"
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
    includesAreEmpty
      .json(data ~> relationship("a") ~> data)
      .flatMap(_.decode[Inclusions.Key])
      .shouldSucceed

  describe("relationship to an object with no required fields") {
    import Optional._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    includesAreMissing.succeedsWith(MyClass(OptionalRefObj("N", None)))
    includesAreEmpty.succeedsWith(MyClass(OptionalRefObj("N", None)))
  }

  describe("relationship to an object with a required field") {
    import Required._
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()

    includesAreMissing.failsWith[MyClass](MissingIncludedResourceObject(_, missingIncludedObjectRef))
    includesAreEmpty.failsWith[MyClass](MissingIncludedResourceObject(_, missingIncludedObjectRef))
  }
}
