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

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.generic.Source
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.auto._
import HListSourceDecoderTest._
import org.scalawag.bateman.jsonapi.lens._

object HListSourceDecoderTest {
  object MyIdSource {
    case class MyClass(@Source a: JSource)
  }
  object MyOptionSource {
    case class MyClass(@Source a: Option[JSource])
  }
  object MyIdSourceWithDefault {
    case class MyClass(
        @Source a: JSource = JSource(JObject("x" -> JNumber(1)).asRootFocus, Map("a" -> JString("foo").asRootFocus))
    )
  }
  object MyOptionSourceWithDefault {
    case class MyClass(@Source a: Option[JSource] = None)
  }
  object MultipleFields {
    case class MyRef(@Id id: String)
    case class MyClass(
        @Type myType: String,
        @Id id: String,
        @Attribute a: Int,
        @Meta b: String,
        @Relationship c: MyRef,
        @IncludedRelationship d: MyRef,
        @Source src: Option[JSource] = None
    )
  }
  object MultipleOptionFields {
    case class MyRef(@Id id: String)
    case class MyClass(
        @Type myType: Option[String],
        @Id id: Option[String],
        @Attribute a: Option[Int],
        @Meta b: Option[String],
        @Relationship c: Option[MyRef],
        @IncludedRelationship d: Option[MyRef],
        @Source src: Option[JSource] = None
    )
  }
}

class HListSourceDecoderTest extends HListDecoderTestBase {
  describe("MyIdSource") {
    import MyIdSource._

    it("should decode empty") {
      val f = json"""{"type": "MyClass"}""".asRootFocus
      f.decode[MyClass].shouldSucceed shouldBe MyClass(JSource(f, Map.empty))
    }
  }

  describe("MyOptionSource") {
    import MyOptionSource._

    it("should decode empty") {
      val f = json"""{"type": "MyClass"}""".asRootFocus
      f.decode[MyClass].shouldSucceed shouldBe MyClass(Some(JSource(f, Map.empty)))
    }
  }

  describe("MyIdSourceWithDefault") {
    import MyIdSourceWithDefault._

    it("should decode empty") {
      val f = json"""{"type": "MyClass"}""".asRootFocus
      f.decode[MyClass].shouldSucceed shouldBe MyClass(JSource(f, Map.empty))
    }
  }

  describe("MyOptionSourceWithDefault") {
    import MyOptionSourceWithDefault._

    it("should decode empty") {
      val f = json"""{"type": "MyClass"}""".asRootFocus
      f.decode[MyClass].shouldSucceed shouldBe MyClass(Some(JSource(f, Map.empty)))
    }
  }

  describe("MultipleFields") {
    import MultipleFields._

    it("should decode all field sources") {
      val f = json"""
        {
          "data": {
            "type": "MyClass",
            "id": "foo",
            "attributes": {
              "a": 8
            },
            "meta": {
              "b": "quux"
            },
            "relationships": {
              "c": {
                "data": {
                  "type": "MyRef",
                  "id": "A"
                }
              },
              "d": {
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
              "id": "B"
            }
          ]
        }
      """.asRootFocus

      val src = f(data).decode[MyClass].shouldSucceed.src.get

      src.fields("myType") shouldBe f(data ~> resourceType).shouldSucceed
      src.fields("id") shouldBe f(data ~> id).shouldSucceed
      src.fields("a") shouldBe f(data ~> attribute("a")).shouldSucceed
      src.fields("b") shouldBe f(data ~> meta("b")).shouldSucceed
      src.fields("c") shouldBe f(data ~> relationship("c")).shouldSucceed
      src.fields("d") shouldBe f(data ~> relationship("d")).shouldSucceed
      src.fields.size shouldBe 6
    }
  }

  describe("MultipleOptionalFields") {
    import MultipleOptionFields._

    it("should decode all field sources") {
      val f = json"""
        {
          "data": {
            "type": "MyClass",
            "id": "foo",
            "attributes": {
              "a": 8
            },
            "meta": {
              "b": "quux"
            },
            "relationships": {
              "c": {
                "data": {
                  "type": "MyRef",
                  "id": "A"
                }
              },
              "d": {
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
              "id": "B"
            }
          ]
        }
      """.asRootFocus

      val src = f(data).decode[MyClass].shouldSucceed.src.get

      src.fields("myType") shouldBe f(data ~> resourceType).shouldSucceed
      src.fields("id") shouldBe f(data ~> id).shouldSucceed
      src.fields("a") shouldBe f(data ~> attribute("a")).shouldSucceed
      src.fields("b") shouldBe f(data ~> meta("b")).shouldSucceed
      src.fields("c") shouldBe f(data ~> relationship("c")).shouldSucceed
      src.fields("d") shouldBe f(data ~> relationship("d")).shouldSucceed
      src.fields.size shouldBe 6
    }

    it("should not decode absent field sources") {
      val f = json"""
        {
          "data": {
            "type": "MyClass"
          }
        }
      """.asRootFocus

      val src = f(data).decode[MyClass].shouldSucceed.src.get

      src.fields("myType") shouldBe f(data ~> resourceType).shouldSucceed
      src.fields.size shouldBe 1
    }
  }

}
