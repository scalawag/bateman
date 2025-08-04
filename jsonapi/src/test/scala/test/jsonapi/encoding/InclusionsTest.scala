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

package test.jsonapi.encoding

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.encoding.Inclusions
import org.scalawag.bateman.jsonapi.encoding.Inclusions.Key
import test.json.BatemanTestBase

class InclusionsTest extends BatemanTestBase {

  describe("Inclusions") {

    describe("adding objects") {

      it("should add a new object") {
        val obj: JObject = json"""{"type": "article", "id": "1"}"""
        val inc = Inclusions.empty + obj
        val keys = inc.objects.map(o => Key(o.asRootFocus)).toList
        keys shouldBe List(Key("article", "1", local = false))
      }

      it("should add multiple objects with different keys") {
        val obj1: JObject = json"""{"type": "article", "id": "1"}"""
        val obj2: JObject = json"""{"type": "author", "id": "2"}"""
        val inc = Inclusions.empty + obj1 + obj2
        val keys = inc.objects.map(o => Key(o.asRootFocus)).toSet
        keys shouldBe Set(Key("article", "1", local = false), Key("author", "2", local = false))
      }

      it("should deduplicate identical objects") {
        val obj1: JObject = json"""{"type": "article", "id": "1"}""".stripLocation
        val obj2: JObject = json"""{"type": "article", "id": "1"}""".stripLocation
        val inc = Inclusions.empty + obj1 + obj2
        val keys = inc.objects.map(o => Key(o.asRootFocus)).toList
        keys shouldBe List(Key("article", "1", local = false))
      }

      it("should throw ProgrammerError on inconsistent duplicate") {
        val obj1: JObject = json"""{"type": "article", "id": "1", "attributes": {"v": 1}}"""
        val obj2: JObject = json"""{"type": "article", "id": "1", "attributes": {"v": 2}}"""
        a[ProgrammerError] should be thrownBy {
          Inclusions.empty + obj1 + obj2
        }
      }

      it("should strip location from added objects") {
        val obj: JObject = json"""{"type": "article", "id": "1"}"""
        obj.location shouldBe defined
        val inc = Inclusions.empty + obj
        inc.objects.head.location shouldBe None
      }

      it("should maintain objects in sorted order by type then id") {
        val obj1: JObject = json"""{"type": "zebra", "id": "1"}"""
        val obj2: JObject = json"""{"type": "aardvark", "id": "2"}"""
        val obj3: JObject = json"""{"type": "aardvark", "id": "1"}"""
        val inc = Inclusions.empty + obj1 + obj2 + obj3
        val keys = inc.objects.map(o => Key(o.asRootFocus)).toList
        keys shouldBe List(
          Key("aardvark", "1", local = false),
          Key("aardvark", "2", local = false),
          Key("zebra", "1", local = false)
        )
      }
    }

    describe("Monoid") {

      it("should have empty as left identity") {
        val obj: JObject = json"""{"type": "article", "id": "1"}"""
        val inc = Inclusions(obj)
        import cats.syntax.monoid._
        val combined = Inclusions.empty |+| inc
        combined.objects.toList.map(_.render) shouldBe inc.objects.toList.map(_.render)
      }

      it("should have empty as right identity") {
        val obj: JObject = json"""{"type": "article", "id": "1"}"""
        val inc = Inclusions(obj)
        import cats.syntax.monoid._
        val combined = inc |+| Inclusions.empty
        combined.objects.toList.map(_.render) shouldBe inc.objects.toList.map(_.render)
      }

      it("should combine two Inclusions") {
        val obj1: JObject = json"""{"type": "article", "id": "1"}"""
        val obj2: JObject = json"""{"type": "author", "id": "2"}"""
        import cats.syntax.monoid._
        val combined = Inclusions(obj1) |+| Inclusions(obj2)
        val keys = combined.objects.map(o => Key(o.asRootFocus)).toSet
        keys shouldBe Set(Key("article", "1", local = false), Key("author", "2", local = false))
      }
    }

    describe("varargs constructor") {

      it("should create Inclusions from multiple objects") {
        val obj1: JObject = json"""{"type": "article", "id": "1"}"""
        val obj2: JObject = json"""{"type": "author", "id": "2"}"""
        val inc = Inclusions(obj1, obj2)
        val keys = inc.objects.map(o => Key(o.asRootFocus)).toSet
        keys shouldBe Set(Key("article", "1", local = false), Key("author", "2", local = false))
      }
    }
  }

  describe("Key") {

    describe("extraction from JFocus") {

      it("should extract from object with type and id") {
        val obj = json"""{"type": "article", "id": "123"}""".asRootFocus
        val key = Key(obj)
        key shouldBe Key("article", "123", local = false)
      }

      it("should extract from object with type and lid") {
        val obj = json"""{"type": "article", "lid": "#temp-1"}""".asRootFocus
        val key = Key(obj)
        key shouldBe Key("article", "#temp-1", local = true)
      }

      it("should throw ProgrammerError when both id and lid present") {
        val obj = json"""{"type": "article", "id": "123", "lid": "#temp"}""".asRootFocus
        a[ProgrammerError] should be thrownBy {
          Key(obj)
        }
      }

      it("should throw ProgrammerError when neither id nor lid present") {
        val obj = json"""{"type": "article"}""".asRootFocus
        a[ProgrammerError] should be thrownBy {
          Key(obj)
        }
      }
    }

    describe("requiredDecoder") {

      it("should decode object with type and id") {
        val obj: JObject = json"""{"type": "article", "id": "123"}"""
        val result = Key.requiredDecoder.decode(obj.asRootFocus)
        result.shouldSucceed shouldBe Key("article", "123", local = false)
      }

      it("should decode object with type and lid") {
        val obj: JObject = json"""{"type": "article", "lid": "#temp"}"""
        val result = Key.requiredDecoder.decode(obj.asRootFocus)
        result.shouldSucceed shouldBe Key("article", "#temp", local = true)
      }

      it("should fail with MissingField when missing type") {
        val obj: JObject = json"""{"id": "123"}"""
        val errors = Key.requiredDecoder.decode(obj.asRootFocus).shouldFail
        inside(errors.head) {
          case mf: MissingField =>
            mf.names.iterator.toSet should contain("type")
            mf.pointer shouldBe obj.asRootFocus.pointer
        }
      }

      it("should fail with MissingField when missing both id and lid") {
        val obj: JObject = json"""{"type": "article"}"""
        val errors = Key.requiredDecoder.decode(obj.asRootFocus).shouldFail
        inside(errors.head) {
          case mf: MissingField =>
            mf.names.iterator.toSet shouldBe Set("id", "lid")
            mf.pointer shouldBe obj.asRootFocus.pointer
        }
      }

      it("should fail with UnexpectedValue when both id and lid present") {
        val obj: JObject = json"""{"type": "article", "id": "123", "lid": "#temp"}"""
        val errors = Key.requiredDecoder.decode(obj.asRootFocus).shouldFail
        inside(errors.head) {
          case uv: UnexpectedValue =>
            uv.pointer.toString should include("lid")
        }
      }
    }

    describe("optionalDecoder") {

      it("should return Some when type and id present") {
        val obj: JObject = json"""{"type": "article", "id": "123"}"""
        val result = Key.optionalDecoder.decode(obj.asRootFocus)
        result.shouldSucceed shouldBe Some(Key("article", "123", local = false))
      }

      it("should return None when fields are missing") {
        val obj: JObject = json"""{"other": "field"}"""
        val result = Key.optionalDecoder.decode(obj.asRootFocus)
        result.shouldSucceed shouldBe None
      }
    }

    describe("ordering") {

      it("should order by type first") {
        val k1 = Key("article", "1", local = false)
        val k2 = Key("author", "1", local = false)
        Key.ordering.compare(k1, k2) should be < 0
      }

      it("should order by local flag second") {
        val k1 = Key("article", "1", local = false)
        val k2 = Key("article", "1", local = true)
        Key.ordering.compare(k1, k2) should be < 0
      }

      it("should order by id third") {
        val k1 = Key("article", "1", local = false)
        val k2 = Key("article", "2", local = false)
        Key.ordering.compare(k1, k2) should be < 0
      }
    }

    describe("toString") {

      it("should format with id") {
        val key = Key("article", "123", local = false)
        key.toString shouldBe """(type="article", id="123")"""
      }

      it("should format with lid") {
        val key = Key("article", "#temp", local = true)
        key.toString shouldBe """(type="article", lid="#temp")"""
      }
    }
  }

  describe("findResourceObjects") {

    it("should find single primary data object") {
      val doc = json"""{
        "data": {"type": "article", "id": "1"}
      }""".asRootFocus
      val objects = Inclusions.findResourceObjects(doc).shouldSucceed
      objects.keySet shouldBe Set(Key("article", "1", local = false))
      objects(Key("article", "1", local = false)).map(_.pointer.toString) shouldBe List("/data")
    }

    it("should find array of primary data objects") {
      val doc = json"""{
        "data": [
          {"type": "article", "id": "1"},
          {"type": "article", "id": "2"}
        ]
      }""".asRootFocus
      val objects = Inclusions.findResourceObjects(doc).shouldSucceed
      objects.keySet shouldBe Set(
        Key("article", "1", local = false),
        Key("article", "2", local = false)
      )
    }

    it("should handle null primary data") {
      val doc = json"""{"data": null}""".asRootFocus
      Inclusions.findResourceObjects(doc).shouldSucceed shouldBe empty
    }

    it("should handle missing data field") {
      val doc = json"""{"meta": {"version": "1"}}""".asRootFocus
      Inclusions.findResourceObjects(doc).shouldSucceed shouldBe empty
    }

    it("should find included objects alongside primary data") {
      val doc = json"""{
        "data": {"type": "article", "id": "1"},
        "included": [
          {"type": "author", "id": "10"},
          {"type": "comment", "id": "100"}
        ]
      }""".asRootFocus
      val objects = Inclusions.findResourceObjects(doc).shouldSucceed
      objects.keySet shouldBe Set(
        Key("article", "1", local = false),
        Key("author", "10", local = false),
        Key("comment", "100", local = false)
      )
    }

    it("should handle missing included field") {
      val doc = json"""{
        "data": {"type": "article", "id": "1"}
      }""".asRootFocus
      val objects = Inclusions.findResourceObjects(doc).shouldSucceed
      objects.keySet shouldBe Set(Key("article", "1", local = false))
    }

    it("should group duplicate keys across primary and included") {
      val doc = json"""{
        "data": {"type": "article", "id": "1"},
        "included": [{"type": "article", "id": "1"}]
      }""".asRootFocus
      val objects = Inclusions.findResourceObjects(doc).shouldSucceed
      val foci = objects(Key("article", "1", local = false))
      foci.map(_.pointer.toString).toSet shouldBe Set("/data", "/included/0")
    }

    it("should filter out objects without keys") {
      val doc = json"""{
        "data": [
          {"type": "article", "id": "1"},
          {"type": "draft"}
        ]
      }""".asRootFocus
      val objects = Inclusions.findResourceObjects(doc).shouldSucceed
      objects.keySet shouldBe Set(Key("article", "1", local = false))
    }

    it("should handle local identifiers") {
      val doc = json"""{
        "data": {"type": "article", "lid": "#temp-1"}
      }""".asRootFocus
      val objects = Inclusions.findResourceObjects(doc).shouldSucceed
      objects.keySet shouldBe Set(Key("article", "#temp-1", local = true))
    }

    it("should fail with JsonTypeMismatch on invalid primary data type") {
      val doc = json"""{"data": "invalid"}""".asRootFocus
      val errors = Inclusions.findResourceObjects(doc).shouldFail
      inside(errors.head) {
        case jtm: JsonTypeMismatch =>
          jtm.pointer.toString should include("data")
          jtm.expected.toNonEmptyList.toList should contain(JObject)
      }
    }

    it("should propagate MissingField errors from included objects missing type") {
      val doc = json"""{
        "data": {"type": "article", "id": "1"},
        "included": [{"missing_type": true}]
      }""".asRootFocus
      val errors = Inclusions.findResourceObjects(doc).shouldFail
      inside(errors.head) {
        case mf: MissingField =>
          mf.names.iterator.toSet should contain("type")
          mf.pointer.toString should include("included")
      }
    }
  }
}
