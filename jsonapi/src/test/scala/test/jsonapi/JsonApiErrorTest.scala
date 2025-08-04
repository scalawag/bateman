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

package test.jsonapi

import cats.data.NonEmptyChain
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.jsonapi.{DuplicateResourceObjectDefinition, JsonApiTypeMismatch, MissingIncludedResourceObject}
import org.scalawag.bateman.jsonapi.encoding.Inclusions.Key
import test.json.BatemanTestBase

class JsonApiErrorTest extends BatemanTestBase {

  describe("JsonApiTypeMismatch") {

    it("should describe a single expected type") {
      val root = json"""{"type": "wrong"}""".asRootFocus
      val cur = root(resourceType).shouldSucceed
      val err = JsonApiTypeMismatch(cur, "expected")
      err.description should include("'expected'")
      err.description should include("'wrong'")
    }

    it("should describe multiple expected types with or-formatting") {
      val root = json"""{"type": "wrong"}""".asRootFocus
      val cur = root(resourceType).shouldSucceed
      val err = JsonApiTypeMismatch(cur, NonEmptyChain("alpha", "beta", "gamma"))
      err.description should include("'alpha'")
      err.description should include("'beta'")
      err.description should include("'gamma'")
      err.description should include("or")
    }

    it("should extract pointer from the focus") {
      val root = json"""{"data": {"type": "wrong"}}""".asRootFocus
      val cur = root(data ~> resourceType).shouldSucceed
      val err = JsonApiTypeMismatch(cur, "expected")
      err.pointer.toString should include("data")
      err.pointer.toString should include("type")
    }

    it("should extract location from the JString value") {
      val root = json"""{"type": "wrong"}""".asRootFocus
      val cur = root(resourceType).shouldSucceed
      val err = JsonApiTypeMismatch(cur, "expected")
      err.location shouldBe defined
    }

    it("should handle stripped location") {
      val root = json"""{"type": "wrong"}""".stripLocation.asRootFocus
      val cur = root(resourceType).shouldSucceed
      val err = JsonApiTypeMismatch(cur, "expected")
      err.location shouldBe None
    }
  }

  describe("DuplicateResourceObjectDefinition") {

    it("should describe duplicate resource objects with their locations") {
      val obj1 = json"""{"type": "article", "id": "1"}""".asRootFocus.asObject.shouldSucceed
      val obj2 = json"""{"type": "article", "id": "1"}""".asRootFocus.asObject.shouldSucceed
      val key = Key("article", "1", local = false)
      val err = DuplicateResourceObjectDefinition(key, obj1, NonEmptyChain.one(obj2))
      err.description should include(key.toString)
      err.description should include("defined multiple times")
    }

    it("should extract pointer from the primary object") {
      val doc = json"""{"data": {"type": "article", "id": "1"}}""".asRootFocus
      val primary = doc(data).shouldSucceed.asObject.shouldSucceed
      val other = json"""{"type": "article", "id": "1"}""".asRootFocus.asObject.shouldSucceed
      val key = Key("article", "1", local = false)
      val err = DuplicateResourceObjectDefinition(key, primary, NonEmptyChain.one(other))
      err.pointer.toString should include("data")
    }

    it("should extract location from the primary object") {
      val obj = json"""{"type": "article", "id": "1"}""".asRootFocus.asObject.shouldSucceed
      val key = Key("article", "1", local = false)
      val err = DuplicateResourceObjectDefinition(key, obj, NonEmptyChain.one(obj))
      err.location shouldBe defined
    }
  }

  describe("MissingIncludedResourceObject") {

    it("should describe the missing key") {
      val root = json"""{"type": "article", "id": "1"}""".asRootFocus.asObject.shouldSucceed
      val key = Key("author", "99", local = false)
      val err = MissingIncludedResourceObject(root, key)
      err.description should include(key.toString)
    }

    it("should describe a missing local key") {
      val root = json"""{"type": "article", "id": "1"}""".asRootFocus.asObject.shouldSucceed
      val key = Key("author", "#temp", local = true)
      val err = MissingIncludedResourceObject(root, key)
      err.description should include("lid")
    }

    it("should extract pointer from the focus") {
      val doc = json"""{"data": {"type": "article", "id": "1"}}""".asRootFocus
      val obj = doc(data).shouldSucceed.asObject.shouldSucceed
      val key = Key("author", "99", local = false)
      val err = MissingIncludedResourceObject(obj, key)
      err.pointer.toString should include("data")
    }

    it("should extract location from the object") {
      val root = json"""{"type": "article", "id": "1"}""".asRootFocus.asObject.shouldSucceed
      val key = Key("author", "99", local = false)
      val err = MissingIncludedResourceObject(root, key)
      err.location shouldBe defined
    }
  }
}
