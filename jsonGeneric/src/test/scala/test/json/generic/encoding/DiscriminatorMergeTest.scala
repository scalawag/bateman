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

package test.json.generic.encoding

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.encoding.DiscriminatorMerge
import org.scalawag.bateman.json.literal._
import test.json.BatemanTestBase

class DiscriminatorMergeTest extends BatemanTestBase {

  describe("mergeDiscriminators") {
    it("should return base unchanged when discriminators is empty") {
      val base: JObject = json"""{"a": 1, "b": 2}"""
      val result = DiscriminatorMerge.mergeDiscriminators(JObject.Empty, base)
      result shouldRenderTo base
    }

    it("should prepend discriminator fields before base fields") {
      val disc: JObject = json"""{"type": "foo"}"""
      val base: JObject = json"""{"a": 1, "b": 2}"""
      val result = DiscriminatorMerge.mergeDiscriminators(disc, base)
      result.fieldList.map(_.name.value) shouldBe List("type", "a", "b")
      result shouldRenderTo json"""{"type": "foo", "a": 1, "b": 2}"""
    }

    it("should keep discriminator value when both sides have equal values") {
      val disc: JObject = json"""{"type": "foo"}"""
      val base: JObject = json"""{"type": "foo", "a": 1}"""
      val result = DiscriminatorMerge.mergeDiscriminators(disc, base)
      result.fieldList.map(_.name.value) shouldBe List("type", "a")
      result shouldRenderTo json"""{"type": "foo", "a": 1}"""
    }

    it("should recursively merge when both sides have JObject values for the same field") {
      val disc: JObject = json"""{"meta": {"status": "active"}}"""
      val base: JObject = json"""{"meta": {"count": 5}, "a": 1}"""
      val result = DiscriminatorMerge.mergeDiscriminators(disc, base)
      result shouldRenderTo json"""{"meta": {"status": "active", "count": 5}, "a": 1}"""
    }

    it("should throw ProgrammerError when discriminator and base have conflicting values") {
      val disc: JObject = json"""{"type": "foo"}"""
      val base: JObject = json"""{"type": "bar", "a": 1}"""
      a[ProgrammerError] shouldBe thrownBy {
        DiscriminatorMerge.mergeDiscriminators(disc, base)
      }
    }

    it("should handle multiple discriminator fields") {
      val disc: JObject = json"""{"type": "foo", "version": 2}"""
      val base: JObject = json"""{"a": 1}"""
      val result = DiscriminatorMerge.mergeDiscriminators(disc, base)
      result shouldRenderTo json"""{"type": "foo", "version": 2, "a": 1}"""
    }

    it("should handle deeply nested recursive merge") {
      val disc: JObject = json"""{"meta": {"inner": {"status": "active"}}}"""
      val base: JObject = json"""{"meta": {"inner": {"count": 5}}}"""
      val result = DiscriminatorMerge.mergeDiscriminators(disc, base)
      result shouldRenderTo json"""{"meta": {"inner": {"status": "active", "count": 5}}}"""
    }
  }
}
