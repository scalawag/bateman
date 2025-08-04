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

package test.json.generic

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.{Config, TraitDeriverParams}
import org.scalawag.bateman.json.generic.Discriminators.SimpleClassNameDiscriminator
import org.scalawag.bateman.json.lens._
import test.json.BatemanTestBase

class TraitDeriverParamsTest extends BatemanTestBase {

  describe("addDiscriminator") {
    it("should add a discriminator field to an empty object") {
      val params = TraitDeriverParams[JAnyEncoder](Config.default, field("type"), SimpleClassNameDiscriminator[JAnyEncoder])
      val result = params.addDiscriminator(JObject.Empty, JString("foo"))
      result.fieldList.map(_.name.value) shouldBe List("type")
      result.fieldList.head.value.render shouldBe JString("foo").render
    }

    it("should overwrite an existing discriminator field") {
      val params = TraitDeriverParams[JAnyEncoder](Config.default, field("type"), SimpleClassNameDiscriminator[JAnyEncoder])
      val existing = JObject("type" -> JString("old"))
      val result = params.addDiscriminator(existing, JString("new"))
      result.fieldList.map(_.name.value) shouldBe List("type")
      result.fieldList.head.value.render shouldBe JString("new").render
    }

    it("should write at a nested path") {
      val metaStatus: CreatableJLens[JObject, JAny] = field("meta") ~> field("status")
      val params = TraitDeriverParams[JAnyEncoder](Config.default, metaStatus, SimpleClassNameDiscriminator[JAnyEncoder])
      val result = params.addDiscriminator(JObject.Empty, JString("active"))
      val meta = result.fieldList.head.value.asInstanceOf[JObject]
      meta.fieldList.head.name.value shouldBe "status"
      meta.fieldList.head.value.render shouldBe JString("active").render
    }

    it("should preserve existing fields when adding a discriminator") {
      val params = TraitDeriverParams[JAnyEncoder](Config.default, field("type"), SimpleClassNameDiscriminator[JAnyEncoder])
      val existing = JObject("other" -> JNumber(42))
      val result = params.addDiscriminator(existing, JString("foo"))
      result.fieldList.map(_.name.value) should contain allOf ("type", "other")
    }
  }
}
