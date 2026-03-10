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

import org.scalatest.funspec.AnyFunSpec
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.semiauto._

class CaseClassEncoderFactoryTest extends AnyFunSpec {
  case class Simple(name: String, age: Int)
  case class WithDefaults(name: String = "John", age: Int = 30)
  case class WithOption(name: String, nickname: Option[String])

  describe("CaseClassEncoderFactory") {
    it("should encode a simple case class") {
      implicit val encoder: JObjectEncoder[Simple] = deriveEncoderForCaseClass[Simple]()
      val obj = Simple("Alice", 25)
      val encoded = encoder.encode(obj)
      assert(encoded == JObject("name" -> JString("Alice"), "age" -> JNumber(25)))
    }

    it("should handle default values") {
      implicit val config: Config = Config(encodeDefaultValues = false)
      implicit val encoder: JObjectEncoder[WithDefaults] = deriveEncoderForCaseClass[WithDefaults]()
      val obj = WithDefaults()
      val encoded = encoder.encode(obj)
      // With encodeDefaultValues = false, default values should be omitted
      assert(encoded == JObject())
    }

    it("should encode default values when configured") {
      implicit val config: Config = Config(encodeDefaultValues = true)
      implicit val encoder: JObjectEncoder[WithDefaults] = deriveEncoderForCaseClass[WithDefaults]()
      val obj = WithDefaults()
      val encoded = encoder.encode(obj)
      assert(encoded == JObject("name" -> JString("John"), "age" -> JNumber(30)))
    }

    it("should handle Option fields") {
      implicit val encoder: JObjectEncoder[WithOption] = deriveEncoderForCaseClass[WithOption]()
      val obj1 = WithOption("Alice", Some("Ali"))
      val encoded1 = encoder.encode(obj1)
      assert(encoded1 == JObject("name" -> JString("Alice"), "nickname" -> JString("Ali")))

      val obj2 = WithOption("Bob", None)
      val encoded2 = encoder.encode(obj2)
      assert(encoded2 == JObject("name" -> JString("Bob")))
    }
  }
}
