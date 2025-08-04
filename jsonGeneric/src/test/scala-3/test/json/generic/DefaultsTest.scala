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

import org.scalawag.bateman.json.generic.defaults.Defaults
import test.json.BatemanTestBase

case class WithDefaults(a: Int, b: String = "hello", c: Boolean = true)
case class NoDefaults(x: Int, y: String)
case class AllDefaults(a: Int = 1, b: String = "two", c: Double = 3.0)

class DefaultsTest extends BatemanTestBase {

  describe("Defaults") {
    it("should extract defaults for fields that have them") {
      val d = Defaults.summonDefaults[WithDefaults]
      val defaults = d.defaults.productIterator.toList
      defaults shouldBe List(None, Some("hello"), Some(true))
    }

    it("should return None for fields without defaults") {
      val d = Defaults.summonDefaults[NoDefaults]
      val defaults = d.defaults.productIterator.toList
      defaults shouldBe List(None, None)
    }

    it("should extract all defaults when every field has one") {
      val d = Defaults.summonDefaults[AllDefaults]
      val defaults = d.defaults.productIterator.toList
      defaults shouldBe List(Some(1), Some("two"), Some(3.0))
    }
  }
}
