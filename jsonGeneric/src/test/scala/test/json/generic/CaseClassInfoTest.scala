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

package test.json.generic

import org.scalawag.bateman.json.generic.CaseClassInfo
import shapeless.HNil
import test.json.BatemanTestBase
import test.json.generic.CaseClassInfoTest.MyClass

object CaseClassInfoTest {
  final case class MyClass(a: Int, b: String = "foo", c: Option[Boolean] = None)
}

class CaseClassInfoTest extends BatemanTestBase {
  it("should extract case class info") {
    val cci = CaseClassInfo[MyClass]
    cci.fieldNames shouldBe List("a", "b", "c")
    cci.defaults shouldBe None :: Some("foo") :: Some(None) :: HNil
  }
}
