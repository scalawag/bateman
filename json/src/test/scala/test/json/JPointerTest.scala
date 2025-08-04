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

package test.json

import org.scalawag.bateman.json.{JLocation, JNumber, JPointer}

class JPointerTest extends BatemanTestBase {
  it("should navigate arrays") {
    val json = parse("""[[[1],[2],[3]],[[4],[5],[6]],[[7],[8],[9]]]""")
    val p = JPointer.Root / 1 / 1 / 0

    inside(json.navigate(p).value.value) {
      case JNumber(n, Some(l)) =>
        n shouldBe "5"
        l shouldBe JLocation(1, 22)
    }
  }

  it("should navigate objects") {
    val json = parse("""{"a":{"a":{"a":1,"b":2,"c":3},"b":{"a":4,"b":5,"c":6},"c":{"a":7,"b":8,"c":9}}}""")
    val p = JPointer.Root / "a" / "b" / "c"

    inside(json.navigate(p).value.value) {
      case JNumber(n, Some(l)) =>
        n shouldBe "6"
        l shouldBe JLocation(1, 52)
    }
  }
}