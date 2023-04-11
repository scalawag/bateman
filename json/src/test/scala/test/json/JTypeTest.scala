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

package test.json

import cats.data.NonEmptyList
import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json._

class JTypeTest extends AnyFunSpec with Matchers with DataDrivenTestUtils {

  describe("labels") {
    def testCase(jtype: JType, label: String)(implicit pos: Position): Unit =
      it(s"should label $jtype as '$label'") {
        jtype.label shouldBe label
      }

    testCase(JObject, "an object")
    testCase(JArray, "an array")
    testCase(JString, "a string")
    testCase(JBoolean, "a boolean")
    testCase(JNumber, "a number")
    testCase(JNull, "null")
  }

  describe("summoners") {
    def testCase[A <: JAny: JType.Summoner](instance: JType)(implicit pos: Position): Unit =
      it(s"should summon JType for $instance") {
        JType[A] shouldBe instance
      }

    testCase[JObject](JObject)
    testCase[JArray](JArray)
    testCase[JString](JString)
    testCase[JBoolean](JBoolean)
    testCase[JNumber](JNumber)
    testCase[JNull](JNull)
  }

  describe("ordering") {
    val unsorted: List[JType] = List(JObject, JArray, JString, JBoolean, JNumber, JNull)
    val sorted: List[JType] = List(JArray, JBoolean, JNull, JNumber, JObject, JString)

    it("should order JTypes with Ordering") {
      unsorted.sorted shouldBe sorted
    }

    it("should order JTypes with Order") {
      NonEmptyList.fromListUnsafe(unsorted).sorted shouldBe NonEmptyList.fromListUnsafe(sorted)
    }
  }
}
