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

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.generic.naming.CamelCase

class NamingTest extends AnyFunSpec with Matchers {
  describe("CamelCase") {
    val cases =
      Iterable(
        "address1" -> List("address1"),
        "uuid" -> List("uuid"),
        "UUID" -> List("u", "u", "i", "d"),
        "base64uuid" -> List("base64uuid"),
        "base64Uuid" -> List("base64", "uuid"),
        "base64UUID" -> List("base64", "u", "u", "i", "d"),
        "ZNamedClass" -> List("z", "named", "class")
      )

    cases.foreach {
      case (in, out) =>
        it(s"should convert '$in' to words") {
          CamelCase.toWords(in) shouldBe out
        }
    }
  }

  describe("CamelCase with capital grouping") {
    val cases =
      Iterable(
        "address1" -> List("address1"),
        "uuid" -> List("uuid"),
        "UUID" -> List("uuid"),
        "base64uuid" -> List("base64uuid"),
        "base64Uuid" -> List("base64", "uuid"),
        "base64UUID" -> List("base64", "uuid"),
        "ZNamedClass" -> List("znamed", "class")
      )

    cases.foreach {
      case (in, out) =>
        it(s"should convert '$in' to words") {
          CamelCase(true).toWords(in) shouldBe out
        }
    }
  }
}
