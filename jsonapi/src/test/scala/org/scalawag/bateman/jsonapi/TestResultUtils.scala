// bateman -- Copyright 2021 -- Justin Patterson
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

package org.scalawag.bateman.jsonapi

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.encoding.EncodeResult

trait TestResultUtils { _: Matchers =>
  implicit class EncodeResultOps[A](r: EncodeResult[A]) {
    def asValid: A =
      r.fold(
        ee => fail("operation should have succeeded but failed:\n " + EncodeResult.formatErrorReport(ee)),
        identity
      )
  }

  implicit class DocumentOps(actual: encoding.Document) {

    // Makes it easier to see what's different when two Documents differ by rendering them.
    def shouldMatch(expected: encoding.Document): Assertion = {
      if (actual == expected)
        succeed
      else {
        actual.sortIncludes.to[json.encoding.JAny] shouldMatch expected.sortIncludes.to[json.encoding.JAny]
      }
    }

    def shouldMatch(that: decoding.Document): Assertion = shouldMatch(that.toEncoding)
  }

  implicit class JObjectOps(actual: json.encoding.JAny) {

    // Makes it easier to see what's different when two Documents differ by rendering them.
    def shouldMatch(expected: json.encoding.JAny): Assertion = {
      if (actual == expected)
        succeed
      else {
        actual.spaces2 shouldBe expected.spaces2
      }
    }

    def shouldMatch(that: json.decoding.JAny): Assertion = shouldMatch(that.toEncoding)
  }
}
