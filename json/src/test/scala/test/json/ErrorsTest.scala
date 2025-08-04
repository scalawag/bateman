package test.json

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

import org.scalawag.bateman.json.JErrors.formatErrorReport
import org.scalawag.bateman.json.focus.JRootFocus
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.JAny

class ErrorsTest extends BatemanTestBase {
  val json: JRootFocus[JAny] = parse("""
    {
      "a": {
        "g": 4,
        "f": "thing",
        "b": true
      },
      "b": 6,
      "g": [
        {
          "c": 8
        },
        {
          "c": "foo"
        }
      ],
      "B": "8"
    }
  """)

  it("should format a single error") {
    val q = json.decodeFrom[Int]("a" ~> "b")
    q.fold(formatErrorReport, "FAIL") shouldBe "/a/b (6:14): Expecting a number here, but a boolean was found instead."
  }

  it("should format errors, one per line") {
    val r = json("a" ~> **).flatMap(_.decode[Int])
    r.fold(formatErrorReport, identity) shouldBe
      """| - /a/f (5:14): Expecting a number here, but a string was found instead.
         | - /a/b (6:14): Expecting a number here, but a boolean was found instead.""".stripMargin
  }
}
