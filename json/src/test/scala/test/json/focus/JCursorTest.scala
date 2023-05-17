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

package test.json.focus

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.JCursor
import org.scalawag.bateman.json.lens._
import test.json.BatemanTestBase

class JCursorTest extends BatemanTestBase {
  private val json = parseAs[JObject]("""
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
        "foo",
        []
      ],
      "deep": [
        [
          { "a": 4 },
          { "a": true },
          { "a": "g" }
        ],
        [
          { "a": 17 },
          { "a": 83 },
          { "a": 56.78e4 }
        ]
      ]
    }
  """)

  it("should extract the values") {
    val lens = "deep" ~> * ~> 0 ~> "a"
    val out = lens(json)
    out.map(_.values).shouldSucceed shouldBe out.shouldSucceed.foci.map(_.value)
  }

  def emptyFoci: JCursor[List, JAny] = json.apply("w".**).shouldSucceed

  it("should handle delete when empty") {
    emptyFoci.delete()
  }

  it("should handle modify when empty") {
    emptyFoci.modify(_ => JNull)
  }
}
