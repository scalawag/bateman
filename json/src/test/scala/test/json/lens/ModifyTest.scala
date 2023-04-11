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

package test.json.lens

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens.{focus, _}
import test.json.BatemanTestBase
import org.scalawag.bateman.json.focus.JFoci
import org.scalawag.bateman.json.focus.weak._

class ModifyTest extends BatemanTestBase {
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

  describe("editing") {
    it("should edit a value in the document") {
      json(focus ~> "g")
        .map(_.modifyValue {
          case arr: JArray => JNumber(arr.items.size)
          case _           => fail
        })
        .shouldSucceed
        .root
        .value shouldRenderTo
        parse("""
        {
          "a": {
            "g": 4,
            "f": "thing",
            "b": true
          },
          "b": 6,
          "g": 3,
          "deep":[
            [
              {"a": 4},
              {"a": true},
              {"a": "g"}
            ],
            [
              {"a": 17},
              {"a": 83},
              {"a": 56.78e4}
            ]
          ]
        }
      """).value
    }

    it("should edit a value in the document deeper") {
      json(focus ~> "deep" ~> 1)
        .map(_.modifyValue {
          case in: JArray => JNumber(in.items.size)
          case _          => fail
        })
        .shouldSucceed
        .root
        .value shouldEncodeTo
        parse("""
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
              3
            ]
          }
        """).value
    }

    it("should append a field to an object") {
      json(focus ~> "a" ~> narrow[JObject]).map(_.append("foo", JString("bar"))).shouldSucceed.root.value shouldEncodeTo
        parse("""
          {
            "a": {
              "g": 4,
              "f": "thing",
              "b": true,
              "foo": "bar"
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
        """).value
    }

    it("should prepend a field to an object") {
      json(focus ~> "a" ~> narrow[JObject])
        .map(_.prepend("foo", JString("bar")))
        .shouldSucceed
        .root
        .value shouldEncodeTo
        parse("""
          {
            "a": {
              "foo": "bar",
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
        """).value
    }

    it("should append a field to an array") {
      json(focus ~> "g" ~> narrow[JArray]).map(_.append(JString("bar"))).shouldSucceed.root.value shouldEncodeTo
        parse("""
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
              [],
              "bar"
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
        """).value
    }

    it("should prepend a field to an array") {
      json(focus ~> "g" ~> narrow[JArray]).map(_.prepend(JString("bar"))).shouldSucceed.root.value shouldEncodeTo
        parse("""
          {
            "a": {
              "g": 4,
              "f": "thing",
              "b": true
            },
            "b": 6,
            "g": [
              "bar",
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
        """).value
    }

    it("should modify multiple") {
      json(focus ~> "deep" ~> * ~> * ~> "a")
        .map(_.modify(_ => JNull))
        .shouldSucceed
        .root
        .map(_.value)
        .get shouldEncodeTo
        parse("""
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
                { "a": null },
                { "a": null },
                { "a": null }
              ],
              [
                { "a": null },
                { "a": null },
                { "a": null }
              ]
            ]
          }
        """).value
    }

    it("should delete multiple") {
      val edited =
        json(focus ~> "deep" ~> * ~> * ~> "a").flatMap(_.delete()).shouldSucceed

      edited.root.get.value shouldEncodeTo
        parse("""
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
                {},
                {},
                {}
              ],
              [
                {},
                {},
                {}
              ]
            ]
          }
        """).value

      edited.foci.size shouldBe 6
    }

    it("should delete multiple and dedup") {
      val edited = json(focus ~> "deep" ~> * ~> *).flatMap(_.delete()).shouldSucceed

      edited.root.get.value shouldEncodeTo
        parse("""
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
              [],
              []
            ]
          }
        """).value

      edited.foci.size shouldBe 2
    }
  }
}
