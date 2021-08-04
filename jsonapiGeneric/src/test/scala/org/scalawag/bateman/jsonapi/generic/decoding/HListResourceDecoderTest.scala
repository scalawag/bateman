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

package org.scalawag.bateman.jsonapi.generic.decoding

import cats.data.NonEmptyChain
import cats.syntax.validated._
import org.scalatest.Assertion
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.DecodeError.formatErrorReport
import org.scalawag.bateman.json.decoding.UnexpectedValue
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, SnakeCase}
import org.scalawag.bateman.jsonapi.query._
import org.scalawag.bateman.jsonapi.Model.{Blue, Green, RGB, Red}
import org.scalawag.bateman.jsonapi.decoding.{Document, ResourceIdentifier, ResourceObject, ResourceObjectDecoder}
import org.scalawag.bateman.jsonapi.generic.TestModel._
import org.scalawag.bateman.jsonapi.generic.semiauto

import java.util.UUID
import scala.annotation.tailrec
import scala.util.Try

class HListResourceDecoderTest extends AnyFunSpec with Matchers with ParserTestUtils {
  describe("allowUnknownFields = false") {
    val decoders = makeDecoders(
      Config.default.copy(allowUnknownFields = false, fieldNameMapping = CaseTransformation(CamelCase, SnakeCase))
    )
    import decoders._

    it("should decode with allowUnknownFields = false") {
      val square =
        Square(
          UUID.randomUUID,
          Some("blue square"),
          List(Red, Blue, RGB(3, 4, 3), Green),
          17
        )

      square.copy(sideLength = 11)

      val doc = parseAs[Document]("""
        {
          "data" : {
            "type" : "square",
            "id" : "55555555-5555-5555-5555-555555555555",
            "attributes" : {
              "label": "mike",
              "color" : ["red", "blue", { "r": 1, "g": 0, "b": 1 }],
              "side_length" : 12
            },
            "relationships": {
              "parent": {
                "data": {
                  "type": "square",
                  "id": "4"
                }
              }
            },
            "links": {
            },
            "meta": {
              "version": 7
            }
          }
        }
      """)

      val decoded = doc.cquery(doc)(_ ~> data ~> required ~> as[ResourceObject] ~> as[Square])
      val un1 = doc.src.root.query(_ ~> "data" ~> "relationships" ~> "parent").getOrElse(fail)
      val un2 = doc.src.root.query(_ ~> "data" ~> "meta" ~> "version").getOrElse(fail)
      decoded.leftMap(_.iterator.toSet) shouldBe Set(UnexpectedValue(un1), UnexpectedValue(un2)).invalid
    }
  }
}
