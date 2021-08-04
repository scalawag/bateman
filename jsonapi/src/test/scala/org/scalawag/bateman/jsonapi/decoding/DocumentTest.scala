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

package org.scalawag.bateman.jsonapi.decoding

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.{JLocation, JNull, JObject, JPointer, JString, UnspecifiedField}
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.encoding

class DocumentTest extends AnyFunSpec with Matchers with ParserTestUtils {
  it("should treat no-ID datum as ResourceObjectOptionalId") {
    val d = parseAs[Document]("""
      {
        "data": {
          "type": "my_class"
        }
      }
    """)

    d.data.collect { case ResourceObjectOptionalIdData(_, r) => r.toEncoding } shouldBe
      Some(encoding.ResourceObjectOptionalId("my_class"))
  }

  it("should treat attribute-containing datum as ResourceObject") {
    val d = parseAs[Document]("""
      {
        "data": {
          "type": "my_class",
          "id": "ID",
          "attributes": {
          }
        }
      }
    """)

    d.data.collect { case ResourceObjectData(_, r) => r.toEncoding } shouldBe
      Some(encoding.ResourceObject("my_class", "ID", Some(Map.empty)))
  }

  it("should treat datum as ResourceIdentifier if possible") {
    val d = parseAs[Document]("""
      {
        "data": {
          "type": "my_class",
          "id": "ID"
        }
      }
    """)

    d.data.collect { case ResourceIdentifierData(_, r) => r.toEncoding } shouldBe
      Some(encoding.ResourceIdentifier("my_class", "ID"))
  }

  it("should treat attribute-containing data as ResourceObjects") {
    val d = parseAs[Document]("""
      {
        "data": [
          {
            "type": "my_class",
            "id": "ID1",
            "attributes": {
            }
          },
          {
            "type": "my_class",
            "id": "ID2"
          }
        ]
      }
    """)

    d.data.collect { case ResourceObjectsData(_, rr) => rr.map(_.toEncoding) } shouldBe
      Some(
        List(
          encoding.ResourceObject("my_class", "ID1", Some(Map.empty)),
          encoding.ResourceObject("my_class", "ID2")
        )
      )
  }

  it("should treat data as ResourceIdentifiers if possible") {
    val d = parseAs[Document]("""
      {
        "data": [
          {
            "type": "my_class",
            "id": "ID1"
          },
          {
            "type": "my_class",
            "id": "ID2"
          }
        ]
      }
    """)

    d.data.collect { case ResourceIdentifiersData(_, rr) => rr.map(_.toEncoding) } shouldBe
      Some(List(encoding.ResourceIdentifier("my_class", "ID1"), encoding.ResourceIdentifier("my_class", "ID2")))
  }

  // You can't have multiple with no ID, per the JSON:API spec.
  it("should fail for no-ID data") {
    val d = parse("""
      {
        "data": [
          {
            "type": "my_class",
            "id": "ID"
          },
          {
            "type": "my_class"
          }
        ]
      }
    """)

    val o = d.query(_ ~> "data" ~> 1 ~> as[JObject]).getOrElse(???)
    d.as[Document] shouldBe UnspecifiedField(o, JPointer.Root / "id").invalidNec
  }

  it("should work for null data") {
    val d = parseAs[Document]("""
      {
        "data": null
      }
    """)

    d.data shouldBe Some(NullData(JNull(JLocation(3, 17), JPointer.Root / "data")))
  }
}
