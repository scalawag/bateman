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

package org.scalawag.bateman.jsonapi.generic

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.DecodeError.formatErrorReport
import org.scalawag.bateman.json.decoding.{ContextualDecoder, JAny, JObject, JString}
import org.scalawag.bateman.json.decoding.parser.toJAny
import org.scalawag.bateman.json.decoding.query.{Query, as}
import org.scalawag.bateman.jsonapi.decoding.{
  Data,
  Document,
  ResourceIdentifier,
  ResourceIdentifierDecoder,
  ResourceObject
}
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.jsonapi.query._
import shapeless.{LabelledGeneric, Lazy}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Mapper
import shapeless.tag.@@

class DerivedResourceIdentifierDecoderFactoryTest extends AnyFunSpec with Matchers with ParserTestUtils {
  val q1 = root[JAny, Unit] ~> as[Document] ~> data ~> required ~> as[ResourceObject] ~> relationship("parent")
  val q2 = q1 ~> data ~> required ~> id

  val in = parse("""
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
              "type": "my_id",
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

  in.extract(q1, ())
    .andThen(_.requiredData)
    .andThen(_.required)
    .fold(ee => fail(ee.toString), identity)

  it("should make a thing") {
    final case class MyId(id: String @@ IdTag)

    implicit val dec = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyId]("my_id")

    // TODO: should be able to either automatically reroot or explicitly reroot a query (recontextualize)
    val doc = in.query(_ ~> as[Document]).getOrElse(???)

    val qe = root[Document, Document] ~> data ~> required ~> as[ResourceIdentifier] ~> as[MyId]
    println(
      doc
        .cquery(doc)(
          _ ~> data ~> required ~> as[ResourceObject] ~> relationship("parent") ~> data ~> required ~> as[MyId]
        )
        .fold(ee => fail(formatErrorReport(ee)), identity)
    )

  }
}
