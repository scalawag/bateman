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

import cats.data.NonEmptyChain
import cats.syntax.validated._
import cats.syntax.apply._
import org.scalawag.bateman.json.decoding.query.TraverseQuery._
import org.scalawag.bateman.json.decoding.DecodeResult
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.jsonapi.query._
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.jsonapi.Model.{Blue, Circle, Drawing, Portfolio, RGB, Red, Square, Triangle}
import org.scalawag.bateman.jsonapi.decoding.{Document, ResourceObject}

import java.util.UUID

class ParserTest extends AnyFunSpec with Matchers with ParserTestUtils {
  it("should decode") {
    val in = parseAs[Document]("""
      {
        "data" : {
          "type" : "Portfolio",
          "id" : "77777777-7777-7777-7777-777777777777",
          "relationships" : {
            "full" : {
              "data" : {
                "type" : "Drawing",
                "id" : "00000000-0000-0000-0000-000000000000"
              }
            },
            "sparse" : {
              "data" : {
                "type" : "Drawing",
                "id" : "88888888-8888-8888-8888-888888888888"
              }
            }
          }
        },
        "included" : [
          {
            "type" : "Square",
            "id" : "55555555-5555-5555-5555-555555555555",
            "attributes" : {
              "label" : null,
              "color" : ["red", "blue", { "r": 1, "g": 0, "b": 1}],
              "side_length" : 12
            },
            "links": {
            },
            "meta": {
              "version": 7
            }
          },
          {
            "type" : "Drawing",
            "id" : "00000000-0000-0000-0000-000000000000",
            "attributes" : {
              "title" : "bart",
              "sub_title" : "sub"
            },
            "relationships" : {
              "primary" : {
                "data" : {
                  "type" : "circle",
                  "id" : "11111111-1111-1111-1111-111111111111"
                }
              },
              "sh_apes" : {
                "data" : [
                  {
                    "type" : "circle",
                    "id" : "22222222-2222-2222-2222-222222222222"
                  },
                  {
                    "type" : "Square",
                    "id" : "33333333-3333-3333-3333-333333333333"
                  },
                  {
                    "type" : "TRIANGLE",
                    "id" : "44444444-4444-4444-4444-444444444444"
                  }
                ]
              },
              "favorite" : {
                "data" : {
                  "type" : "Square",
                  "id" : "55555555-5555-5555-5555-555555555555"
                },
                "meta": {
                  "team": 6
                }
              }
            }
          }
        ]
      }
    """)

//    val f = in.requiredData
//      .andThen(_.singular)
//      .andThen(_.as[ResourceObject])
//      .andThen(_.as[Portfolio])
//      .andThen(_.full.toResourceIdentifier.included(in))
//      .andThen(_.as[Drawing])

    val f = in.cquery(in)(_ ~> data ~> required ~> as[ResourceObject] ~> as[Portfolio])

    f shouldBe
      Portfolio(
        UUID.fromString("77777777-7777-7777-7777-777777777777"),
        Drawing(
          UUID.fromString("00000000-0000-0000-0000-000000000000"),
          "bart",
          Circle.Id(UUID.fromString("11111111-1111-1111-1111-111111111111")),
          Some("sub"),
          List(
            Circle.Id(UUID.fromString("22222222-2222-2222-2222-222222222222")),
            Square.Id(UUID.fromString("33333333-3333-3333-3333-333333333333")),
            Triangle.Id(UUID.fromString("44444444-4444-4444-4444-444444444444")),
          ),
          Some(
            Square(
              UUID.fromString("55555555-5555-5555-5555-555555555555"),
              None,
              List(Red, Blue, RGB(1, 0, 1)),
              12
            )
          )
        ),
        Some(
          Drawing.Id(
            UUID.fromString("88888888-8888-8888-8888-888888888888")
          )
        )
      ).validNec

//    println(f.map(_.full.favorite.toIterable.flatMap(_.color)).fold(formatErrorReport, identity))
//    println(f)

    /*
    val drawing =
      in.as[Document].andThen { doc =>
        val full: DecodeResult[Drawing] =
          doc.datum.andThen(_.as[Portfolio]).andThen { portfolio =>
            doc.requiredIncluded(portfolio.full.src).andThen(_.as[Drawing])
          }

        val metas: DecodeResult[Option[List[(Int, Option[Int])]]] =
          doc.included
            .traverse(
              _.asList.zipWithIndex
                .traverse {
                  case (inc, m) =>
                    inc.meta.flatMap(_.get("version")).traverse(_.as[Int]).map(m -> _)
                }
            )

        (full, metas).tupled
      }

    println(drawing.fold(ee => formatErrorReport(_), identity))
     */

    {
//      import org.scalawag.bateman.json.extractor.dsl._
//      val r: DecodeResult[Square] = (Extractor.root[JAny] /? "included" / 0 ).as[Square].extractOne(in)
//      println(r.fold(formatErrorReport, identity))
    }

  }
}
