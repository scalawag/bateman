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

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.DecodeError.formatErrorReport
import org.scalawag.bateman.json.decoding.JAny
import org.scalawag.bateman.json.encoding.{
  AsciiOnly,
  JString,
  SortedFields,
  PrettyRenderer,
  PrettySpaces2,
  PrettySpaces4
}
import org.scalawag.bateman.json.decoding.query.TraverseQuery.RichTraverseQuery
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.jsonapi.decoding.{Document, ResourceObject}
import org.scalawag.bateman.jsonapi.query._

class QueryTest extends AnyFunSpec with Matchers with ParserTestUtils {
  it("should extract and maintain cardinality") {
    val in = parse("""
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
            "type" : "Circle",
            "id" : "55555555-5555-5555-5555-555555555555",
            "attributes" : {
              "label" : null,
              "color" : ["green"],
              "side_length" : 12
            },
            "links": {
            },
            "meta": {
              "version": 7
            }
          }
        ]
      }
    """)

    // TODO: need to be able to lose this import by finding the implicits automatically
//    import org.scalawag.bateman.json.decoding.query.TraverseQuery._
//    import org.scalawag.bateman.json.decoding.query.Query._
//    val o = root[Document] ~> data ~> required
    val p = root[JAny, Unit] ~> "included" ~> 8
    val q = root[JAny, Unit] ~> "included" ~> index(0) ~> field("attributes") ~> field("color")
    val r = root[JAny, Unit] ~> "included" ~> index(0) ~> field("attributes") ~> field("color") ~> *
    val s = root[JAny, Unit] ~> "included" ~> index(0) ~> field("attributes") ~> field("color") ~> * ~>* *
    val t = root[JAny, Unit] ~> "included" ~> 0 ~> "attributes" ~> "color" ~> *
    val u = root[JAny, Unit] ~> as[Document] ~> meta ~> "version"
    val uu =
      root[Document, Unit] //~> data //~> required ~> as[ResourceObject] ~> relationship("vern") ~> data ~> required
    val v = root[JAny, Unit] ~> as[Document] ~>? meta ~> field("version")
//    val w = root[JAny] ~> "included" ~> * ~> field("attributes") ~> field("color") ~>* *
//    val x = root[JAny] ~> as[Document] ~> data ~> multiple ~> as[ResourceObject] ~> id

    println("=== p")
    println(p(in, ()).fold(formatErrorReport, _.toEncoding.render))
    println("=== q")
    println(q(in, ()).fold(formatErrorReport, _.toEncoding.render(PrettySpaces2)))
    println(q(in, ()).fold(formatErrorReport, _.toEncoding.render(new PrettySpaces4 with SortedFields)))
    println("=== r")
    println(r(in, ()).fold(formatErrorReport, _.map(_.toEncoding.render).mkString("\n")))
    println("=== s")
    println(s(in, ()).fold(formatErrorReport, _.map(_.toEncoding.render).mkString("\n")))
    println("=== t")
    println(t(in, ()).fold(formatErrorReport, _.map(_.toEncoding.render).mkString("\n")))
    println("=== u")
    println(u(in, ()).fold(formatErrorReport, _.toEncoding.render))
    println("=== v")
    println(v(in, ()).fold(formatErrorReport, _.map(_.toEncoding.render).mkString("\n")))
//    println("=== w")
//    println(w(in, ()).fold(formatErrorReport, _.map(_.toEncoding.render(PrettySpaces4)).mkString("\n")))
//    println("=== x")
//    println(x(in, ()).fold(formatErrorReport, _.map(_.toString).mkString("\n")))
  }

  it("should render weird chars and escapes") {
    println(JString("\uD83D\uDE22").render)
    println(JString("middle \t\n\r\f\t\b \" of string").render)
    val r = new PrettySpaces2 with AsciiOnly
    println(JString("\uD83D\uDE22").render(r))
    println(JString("middle \t\n\r\f\t\b \" of string").render(r))
  }
}
