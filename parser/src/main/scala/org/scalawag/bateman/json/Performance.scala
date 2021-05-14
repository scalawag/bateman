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

package org.scalawag.bateman.json

import org.scalawag.bateman.json.decoding.parser

import scala.io.Source
import scala.util.Try

object Performance {
  def bigJson = Source.fromFile("/Users/jpatterson/downloads/large-file.json")
  val smallJson = """
    {
      "a": 12.3,
      "b": false,
      "c": 5,
      "d": [ "g", "h", 4, true, null],
      "e": {
        "e": "f",
        "f": null,
        "g": true
      }
    }
  """

  def time[A](fn: => A): (Long, A) = {
    val start = System.currentTimeMillis
    Try(fn).map((System.currentTimeMillis - start, _)).get
  }

  def main(args: Array[String]): Unit = {
    val (newer, na) = time {
      val src = bigJson
      for {
        _ <- 1 to 10
      } yield parser.toJAny(src.toStream)
    }
    println(s"NA: ${na.head}")

    val large = bigJson.mkString
    val (old, oa) = time {
      for {
        _ <- 1 to 10
      } yield parser2.JsonParser.parse(large)
    }

    println(s"O:$old N:$newer")
    println(s"OA: ${oa.head}")
    println(s"NA: ${na.head}")
    println(oa.tail.forall(_ == oa.head) + " " + oa.length)
    println(na.tail.forall(_ == na.head) + " " + na.length)
    println(oa.head.getOrElse(???) == na.head.getOrElse(???))
  }

  def main2(args: Array[String]): Unit = {
    val (old, oa) = time {
      for {
        _ <- 1 to 10000
      } yield parser2.JsonParser.parse(smallJson)
    }
    val (newer, na) = time {
      val src = Source.fromString(smallJson)
      for {
        _ <- 1 to 10000
      } yield parser.toJAny(Source.fromString(smallJson).toStream)
    }

    println(s"O:$old N:$newer")
    println(s"OA: ${oa.head}")
    println(s"NA: ${na.head}")
    println(oa.tail.forall(_ == oa.head) + " " + oa.length)
    println(na.tail.forall(_ == na.head) + " " + na.length)
    println(oa.head.getOrElse(???) == na.head.getOrElse(???))
  }
}
