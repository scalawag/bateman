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

/*
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

import org.scalawag.bateman.json.{Decoder, JString}
import org.scalawag.bateman.json.DecodeError.formatErrorReport
import scala.collection.compat.immutable.LazyList

object Errors {
  def main(args: Array[String]): Unit = {
    import org.scalawag.bateman.json.JAny
    import org.scalawag.bateman.json.parser._
    import org.scalawag.bateman.json.query._

    val j = toJAny("""
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
  """.to(LazyList)).getOrElse(???)

    val q = root[JAny, Any] ~> "a" ~> "b" ~> as[Int]
    println(q(j, ()).fold(formatErrorReport, identity))

    val r = j.query(_ ~> "a" ~> "b" ~> as[Int])
    println(r.fold(formatErrorReport, identity))

    val s = j.query(_ ~> "b" ~> as[Int])
    println(s.fold(formatErrorReport, identity))

    val t = j.query(_ ~> "B" ~> narrow[JString] ~> decode[Int](Decoder.jstringToJNumber))
    println(t.fold(formatErrorReport, identity))
  }
}
*/
