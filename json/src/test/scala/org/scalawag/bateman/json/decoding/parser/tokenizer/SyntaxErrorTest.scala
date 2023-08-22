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

package org.scalawag.bateman.json.decoding.parser.tokenizer

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.DataDrivenTestUtils
import org.scalawag.bateman.json.decoding.JLocation
import org.scalawag.bateman.json.decoding.parser.{StringUtils, SyntaxError}

import scala.collection.compat.immutable.LazyList

class SyntaxErrorTest extends AnyFunSpec with Matchers with DataDrivenTestUtils with StringUtils {
  describe("from tokenizer") {
    val cases = Iterable[DataDrivenTestCase[(String, String)]](
      "" -> "expecting foo at end of input",
      "abcd" -> "expecting foo, found: abcd",
      "abcdefghijklmnop" -> "expecting foo, found: abcdefghijkl...",
      "\n\b\r\t\f" -> "expecting foo, found: \\n\\b\\r\\t\\f",
      "\uD83C\uDF89" -> "expecting foo, found: \uD83C\uDF89",
      "\u0001" -> "expecting foo, found: \\u0001",
    )

    cases foreach {
      case DataDrivenTestCase((upcoming, msg), pos) =>
        it(s"should report correctly for ${makePrintableString(upcoming)}") {
          val cs = CharStream(upcoming.to(LazyList))
          SyntaxError(cs, "foo") shouldBe SyntaxError(JLocation(1, 1), msg)
        }(pos)
    }
  }

  describe("from eventizer") {
    val cases = Iterable[DataDrivenTestCase[(Token, String)]](
      OpenBrace(JLocation(1, 1)) -> "expecting foo, found: {",
      CloseBrace(JLocation(1, 1)) -> "expecting foo, found: }",
      OpenBracket(JLocation(1, 1)) -> "expecting foo, found: [",
      CloseBracket(JLocation(1, 1)) -> "expecting foo, found: ]",
      Colon(JLocation(1, 1)) -> "expecting foo, found: :",
      Comma(JLocation(1, 1)) -> "expecting foo, found: ,",
      True(JLocation(1, 1)) -> "expecting foo, found: true",
      False(JLocation(1, 1)) -> "expecting foo, found: false",
      Null(JLocation(1, 1)) -> "expecting foo, found: null",
      StringToken(JLocation(1, 1), "x") -> "expecting foo, found: \"x\"",
      StringToken(JLocation(1, 1), "xxxxxxxxxxxxxx") -> "expecting foo, found: \"xxxxxxxxxxx...",
      StringToken(JLocation(1, 1), "\n\b\r\t\f") -> "expecting foo, found: \"\\n\\b\\r\\t\\f\"",
      StringToken(JLocation(1, 1), "\uD83C\uDF89") -> "expecting foo, found: \"\uD83C\uDF89\"",
      StringToken(JLocation(1, 1), "\u0000") -> "expecting foo, found: \"\\u0000\"",
      NumberToken(JLocation(1, 1), "55555555555555") -> "expecting foo, found: 555555555555...",
      EndOfInput(JLocation(13, 17)) -> "expecting foo at end of input",
    )

    cases foreach {
      case DataDrivenTestCase((token, msg), pos) =>
        it(s"should report correctly for $token") {
          SyntaxError(token, "foo") shouldBe SyntaxError(token.position, msg)
        }(pos)
    }
  }
}
