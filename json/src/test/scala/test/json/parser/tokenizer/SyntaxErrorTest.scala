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

package test.json.parser.tokenizer

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import test.json.DataDrivenTestUtils
import org.scalawag.bateman.json.JLocation
import org.scalawag.bateman.json.parser.{StringUtils, UnexpectedToken, UnexpectedChars}
import org.scalawag.bateman.json.parser.tokenizer._

import scala.collection.compat.immutable.LazyList

class SyntaxErrorTest extends AnyFunSpec with Matchers with DataDrivenTestUtils with StringUtils {
  describe("from tokenizer") {
    val cases = Iterable[DataDrivenTestCase[(String, String)]](
      "" -> "expecting foo at end of text",
      "abcd" -> "expecting foo, found: abcd",
      "abcdefghijklmnop" -> "expecting foo, found: abcdefghijkl...",
      "\n\b\r\t\f" -> "expecting foo, found: \\n\\b\\r\\t\\f",
      "\uD83C\uDF89" -> "expecting foo, found: \uD83C\uDF89",
      "\u0001" -> "expecting foo, found: \\u0001",
    )

    cases foreach {
      case DataDrivenTestCase((upcoming, msg), pos) =>
        implicit val p = pos
        it(s"should report correctly for ${makePrintableString(upcoming)}") {
          val cs = CharStream(upcoming.to(LazyList))
          val e = UnexpectedChars(cs, "foo")
          e.location shouldBe JLocation(1, 1)
          e.reason shouldBe msg
        }
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
      EndOfText(JLocation(13, 17)) -> "expecting foo at end of text",
    )

    cases foreach {
      case DataDrivenTestCase((token, msg), pos) =>
        implicit val p = pos
        it(s"should report correctly for $token") {
          val e = UnexpectedToken(token, "foo")
          e.location shouldBe token.location
          e.reason shouldBe msg
        }
    }
  }
}
