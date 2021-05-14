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

import cats.syntax.either._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.DataDrivenTestUtils
import org.scalawag.bateman.json.decoding.JLocation
import org.scalawag.bateman.json.decoding.parser.SyntaxError

class StringCharCollectorTest extends AnyFunSpec with Matchers with DataDrivenTestUtils {
  val cases = Iterable[DataDrivenTestCase[(String, Either[(Int, String), (Int, String)])]](
    """"basic"""" -> Right(7 -> "basic"),
    """"\""""" -> Right(4 -> "\""),
    """"\\"""" -> Right(4 -> "\\"),
    """"\/"""" -> Right(4 -> "/"),
    """"\b"""" -> Right(4 -> "\b"),
    """"\f"""" -> Right(4 -> "\f"),
    """"\n"""" -> Right(4 -> "\n"),
    """"\r"""" -> Right(4 -> "\r"),
    """"\t"""" -> Right(4 -> "\t"),
    """"\t\t"""" -> Right(6 -> "\t\t"),
    """"a\ta"""" -> Right(6 -> "a\ta"),
    """"\ta"""" -> Right(5 -> "\ta"),
    """"a\t"""" -> Right(5 -> "a\t"),
    """"\"\""""" -> Right(6 -> "\"\""),
    // This craziness is required to prevent scala from interpreting the unicode escape (even in a multiline string!)
    s""""\\${'u'}0b94"""" -> Right(8 -> "\u0b94"),
    s""""\\${'u'}D83C\\${'u'}DF89"""" -> Right(14 -> "\uD83C\uDF89"),
    s""""\\${'u'}d83C\\${'u'}dF89"""" -> Right(14 -> "\uD83C\uDF89"),
    s""""\\${'u'}D83c\\${'u'}dF89"""" -> Right(14 -> "\uD83C\uDF89"),
    """"\"""" -> Left(4 -> "expecting a legal string character, escape sequence or end quote"),
    "\"abc\u0000\"" -> Left(5 -> "expecting a legal string character, escape sequence or end quote"),
    "\"\u0000\"" -> Left(2 -> "expecting a legal string character, escape sequence or end quote"),
    "\"a" -> Left(3, "expecting a legal string character, escape sequence or end quote"),
    """"\x""" -> Left(3 -> """expecting escape character [bfnrtu\/"]"""),
    s""""\\${'u'}X""" -> Left(4 -> "expecting a hexadecimal digit"),
    "a" -> Left(1, "expecting a beginning quote"),
  )

  cases.foreach {
    case DataDrivenTestCase((in, Right((len, actual))), pos) =>
      it(s"should collect $len character${if (len == 1) "" else "s"} of $in") {
        val cs = CharStream(in.toStream)
        val expectedCs = cs.drop(len)
        val expectedToken = StringToken(JLocation(1, 1), actual)
        StringCharCollector.stringToken(cs) shouldBe (expectedCs, expectedToken).asRight
      }(pos)

    case DataDrivenTestCase((in, Left((failPos, failMsg))), pos) =>
      it(s"should fail at character $failPos of $in") {
        val cs = CharStream(in.toStream)
        val res = StringCharCollector.stringToken(cs)
        res shouldBe SyntaxError(cs.drop(failPos - 1), failMsg).asLeft
      }(pos)
  }
}
