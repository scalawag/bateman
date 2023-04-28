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

import cats.syntax.either._
import test.json.{BatemanTestBase, DataDrivenTestUtils}
import org.scalawag.bateman.json.JLocation
import org.scalawag.bateman.json.parser.{SyntaxError, UnexpectedChars}
import org.scalawag.bateman.json.parser.tokenizer._

import scala.collection.compat.immutable.LazyList

class StringCharCollectorTest extends BatemanTestBase with DataDrivenTestUtils {
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
    s""""${"a" * 8192}"""" -> Right(8194 -> "a" * 8192),
    s""""${"a" * 1048576}"""" -> Right(1048578 -> "a" * 1048576),
    // This craziness is required to prevent scala from interpreting the unicode escape (even in a multiline string!)
    s""""\\${'u'}0b94"""" -> Right(8 -> "\u0b94"),
    s""""\\${'u'}D83C\\${'u'}DF89"""" -> Right(14 -> "\uD83C\uDF89"),
    s""""\\${'u'}d83C\\${'u'}dF89"""" -> Right(14 -> "\uD83C\uDF89"),
    s""""\\${'u'}D83c\\${'u'}dF89"""" -> Right(14 -> "\uD83C\uDF89"),
    """"\"""" -> Left(4 -> "a legal string character, escape sequence or end quote"),
    "\"abc\u0000\"" -> Left(5 -> "a legal string character, escape sequence or end quote"),
    "\"\u0000\"" -> Left(2 -> "a legal string character, escape sequence or end quote"),
    "\"a" -> Left(3, "a legal string character, escape sequence or end quote"),
    """"\x""" -> Left(3 -> """an escape character [bfnrtu\/"]"""),
    s""""\\${'u'}X""" -> Left(4 -> "a hexadecimal digit"),
    "a" -> Left(1, "an opening quote"),
  )

  cases.foreach {
    case DataDrivenTestCase((in, Right((len, actual))), pos) =>
      implicit val p = pos
      it(s"should collect $len character${if (len == 1) "" else "s"} of ${truncate(in)}") {
        val cs = CharStream(in.to(LazyList))
        val expectedCs = cs.drop(len)
        val expectedToken = StringToken(JLocation(1, 1), actual)
        StringCharCollector.stringToken(cs) shouldBe (expectedCs, expectedToken).asRight
      }

    case DataDrivenTestCase((in, Left((failPos, failMsg))), pos) =>
      implicit val p = pos
      it(s"should fail at character $failPos of ${truncate(in, 120)}") {
        val cs = CharStream(in.to(LazyList))
        val res = StringCharCollector.stringToken(cs)
        res shouldBe UnexpectedChars(cs.drop(failPos - 1), failMsg).asLeft
      }
  }
}