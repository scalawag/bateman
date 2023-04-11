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
import org.scalawag.bateman.json.JLocation
import org.scalawag.bateman.json.parser.{SyntaxError, UnexpectedChars}
import org.scalawag.bateman.json.parser.tokenizer._
import test.json.BatemanTestBase

import scala.collection.compat.immutable.LazyList

class NumberCharCollectorTest extends BatemanTestBase {
  val cases = Iterable(
    "5.67" -> Right(4),
    "5.67 " -> Right(4),
    "5.67," -> Right(4),
    "5.67]" -> Right(4),
    "5.67}" -> Right(4),
    "5 " -> Right(1),
    "0 " -> Right(1),
    "-0 " -> Right(2),
    "-5 " -> Right(2),
    "5a" -> Right(1),
    "00" -> Right(1),
    "-54.000034e45 " -> Right(13),
    "-54.000034E45 " -> Right(13),
    "17.0123456789e+1 " -> Right(16),
    "17.0123456789e-1 " -> Right(16),
    "-54.000034f45 " -> Right(10),
    "-0.000034e45 " -> Right(12),
    "-01.000034e45 " -> Right(2),
    s"1.${"0" * 8192}" -> Right(8194),
    "-.000034e45" -> Left(2),
    "5." -> Left(3),
    "-" -> Left(2),
    "-1.e3" -> Left(4),
    "-1e" -> Left(4),
    "-1e-" -> Left(5),
    "-1e+" -> Left(5),
  )

  cases.foreach {
    case (in, Right(len)) =>
      it(s"should collect $len character${if (len == 1) "" else "s"} of ${truncate(in)}") {
        val cs = CharStream(in.to(LazyList))
        val expectedCs = cs.drop(len)
        val expectedToken = NumberToken(JLocation(1, 1), cs.chars.take(len).mkString)
        NumberCharCollector.numberToken(cs) shouldBe (expectedCs, expectedToken).asRight
      }

    case (in, Left(failPos)) =>
      it(s"should fail at character $failPos of ${truncate(in)}") {
        val cs = CharStream(in.to(LazyList))
        val res = NumberCharCollector.numberToken(cs)
        res shouldBe UnexpectedChars(cs.drop(failPos - 1), "a digit").asLeft
      }
  }
}
