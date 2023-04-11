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

package org.scalawag.bateman.json.parser

import org.scalawag.bateman.json.{JErrorFormatters, JLocation}
import org.scalawag.bateman.json.parser.tokenizer.{CharStream, CloseBrace, CloseBracket, Colon, Comma, EndOfText, False, Null, NumberToken, OpenBrace, OpenBracket, StringToken, Token, True}
import StringUtils._

sealed trait SyntaxError extends Exception {
  def location: JLocation
  def reason: String

  override def getMessage: String = s"syntax error: $reason ($location)"
}

final case class InvalidLiteral(in: CharStream) extends SyntaxError {
  override val location: JLocation = in.location
  override val reason: String = s"invalid literal found '${in.chars.takeWhile(_.isLetter).mkString}'"
}

final case class UnexpectedToken(actual: Token, expected: String*) extends SyntaxError {
  override val location: JLocation = actual.location
  override val reason: String = {
    val at =
      actual match {
        case _: OpenBrace    => ", found: {"
        case _: CloseBrace   => ", found: }"
        case _: OpenBracket  => ", found: ["
        case _: CloseBracket => ", found: ]"
        case _: Colon        => ", found: :"
        case _: Comma        => ", found: ,"
        case _: True         => ", found: true"
        case _: False        => ", found: false"
        case _: Null         => ", found: null"
        case s: StringToken  => s""", found: ${truncate(12, makePrintable, "\"")(s.value)}"""
        case n: NumberToken  => s""", found: ${truncate(12)(n.value)}"""
        case _: EndOfText    => " at end of text"
      }

    val expectedStr = JErrorFormatters.formatOrList(expected.iterator)
    s"expecting $expectedStr$at"
  }
}

final case class UnexpectedChars(actual: CharStream, expected: String) extends SyntaxError {
  override val location: JLocation = actual.location
  override val reason: String =
    if (actual.chars.isEmpty)
      s"expecting $expected at end of text"
    else {
      val upcomingSnippet = truncate(12, makePrintable)(actual.chars)
      s"""expecting $expected, found: $upcomingSnippet"""
    }
}
