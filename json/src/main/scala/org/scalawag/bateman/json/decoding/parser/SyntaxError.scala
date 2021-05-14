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

package org.scalawag.bateman.json.decoding.parser

import org.scalawag.bateman.json.decoding.JLocation
import org.scalawag.bateman.json.decoding.parser.tokenizer.{
  CharStream,
  CloseBrace,
  CloseBracket,
  Colon,
  Comma,
  EndOfInput,
  False,
  Null,
  NumberToken,
  OpenBrace,
  OpenBracket,
  StringToken,
  Token,
  True
}

final case class SyntaxError(location: JLocation, reason: String) {
  val description: String = s"syntax error: $reason ($location)"
}

object SyntaxError extends StringUtils {
  def apply(in: CharStream, expected: String): SyntaxError =
    if (in.chars.isEmpty)
      SyntaxError(in.position, s"expecting $expected at end of input")
    else {
      val upcomingSnippet = truncate(12, makePrintable)(in.chars)
      SyntaxError(in.position, s"""expecting $expected, found: $upcomingSnippet""")
    }

  def apply(in: Token, expected: String): SyntaxError = {
    val at =
      in match {
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
        case _: EndOfInput   => " at end of input"
      }

    SyntaxError(in.position, s"expecting $expected$at")
  }
}
