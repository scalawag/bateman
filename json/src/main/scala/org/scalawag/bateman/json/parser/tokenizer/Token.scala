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

package org.scalawag.bateman.json.parser.tokenizer

import org.scalawag.bateman.json.JLocation

sealed trait Token {
  val location: JLocation
}

object Token {
  // Used in error messages
  def describe(token: Token): String =
    token match {
      case t: OpenBrace    => "an opening curly bracket ('{')"
      case t: CloseBrace   => "a closing curly bracket ('}')"
      case t: OpenBracket  => "an opening square bracket ('[')"
      case t: CloseBracket => "a closing square bracket (']')"
      case t: Colon        => "a colon (':')"
      case t: Comma        => "a comma (',')"
      case t: True         => "a boolean literal ('true')"
      case t: False        => "a boolean literal ('false')"
      case t: Null         => "a null literal ('null')"
      case t: StringToken  => s"a string literal ('${t.value}')"
      case t: NumberToken  => s"a number literal ('${t.value}')"
      case t: EndOfText   => "the end of text"
    }
}

sealed trait ValidToken extends Token

sealed trait StructuralToken extends ValidToken

final case class OpenBrace(location: JLocation) extends StructuralToken
final case class CloseBrace(location: JLocation) extends StructuralToken
final case class OpenBracket(location: JLocation) extends StructuralToken
final case class CloseBracket(location: JLocation) extends StructuralToken
final case class Colon(location: JLocation) extends StructuralToken
final case class Comma(location: JLocation) extends StructuralToken

sealed trait PrimitiveToken extends ValidToken

sealed trait Literal extends PrimitiveToken

final case class True(location: JLocation) extends Literal
final case class False(location: JLocation) extends Literal
final case class Null(location: JLocation) extends Literal

final case class StringToken(location: JLocation, value: String) extends PrimitiveToken
final case class NumberToken(location: JLocation, value: String) extends PrimitiveToken

final case class EndOfText(location: JLocation) extends Token
