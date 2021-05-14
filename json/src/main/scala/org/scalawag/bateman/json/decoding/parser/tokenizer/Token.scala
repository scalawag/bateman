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

import org.scalawag.bateman.json.decoding.JLocation

sealed trait Token {
  val position: JLocation
}

sealed trait ValidToken extends Token

sealed trait StructuralToken extends ValidToken

final case class OpenBrace(position: JLocation) extends StructuralToken
final case class CloseBrace(position: JLocation) extends StructuralToken
final case class OpenBracket(position: JLocation) extends StructuralToken
final case class CloseBracket(position: JLocation) extends StructuralToken
final case class Colon(position: JLocation) extends StructuralToken
final case class Comma(position: JLocation) extends StructuralToken

sealed trait PrimitiveToken extends ValidToken

sealed trait Literal extends PrimitiveToken

final case class True(position: JLocation) extends Literal
final case class False(position: JLocation) extends Literal
final case class Null(position: JLocation) extends Literal

final case class StringToken(position: JLocation, value: String) extends PrimitiveToken
final case class NumberToken(position: JLocation, value: String) extends PrimitiveToken

final case class EndOfInput(position: JLocation) extends Token
