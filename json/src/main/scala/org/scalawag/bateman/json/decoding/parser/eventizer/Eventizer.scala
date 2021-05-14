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

package org.scalawag.bateman.json.decoding.parser.eventizer

import cats.syntax.either._
import org.scalawag.bateman.json.decoding.parser.SyntaxError
import org.scalawag.bateman.json.decoding.parser.tokenizer.{
  CloseBrace,
  CloseBracket,
  Colon,
  Comma,
  EndOfInput,
  OpenBrace,
  OpenBracket,
  PrimitiveToken,
  StringToken,
  Token,
  ValidToken
}
import org.scalawag.bateman.json.decoding.parser.tokenizer.Tokenizer.TokenStream

/** The input to this module comes from the Tokenizer. That means that it is guaranteed to have a terminating
  * SyntaxError or EndOfInput. An external caller is not allowed to provide input to this module, so we can make
  * this assumption without having extra error-handling code.
  *
  * Syntax errors are detected in this module. If a lexical error appears on the input stream, it is simply emitted
  * on the output stream and processing is stopped.
  *
  * This module is also responsible for determining whether exactly one JSON value is allowed in the input (which is
  * typical) or whether zero or more is allowed (which is the case for some log files, etc.).
  *
  * The output of this module is directly available to consumers that want to process very large files without keeping
  * them in memory. Due to that, there is no chance for error checking beyond this module. The output must be a valid
  * (i.e., properly nested) stream of events. The last item (and only the last item) in the output stream _may_ be a
  * SyntaxError. If that is not the case, the stream is valid. That means that the cardinality of values is correct
  *  and that the "start" and "end" events are properly matched. If the last item _is_ an error, all bets are off.
  *  Consumers may do with the partial stream what they will, but events may not be balanced.
  *
  *  Having multiple SyntaxErrors in the output stream is a bug as is having any items after the first SyntaxError.
  */

object Eventizer {
  type EventStream = Stream[Either[SyntaxError, Event]]

  private type State = TokenStream => EventStream

  private def syntaxError(token: Token, reason: String)(tokens: TokenStream): EventStream =
    Stream(SyntaxError(token.position, reason).asLeft)

  private def unexpected(expected: String)(in: TokenStream): EventStream =
    in.head match {
      case Right(t) => syntaxError(t, s"expected $expected, not token: $t")(in)
      case Left(e)  => Stream(e.asLeft)
    }

  private def fieldEnd(next: State)(in: TokenStream): EventStream =
    FieldEnd.asRight #:: next(in)

  private def fieldValue(next: State)(in: TokenStream): EventStream =
    in.head match {
      case Right(x: Colon) => value(fieldEnd(next))(in.drop(1))
      case _               => unexpected("a colon")(in)
    }

  private def fieldName(next: State)(in: TokenStream): EventStream =
    in.head match {
      case Right(x: StringToken) => FieldStart(x).asRight #:: fieldValue(next)(in.drop(1))
      case _                     => unexpected("a field name (string)")(in)
    }

  private def nonEmptyFields(next: State)(in: TokenStream): EventStream =
    fieldName(additionalFields(next))(in)

  private def additionalFields(next: State)(in: TokenStream): EventStream =
    in.head match {
      case Right(t: CloseBrace) => ObjectEnd(t).asRight #:: next(in.drop(1))
      case Right(_: Comma)      => nonEmptyFields(next)(in.drop(1))
      case _                    => unexpected("a closing curly bracket or a comma")(in)
    }

  private def fields(next: State)(in: TokenStream): EventStream =
    in.head match {
      case Right(t: CloseBrace) => ObjectEnd(t).asRight #:: next(in.drop(1))
      case _                    => nonEmptyFields(next)(in)
    }

  private def nonEmptyItems(next: State)(in: TokenStream): EventStream =
    value(additionalItems(next))(in)

  private def additionalItems(next: State)(in: TokenStream): EventStream =
    in.head match {
      case Right(t: CloseBracket) => ArrayEnd(t).asRight #:: next(in.drop(1))
      case Right(_: Comma)        => nonEmptyItems(next)(in.drop(1))
      case _                      => unexpected("a closing square bracket or a comma")(in)
    }

  private def items(next: State)(in: TokenStream): EventStream =
    in.head match {
      case Right(t: CloseBracket) => ArrayEnd(t).asRight #:: next(in.drop(1))
      case _                      => nonEmptyItems(next)(in)
    }

  private def value(next: State)(in: TokenStream): EventStream =
    in.head match {
      case Right(x: PrimitiveToken) => Value(x).asRight #:: next(in.drop(1))
      case Right(b: OpenBrace)      => ObjectStart(b).asRight #:: fields(next)(in.drop(1))
      case Right(b: OpenBracket)    => ArrayStart(b).asRight #:: items(next)(in.drop(1))
      case Right(t: EndOfInput)     => Stream.Empty
      case _                        => unexpected("a JSON value")(in)
    }

  private def end(in: TokenStream): EventStream =
    in.head match {
      case Right(t: EndOfInput) => Stream.empty
      case _                    => unexpected("end of input")(in)
    }

  private def values(in: TokenStream): EventStream =
    in.headOption match {
      case None => Stream.Empty
      case _    => value(values)(in)
    }

  def eventize(src: TokenStream): EventStream = values(src)

  def eventizeOne(in: TokenStream): EventStream =
    in.head match {
      case Right(_: ValidToken) => value(end)(in)
      case t                    => unexpected("a JSON value")(in)
    }
}
