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

import cats.syntax.either._
import org.scalawag.bateman.json.JLocation
import org.scalawag.bateman.json.parser.{InvalidLiteral, SyntaxError, UnexpectedChars}

import scala.collection.compat.immutable.LazyList

/** The output of this module is a stream of tokens, representing the tokens in the JSON specification. There's
  *  also one representing the end of the input, so that we will have the position for error reporting. This module
  *  may emit lexical errors (though they are called "syntax errors" here). This includes things like invalid numbers,
  *  invalid characters within strings and unsupported structural tokens/punctuation. Note that this module doesn't
  *  not check syntax at all (e.g., the structure of a JSON object as a series of tokens). That's left to the
  *  Eventizer. External consumers don't directly consume this output stream. It's only intended to be used by the
  *  Eventizer.
  *
  *  The last item of the stream (and only the last item of the stream) will be either a SyntaxError or an EndOfInput
  *  token. It is a bug if there are tokens after that. It is a bug if neither of those is present as the last element
  *  of the output stream.
  */

object Tokenizer {
  type TokenStream = LazyList[Either[SyntaxError, Token]]

  private def raise[A](error: SyntaxError): TokenStream =
    LazyList(error.asLeft)

  def makeToken[A](in: CharStream, ctor: JLocation => A): Either[SyntaxError, A] =
    ctor(in.location).asRight

  private def endOfInput(in: CharStream): TokenStream =
    LazyList(makeToken(in, EndOfText.apply))

  private def literal(in: CharStream): TokenStream = {
    class StartsWith(s: String) {
      def unapply(arg: LazyList[Char]): Option[String] =
        if (arg.zip(s).forall(x => x._1 == x._2))
          Some(s)
        else
          None
    }

    object startsWithNull extends StartsWith("null")
    object startsWithTrue extends StartsWith("true")
    object startsWithFalse extends StartsWith("false")

    in.chars match {
      case startsWithNull(_)  => makeToken(in, Null.apply) #:: tokenize(in.drop(4))
      case startsWithTrue(_)  => makeToken(in, True.apply) #:: tokenize(in.drop(4))
      case startsWithFalse(_) => makeToken(in, False.apply) #:: tokenize(in.drop(5))
      case _                  => raise(InvalidLiteral(in))
    }
  }

  // TODO: clean up the interactions between the collectors and the tokenizer here

  private def string(in: CharStream): TokenStream =
    StringCharCollector.stringToken(in) match {
      case Left(e)          => LazyList(e.asLeft)
      case Right((next, t)) => t.asRight #:: tokenize(next)
    }

  private def number(in: CharStream): TokenStream =
    NumberCharCollector.numberToken(in) match {
      case Left(e)          => LazyList(e.asLeft)
      case Right((next, t)) => t.asRight #:: tokenize(next)
    }

  private def token(in: CharStream): TokenStream = {
    in.chars.headOption
      .map {
        case '{'                        => makeToken(in, OpenBrace.apply) #:: tokenize(in.drop(1))
        case '}'                        => makeToken(in, CloseBrace.apply) #:: tokenize(in.drop(1))
        case '['                        => makeToken(in, OpenBracket.apply) #:: tokenize(in.drop(1))
        case ']'                        => makeToken(in, CloseBracket.apply) #:: tokenize(in.drop(1))
        case ':'                        => makeToken(in, Colon.apply) #:: tokenize(in.drop(1))
        case ','                        => makeToken(in, Comma.apply) #:: tokenize(in.drop(1))
        case '"'                        => string(in)
        case c if c.isLetter            => literal(in)
        case c if c == '-' || c.isDigit => number(in)
        case c                          => raise(UnexpectedChars(in, "valid JSON"))
      }
      .getOrElse(endOfInput(in))
  }

  def tokenize(in: CharStream): TokenStream = token(in.skipWhitespace)
}
