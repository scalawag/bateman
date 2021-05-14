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
import org.scalawag.bateman.json.decoding.JLocation
import org.scalawag.bateman.json.decoding.parser.SyntaxError

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
  type TokenStream = Stream[Either[SyntaxError, Token]]

  private def syntaxError[A](in: CharStream, reason: String): TokenStream =
    Stream(SyntaxError(in.position, reason).asLeft)

  def makeToken[A](in: CharStream, ctor: JLocation => A): Either[SyntaxError, A] =
    ctor(in.position).asRight

  private def endOfInput(in: CharStream): TokenStream =
    Stream(makeToken(in, EndOfInput))

  private def literal(in: CharStream): TokenStream = {
    class StartsWith(s: String) {
      def unapply(arg: Stream[Char]): Option[String] =
        if (arg.zip(s).forall(x => x._1 == x._2))
          Some(s)
        else
          None
    }

    object startsWithNull extends StartsWith("null")
    object startsWithTrue extends StartsWith("true")
    object startsWithFalse extends StartsWith("false")

    in.chars match {
      case startsWithNull(_)  => makeToken(in, Null) #:: tokenize(in.drop(4))
      case startsWithTrue(_)  => makeToken(in, True) #:: tokenize(in.drop(4))
      case startsWithFalse(_) => makeToken(in, False) #:: tokenize(in.drop(5))
      case _                  => syntaxError(in, "unexpected literal found")
    }
  }

  // TODO: clean up the interactions between the collectors and the tokenizer here

  private def string(in: CharStream): TokenStream =
    StringCharCollector.stringToken(in) match {
      case Left(e)          => Stream(e.asLeft)
      case Right((next, t)) => t.asRight #:: tokenize(next)
    }

  private def number(in: CharStream): TokenStream =
    NumberCharCollector.numberToken(in) match {
      case Left(e)          => Stream(e.asLeft)
      case Right((next, t)) => t.asRight #:: tokenize(next)
    }

  private def token(in: CharStream): TokenStream = {
    in.chars.headOption
      .map {
        case '{'                        => makeToken(in, OpenBrace) #:: tokenize(in.drop(1))
        case '}'                        => makeToken(in, CloseBrace) #:: tokenize(in.drop(1))
        case '['                        => makeToken(in, OpenBracket) #:: tokenize(in.drop(1))
        case ']'                        => makeToken(in, CloseBracket) #:: tokenize(in.drop(1))
        case ':'                        => makeToken(in, Colon) #:: tokenize(in.drop(1))
        case ','                        => makeToken(in, Comma) #:: tokenize(in.drop(1))
        case '"'                        => string(in)
        case c if c.isLetter            => literal(in)
        case c if c == '-' || c.isDigit => number(in)
        case c                          => syntaxError(in, s"unexpected character '$c'")
      }
      .getOrElse(endOfInput(in))
  }

  def tokenize(in: CharStream): TokenStream = token(in.skipWhitespace)
}
