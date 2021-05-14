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

import cats.implicits.catsSyntaxEitherId
import org.scalawag.bateman.json.decoding.parser.SyntaxError

private[parser] object NumberCharCollector extends CharCollector {
  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def accept(next: State[List[Char]]): State[List[Char]] =
    for {
      c <- peek
      _ <- consume
      n <- next
    } yield c.head :: n

  private def nonEmptyDigits(next: State[List[Char]]): State[List[Char]] =
    peek flatMap {
      case Some(c) if isDigit(c) => accept(digits(next))
      case _                     => syntaxError("expecting a digit")
    }

  private def digits(next: State[List[Char]]): State[List[Char]] =
    peek flatMap {
      case Some(c) if isDigit(c) => accept(digits(next))
      case _                     => next
    }

  private val leadingMinus: State[List[Char]] =
    peek flatMap {
      case Some('-') => accept(nonfractional)
      case _         => nonfractional
    }

  private val nonfractional: State[List[Char]] =
    peek flatMap {
      case Some('0')             => accept(fractional)
      case Some(c) if isDigit(c) => accept(digits(fractional))
      case _                     => syntaxError("expecting a digit")
    }

  private val fractional: State[List[Char]] =
    peek flatMap {
      case Some('.') => accept(nonEmptyDigits(exponent))
      case _         => exponent
    }

  private val exponent: State[List[Char]] =
    peek flatMap {
      case Some('e' | 'E') => accept(sign)
      case _               => end
    }

  private val sign: State[List[Char]] =
    peek flatMap {
      case Some('+' | '-') => accept(nonEmptyDigits(end))
      case _               => nonEmptyDigits(end)
    }

  private val end: State[List[Char]] = pure(Nil)

  def numberToken(in: CharStream): Either[SyntaxError, (CharStream, Token)] =
    leadingMinus.run(in) flatMap {
      case (next, cc) => (next -> NumberToken(in.position, cc.mkString)).asRight
    }
}
