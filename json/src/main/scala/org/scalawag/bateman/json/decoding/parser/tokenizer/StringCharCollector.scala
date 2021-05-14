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

import org.scalawag.bateman.json.decoding.parser.SyntaxError

private[parser] object StringCharCollector extends CharCollector {
  private def isHex(c: Char) = c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'
  private def illegal(c: Char) = c == '\\' || c == '"' || c <= 0x1f

  private def append(c: Char): State[List[Char]] = chars.map(c :: _)

  private val hexDigit: State[Char] =
    for {
      c <- peek
      t <- c match {
        case Some(c) if isHex(c) => consume.flatMap(_ => pure(c))
        case _                   => syntaxError(s"expecting a hexadecimal digit")
      }
    } yield t

  private val unicodeEscape: State[List[Char]] =
    for {
      d1 <- hexDigit
      d2 <- hexDigit
      d3 <- hexDigit
      d4 <- hexDigit
      d = Iterable(d1, d2, d3, d4).mkString
      i = Integer.parseInt(d, 16)
      s <- append(i.toChar)
    } yield s

  private val escape: State[List[Char]] =
    for {
      c <- peek
      s <- c match {
        case Some('b')  => consume.flatMap(_ => append('\b'))
        case Some('f')  => consume.flatMap(_ => append('\f'))
        case Some('n')  => consume.flatMap(_ => append('\n'))
        case Some('r')  => consume.flatMap(_ => append('\r'))
        case Some('t')  => consume.flatMap(_ => append('\t'))
        case Some('\\') => consume.flatMap(_ => append('\\'))
        case Some('"')  => consume.flatMap(_ => append('"'))
        case Some('/')  => consume.flatMap(_ => append('/'))
        case Some('u')  => consume.flatMap(_ => unicodeEscape)
        case _          => syntaxError("expecting escape character [bfnrtu\\/\"]")
      }
    } yield s

  private val chars: State[List[Char]] =
    for {
      c <- peek
      s <- c match {
        case Some('"')              => consume.map(_ => Nil)
        case Some('\\')             => consume.flatMap(_ => escape)
        case Some(c) if !illegal(c) => consume.flatMap(_ => append(c))
        case _                      => syntaxError("expecting a legal string character, escape sequence or end quote")
      }
    } yield s

  private val string: State[List[Char]] =
    for {
      c <- peek
      cc <- c match {
        case Some('"') => consume.flatMap(_ => chars)
        case _         => syntaxError("expecting a beginning quote")
      }
    } yield cc

  def stringToken(in: CharStream): Either[SyntaxError, (CharStream, Token)] =
    string.run(in) flatMap {
      case (next, cc) => Right(next -> StringToken(in.position, cc.mkString))
    }
}
