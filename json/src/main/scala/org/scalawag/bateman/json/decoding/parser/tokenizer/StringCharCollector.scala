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

import scala.annotation.tailrec

private[parser] object StringCharCollector extends CharCollector {
  private object hex {
    def unapply(arg: Char): Option[Char] = Some(arg).filter(isHex)
  }

  private object escape {
    def unapply(c: Char): Option[Char] =
      c match {
        case 'b'  => Some('\b')
        case 'f'  => Some('\f')
        case 'n'  => Some('\n')
        case 'r'  => Some('\r')
        case 't'  => Some('\t')
        case '\\' => Some('\\')
        case '"'  => Some('"')
        case '/'  => Some('/')
        case _    => None
      }
  }

  private def isHex(c: Char) = c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'
  private def illegal(c: Char) = c == '\\' || c == '"' || c <= 0x1f

  private def collectUnicodeEscape(in: CharStream, acc: List[Char]) =
    in.chars.take(4).toList match {
      case List(hex(d1), hex(d2), hex(d3), hex(d4)) =>
        val d = Iterable(d1, d2, d3, d4).mkString
        val i = Integer.parseInt(d, 16)
        collectString(in.drop(4), i.toChar :: acc)
      case next =>
        val hexes = next.takeWhile(isHex)
        Left(SyntaxError(in.drop(hexes.length), "expecting a hexadecimal digit"))
    }

  private def collectEscape(in: CharStream, acc: List[Char]) =
    in.chars.headOption match {
      case Some(escape(c)) => collectString(in.drop(1), c :: acc)
      case Some('u')       => collectUnicodeEscape(in.drop(1), acc)
      case _               => Left(SyntaxError(in, "expecting escape character [bfnrtu\\/\"]"))
    }

  @tailrec
  private def collectString(in: CharStream, acc: List[Char]): Either[SyntaxError, (CharStream, List[Char])] =
    in.chars.headOption match {
      case Some('"')              => Right((in.drop(1), acc.reverse))
      case Some('\\')             => collectEscape(in.drop(1), acc)
      case Some(c) if !illegal(c) => collectString(in.drop(1), c :: acc)

      case _ => Left(SyntaxError(in, "expecting a legal string character, escape sequence or end quote"))
    }

  private def initialQuote(in: CharStream): Either[SyntaxError, (CharStream, List[Char])] =
    in.chars.headOption match {
      case Some('"') => collectString(in.drop(1), Nil)
      case _         => Left(SyntaxError(in, ("expecting a beginning quote")))
    }

  def stringToken(in: CharStream): Either[SyntaxError, (CharStream, Token)] =
    initialQuote(in) flatMap {
      case (next, cc) => Right(next -> StringToken(in.position, cc.mkString))
    }
}
