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

  @tailrec
  private def collectString(in: CharStream, acc: List[Char]): Either[SyntaxError, (CharStream, List[Char])] =
    in.chars match {
      case '"' #:: _ =>
        Right((in.drop(1), acc.reverse))

      case '\\' #:: tail =>
        tail match {
          case escape(c) #:: _ => collectString(in.drop(2), c :: acc)
          case 'u' #:: hex(d1) #:: hex(d2) #:: hex(d3) #:: hex(d4) #:: _ =>
            val d = Iterable(d1, d2, d3, d4).mkString
            val i = Integer.parseInt(d, 16)
            collectString(in.drop(6), i.toChar :: acc)
          case 'u' #:: tail =>
            val hexes = tail.takeWhile(isHex)
            Left(SyntaxError(in.drop(2 + hexes.length), "expecting a hexadecimal digit"))
          case _ =>
            Left(SyntaxError(in.drop(1), "expecting escape character [bfnrtu\\/\"]"))
        }

      case c #:: _ if !illegal(c) =>
        collectString(in.drop(1), c :: acc)

      case _ => Left(SyntaxError(in, "expecting a legal string character, escape sequence or end quote"))
    }

  private def initialQuote(in: CharStream): Either[SyntaxError, (CharStream, List[Char])] =
    in.chars match {
      case '"' #:: _ => collectString(in.drop(1), Nil)
      case _         => Left(SyntaxError(in, ("expecting a beginning quote")))
    }

  def stringToken(in: CharStream): Either[SyntaxError, (CharStream, Token)] =
    initialQuote(in) flatMap {
      case (next, cc) => Right(next -> StringToken(in.position, cc.mkString))
    }
}
