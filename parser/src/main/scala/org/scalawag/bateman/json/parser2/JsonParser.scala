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

package org.scalawag.bateman.json.parser2

import cats.syntax.validated._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.decoding._
import fastparse.NoWhitespace._
import fastparse._

import scala.io.Source

object JsonParser {
  def parse(text: String, source: Option[String] = None): ParseResult[JAny] =
    new JsonParser(text, source).parse.map(addPointers)

  // Do this all at once after parsing so that we don't have to maintain the state down through the parser.
  private def addPointers(j: JAny): JAny = {
    def go(pointer: JPointer, jany: JAny): JAny =
      jany match {
        case x: JNull    => x.copy(pointer = pointer)
        case x: JBoolean => x.copy(pointer = pointer)
        case x: JString  => x.copy(pointer = pointer)
        case x: JNumber  => x.copy(pointer = pointer)
        case x: JObject =>
          x.copy(
            fieldList = x.fieldList.map { f =>
              val p = pointer / f.name.value
              f.copy(
                name = f.name.copy(pointer = p),
                value = go(p, f.value)
              )
            },
            pointer = pointer
          )
        case x: JArray =>
          x.copy(
            items = x.items.zipWithIndex.map {
              case (v, n) =>
                go(pointer / n, v)
            },
            pointer = pointer
          )
      }

    go(JPointer.Root, j)
  }
}

class JsonParser private (input: String, source: Option[String] = None) {

  private def ws[_: P] = P(CharIn("\t\r\n ").rep(1))

  private def hexDigit[_: P] = P(CharIn("0-9", "a-f", "A-F"))

  private def twoCharacterEscape[_: P]: P[Char] =
    P(escapedCharacter.map {
      case c @ ("\"" | "/" | "\\") => c.head
      case "b"                     => '\b'
      case "f"                     => '\f'
      case "r"                     => '\r'
      case "n"                     => '\n'
      case "t"                     => '\t'
    })

  private def sixCharacterEscape[_: P]: P[Char] =
    P("u" ~ hexDigit.rep(4, "", 4).!).map { s =>
      java.lang.Integer.parseInt(s, 16).toChar
    }

  private def escapedCharacter[_: P]: P[String] =
    P((CharIn("\\\"/\\\\bfnrt").!))

  private def escapeSequence[_: P]: P[Char] =
    P("\\" ~/ (twoCharacterEscape | sixCharacterEscape))

  // strings: TODO: exclude control characters (U+0000 to U+001F) here
  private def quote[_: P] = P("\"")
  private def notQuote[_: P] = P(CharPred(_ != '"').!.map(_.head))
  private def quotedStringChars[_: P]: P[String] =
    P((escapeSequence | notQuote).rep).map(_.mkString)
  private def string[_: P] =
    P(Index ~ quote ~ quotedStringChars ~ quote).map {
      case (index, s) =>
        JString(s, locationForIndex(index), null)
    }

  private def comma[_: P] = P(ws.? ~ "," ~/ ws.?)

  private def array[_: P]: P[JArray] =
    P(Index ~ "[" ~/ jsonExpr.rep(sep = comma) ~ "]").map {
      case (index, items) =>
        // Prepend the index onto each of the pointers contained in here.
        JArray(items.toList, locationForIndex(index), null)
    }

  private def field[_: P]: P[JField] =
    P {
      (string ~ ws.? ~ ":" ~ ws.? ~ jsonExpr).map(JField.tupled)
    }

  private def `object`[_: P]: P[JObject] =
    P {
      (Index ~ "{" ~/ ws.? ~ field.rep(sep = comma) ~ ws.? ~/ "}").map {
        case (index, fields) =>
          JObject(fields.toList, locationForIndex(index), null)
      }
    }

  private def `"null"`[_: P]: P[JNull] =
    P {
      (Index ~ "null").map { index =>
        JNull(locationForIndex(index), null)
      }
    }

  private def boolean[_: P]: P[JBoolean] =
    P {
      (Index ~ ("true" | "false").!).map {
        case (index, b) =>
          JBoolean(b.toBoolean, locationForIndex(index), null)
      }
    }

  private def digit[_: P] = P(CharIn("0-9"))
  private def digits[_: P] = P(digit.rep(1))
  private def sign[_: P] = P("-")
  private def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)
  private def fractional[_: P] = P("." ~ digits)
  private def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

  private def number[_: P]: P[JNumber] =
    P {
      (Index ~ (sign.? ~ integral ~/ fractional.? ~ exponent.?).!).map {
        case (index, s) =>
          JNumber(s, locationForIndex(index), null)
      }
    }

  private def jsonExpr[_: P]: P[JAny] =
    P {
      ws.? ~ (`object` | array | `"null"` | boolean | number | string) ~ ws.?
    }

  private def root[_: P] = P(jsonExpr ~ End)

  def parse: ParseResult[JAny] =
    fastparse.parse(input, root(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => Right(value)
      case f: Parsed.Failure        => Left(SyntaxError(locationForIndex(f.index), f))
    }

  // This is just a list of the index offset for the beginning of each line to speed up the location derivation.
  private val lineForIndex: List[Int] =
    Source
      .fromString(input)
      .getLines()
      .map(_.length)
      .scanLeft(0)(_ + _ + 1)
      .toList

  private def locationForIndex(index: Int): JLocation = {
    val (offset, line) = lineForIndex.zipWithIndex.takeWhile(_._1 <= index).last
    JLocation(line + 1, index - offset + 1)
  }
}
