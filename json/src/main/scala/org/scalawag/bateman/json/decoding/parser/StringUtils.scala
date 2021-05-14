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

package org.scalawag.bateman.json.decoding.parser

import scala.annotation.tailrec

trait StringUtils {
  private val zeroes = "0000"

  def charHex(c: Char): String = {
    val s = c.toHexString
    (4 - s.length) match {
      case n if n > 0 => zeroes.take(n) + s
      case _          => s
    }
  }

  def truncate[A](maxLength: Int, fn: A => String = (_: Any).toString, delimiter: String = "")(
      aa: Iterable[A]
  ): String = {
    @tailrec
    def go(remainingLength: Int, remainingInput: Iterable[A], acc: String): String =
      if (remainingLength > 0 && remainingInput.nonEmpty) {
        val mapped = fn(remainingInput.head)
        go(remainingLength - mapped.length, remainingInput.tail, acc + mapped)
      } else if (remainingInput.nonEmpty)
        acc + "..."
      else
        acc + delimiter

    go(maxLength - delimiter.length, aa, delimiter)
  }

  def makePrintable(c: Char): String =
    c match {
      case '\"'            => "\\\""
      case '\\'            => "\\\\"
      case '\b'            => "\\b"
      case '\f'            => "\\f"
      case '\r'            => "\\r"
      case '\n'            => "\\n"
      case '\t'            => "\\t"
      case c if c < 0x001f => s"\\u${charHex(c)}"
      case c               => c.toString
    }

  def makePrintableString(s: String): String = s.map(makePrintable).mkString
}
