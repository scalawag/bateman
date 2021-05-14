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
import org.scalawag.bateman.json.decoding.parser.SyntaxError

import java.io.File
import scala.annotation.tailrec
import scala.io.Source

final case class CharStream(chars: Stream[Char], position: JLocation) {
  @tailrec
  def drop(n: Int): CharStream = {
    if (n < 1)
      this
    else
      (chars match {
        // Don't count carriage return as a new line if followed by a real newline.
        case '\r' #:: '\n' #:: tail => CharStream(tail, position.copy(column = position.column + 1))
        case ('\r' | '\n') #:: tail => CharStream(tail, position.copy(line = position.line + 1, column = 1))
        case _ #:: tail             => CharStream(tail, position.copy(column = position.column + 1))
      }).drop(n - 1)
  }

  @tailrec
  def skipWhitespace: CharStream =
    chars match {
      case ('\r' | '\n' | '\t' | ' ') #:: _ => drop(1).skipWhitespace
      case _                                => this
    }
}

object CharStream {
  def apply(src: Stream[Char], name: Option[String] = None): CharStream = CharStream(src, JLocation(1, 1, name))

  /** You are still responsible for closing this Source. */
  def fromSource(src: Source, name: Option[String] = None): CharStream = apply(src.toStream, name)
  // TODO: how to close this source?
  def fromFile(file: File): CharStream = fromSource(Source.fromFile(file), Some(file.getPath))
}
