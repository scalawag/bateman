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
import scala.collection.compat.immutable.LazyList
import scala.io.Source

final case class CharStream(chars: LazyList[Char], position: JLocation) {
  @tailrec
  def drop(n: Int): CharStream = {
    if (n < 1)
      this
    else {
      (chars.head match {
        // Don't count carriage return as a new line if followed by a real newline.
        case '\r' if chars.tail.headOption.contains('\n') =>
          CharStream(chars.tail.tail, position.copy(column = position.column + 1))
        case '\r' | '\n' =>
          CharStream(chars.tail, position.copy(line = position.line + 1, column = 1))
        case _ => CharStream(chars.tail, position.copy(column = position.column + 1))
      }).drop(n - 1)
    }
  }

  @tailrec
  def skipWhitespace: CharStream =
    chars.headOption match {
      case Some('\r' | '\n' | '\t' | ' ') => drop(1).skipWhitespace
      case _                              => this
    }
}

object CharStream {
  def apply(src: LazyList[Char], name: Option[String] = None): CharStream = CharStream(src, JLocation(1, 1, name))

  /** You are still responsible for closing this Source. */
  def fromSource(src: Source, name: Option[String] = None): CharStream = apply(src.to(LazyList), name)
  // TODO: how to close this source?
  def fromFile(file: File): CharStream = fromSource(Source.fromFile(file), Some(file.getPath))
}
