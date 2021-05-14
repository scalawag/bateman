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

package org.scalawag.bateman.json.decoding

import org.scalawag.bateman.json.decoding.parser.documenter.Documentizer
import org.scalawag.bateman.json.decoding.parser.eventizer.Eventizer
import org.scalawag.bateman.json.decoding.parser.tokenizer.{CharStream, Tokenizer}

package object parser {
  type ParseResult[+A] = Either[SyntaxError, A]

  private def toTokens(src: Stream[Char], loc: Option[String] = None): Tokenizer.TokenStream =
    Tokenizer.tokenize(CharStream(src, loc))

  def toEvents(src: Stream[Char], loc: Option[String] = None): Eventizer.EventStream =
    Eventizer.eventize(toTokens(src, loc))

  def toJAnys(src: Stream[Char], loc: Option[String] = None): Documentizer.OutStream =
    Documentizer.documentize(Eventizer.eventize(toTokens(src, loc)))

  def toJAny(src: Stream[Char], loc: Option[String] = None): ParseResult[JAny] = {
    val anys = Documentizer.documentize(Eventizer.eventizeOne(toTokens(src, loc)))
    // Make sure there's not a second item that's an error. There could have been another (illegal) value in the input.
    anys.find(_.isLeft).getOrElse(anys.head)
  }
}
