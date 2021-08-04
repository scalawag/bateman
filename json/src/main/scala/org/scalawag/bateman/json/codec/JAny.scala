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

package org.scalawag.bateman.json.codec

import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.{Decoder, JAnyDecoder}
import org.scalawag.bateman.json.encoding.JAnyEncoder
import org.scalawag.bateman.json.{ProgrammerError, decoding, encoding}

sealed trait JAny {
  def toEncoding: encoding.JAny
  def toDecoding: decoding.JAny
}

object JAny {
  def apply(jany: encoding.JAny): JAny = Encoding(jany)
  def apply(jany: decoding.JAny): JAny = Decoding(jany)

  case class Encoding(toEncoding: encoding.JAny) extends JAny {
    override def toDecoding: decoding.JAny =
      throw ProgrammerError("This value was not parsed and can not be turned into a decoding JAny.")
  }

  case class Decoding(toDecoding: decoding.JAny) extends JAny {
    override def toEncoding: encoding.JAny = toDecoding.toEncoding
  }

  implicit val decoder: JAnyDecoder[JAny] = Decoder(Decoding(_).validNec)
  implicit val encoder: JAnyEncoder[JAny] = _.toEncoding
}
