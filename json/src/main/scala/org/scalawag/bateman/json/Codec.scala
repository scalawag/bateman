// bateman -- Copyright 2021-2023 -- Justin Patterson
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

package org.scalawag.bateman.json

import org.scalawag.bateman.json.focus.JFocus

class Codec[A <: JAny, B](
    val encoder: Encoder[B, A],
    val decoder: Decoder[A, B],
) extends Encoder[B, A]
    with Decoder[A, B] {
  override def encode(a: B): A = encoder.encode(a)
  override def decode(in: JFocus[A]): JResult[B] = decoder.decode(in)
}

object Codec {
  def apply[A <: JAny, B](implicit codec: Codec[A, B]): Codec[A, B] = codec
//  def apply[A <: JAny, B](decoder: Decoder[A, B], encoder: Encoder[B, A]): Codec[A, B] = new Codec(decoder, encoder)
}
