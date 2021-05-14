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

package org.scalawag.bateman.json

import org.scalawag.bateman.json.decoding.{DecodeResult, ContextualDecoder}
import org.scalawag.bateman.json.encoding.Encoder

class ContextualCodec[From, A, +To, Context](
    val decoder: ContextualDecoder[From, A, Context],
    val encoder: Encoder[A, To]
) extends Encoder[A, To]
    with ContextualDecoder[From, A, Context] {
  override def encode(a: A): To = encoder.encode(a)
  override def decode(in: From, context: Context): DecodeResult[A] = decoder.decode(in, context)
}

object ContextualCodec {
  def apply[From, A, To, Context](implicit
      codec: ContextualCodec[From, A, To, Context]
  ): ContextualCodec[From, A, To, Context] = codec

  def apply[From, A, To, Context](
      decoder: ContextualDecoder[From, A, Context],
      encoder: Encoder[A, To]
  ): ContextualCodec[From, A, To, Context] = new ContextualCodec(decoder, encoder)

  implicit def toEncoder[A, To](implicit codec: ContextualCodec[Nothing, A, To, Nothing]): Encoder[A, To] =
    codec.encoder
  implicit def toDecoder[From, A, Context](implicit
      codec: ContextualCodec[From, A, Nothing, Context]
  ): ContextualDecoder[From, A, Context] = codec.decoder
}
