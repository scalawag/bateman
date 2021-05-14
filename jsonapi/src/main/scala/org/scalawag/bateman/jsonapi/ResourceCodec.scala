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

package org.scalawag.bateman.jsonapi

import org.scalawag.bateman.json.decoding.{ContextualDecoder, DecodeResult}
import org.scalawag.bateman.jsonapi.decoding.{Document, ResourceDecoder}
import org.scalawag.bateman.jsonapi.encoding.{EncodeResult, FieldsSpec, IncludeSpec, ResourceEncoder, ResourceLike}

class ResourceCodec[From, A, +To <: ResourceLike](
    val decoder: ResourceDecoder[From, A],
    val encoder: ResourceEncoder[A, To]
) extends ResourceDecoder[From, A]
    with ResourceEncoder[A, To] {
  override def encodeResource(
      in: A,
      includeSpec: IncludeSpec,
      fieldsSpec: FieldsSpec
  ): EncodeResult[ResourceEncoder.PartiallyEncoded[To]] =
    encoder.encodeResource(in, includeSpec, fieldsSpec)
  override def decode(from: From, context: Document): DecodeResult[A] = decoder.decode(from, context)

  implicit def toEncoder: ResourceEncoder[A, To] = encoder
  implicit def toDecoder: ContextualDecoder[From, A, Document] = decoder
}

object ResourceCodec {
  def apply[From, A, To <: ResourceLike](implicit codec: ResourceCodec[From, A, To]): ResourceCodec[From, A, To] = codec

  def apply[From, A, To <: ResourceLike](
      decoder: ResourceDecoder[From, A],
      encoder: ResourceEncoder[A, To]
  ): ResourceCodec[From, A, To] =
    new ResourceCodec(decoder, encoder)
}
