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

package org.scalawag.bateman.json.generic.codec

import org.scalawag.bateman.json.decoding.DecodeResult
import org.scalawag.bateman.json.generic.decoding.TraitDecoder
import org.scalawag.bateman.json.generic.encoding.TraitEncoder
import org.scalawag.bateman.json.{decoding, encoding}

case class TraitCodec[Trait, Context](
    encoder: TraitEncoder[Trait],
    decoder: TraitDecoder[Trait, Context],
) extends TraitEncoder[Trait]
    with TraitDecoder[Trait, Context] {
  override def decode(in: decoding.JObject, context: Context): DecodeResult[Trait] = decoder.decode(in, context)
  override def encode(a: Trait): encoding.JObject = encoder.encode(a)
}
