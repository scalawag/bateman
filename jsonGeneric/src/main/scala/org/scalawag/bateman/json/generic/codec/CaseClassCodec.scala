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

import org.scalawag.bateman.json.decoding.{DecodeResult, JPointer}
import org.scalawag.bateman.json.{decoding, encoding}
import org.scalawag.bateman.json.generic.decoding.{CaseClassDecoder, JSource}
import org.scalawag.bateman.json.generic.encoding.CaseClassEncoder

case class CaseClassCodec[CaseClass, Context](
    encoder: CaseClassEncoder[CaseClass],
    decoder: CaseClassDecoder[CaseClass, Context],
) extends CaseClassEncoder[CaseClass]
    with CaseClassDecoder[CaseClass, Context] {
  override def discriminatorValue: String =
    decoder.discriminatorValue
  override def decode(
      in: decoding.JObject,
      context: Context,
      discriminatorField: Option[String]
  ): DecodeResult[CaseClass] =
    decoder.decode(in, context, discriminatorField)
  override def encode(a: CaseClass): encoding.JObject =
    encoder.encode(a)
}
