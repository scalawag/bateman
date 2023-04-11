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

package org.scalawag.bateman.jsonapi

import org.scalawag.bateman.json.{JAny, JObject, JObjectDecoder, JResult}
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.jsonapi.encoding.{EncodeResult, FieldsSpec, IncludeSpec, ResourceEncoder}

class ResourceCodec[A](
    val encoder: ResourceEncoder[A],
    val decoder: JObjectDecoder[A],
) extends ResourceEncoder[A]
    with JObjectDecoder[A] {
  override def encodeResource(
      in: A,
      includeSpec: IncludeSpec,
      fieldsSpec: FieldsSpec,
      discriminators: JObject
  ): EncodeResult[ResourceEncoder.Encoded] =
    encoder.encodeResource(in, includeSpec, fieldsSpec, discriminators)
  override def decode(in: JFocus[JObject], discriminatorFields: Set[JFocus[JAny]]): JResult[A] =
    decoder.decode(in, discriminatorFields)
}

object ResourceCodec {
  def apply[A](implicit codec: ResourceCodec[A]): ResourceCodec[A] = codec
}
