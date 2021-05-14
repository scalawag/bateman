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

package org.scalawag.bateman.jsonapi.decoding

import org.scalawag.bateman.json.decoding.{Decoder, JObject, JString}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.{SourceTag, semiauto}
import org.scalawag.bateman.jsonapi.encoding
import shapeless.tag.@@

final case class ErrorSource(
    src: JSource @@ SourceTag,
    pointer: Option[JString] = None,
    parameter: Option[JString] = None
) {
  def toEncoding: encoding.ErrorSource =
    encoding.ErrorSource(
      pointer = pointer.map(_.value),
      parameter = parameter.map(_.value)
    )
}

object ErrorSource {
  implicit val decoder: Decoder[JObject, ErrorSource] = semiauto.deriveDecoderForCaseClass[ErrorSource, Any]()
}

final case class Error(
    src: JSource @@ SourceTag,
    id: Option[JString],
    links: Option[Links],
    status: Option[JString],
    code: Option[JString],
    title: Option[JString],
    detail: Option[JString],
    source: Option[ErrorSource],
    meta: Option[Meta]
) {
  def toEncoding: encoding.Error =
    encoding.Error(
      id = id.map(_.value),
      links = links.map(_.toEncoding),
      status = status.map(_.value),
      code = code.map(_.value),
      title = title.map(_.value),
      detail = detail.map(_.value),
      source = source.map(_.toEncoding),
      meta = meta.map(_.toEncoding)
    )
}

object Error {
  implicit val decoder: Decoder[JObject, Error] = semiauto.deriveDecoderForCaseClass[Error, Any]()
}
