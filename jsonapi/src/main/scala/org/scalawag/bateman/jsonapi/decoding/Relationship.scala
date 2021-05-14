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

import org.scalawag.bateman.json.decoding.{Decoder, JObject}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.{SourceTag, semiauto}
import org.scalawag.bateman.jsonapi.encoding
import shapeless.tag.@@

final case class Relationship(
    src: JSource @@ SourceTag,
    data: Option[RelationshipData] = None,
    meta: Option[Meta] = None,
    links: Option[Links] = None,
) extends HasLinks
    with HasMeta
    with HasData[RelationshipData] {
  def toEncoding: encoding.Relationship =
    encoding.Relationship(
      data = data.map(_.toEncoding),
      meta = meta.map(_.toEncoding),
      links = links.map(_.toEncoding)
    )
}

object Relationship {
  implicit val decoder: Decoder[JObject, Relationship] = semiauto.deriveDecoderForCaseClass[Relationship, Any]()
}
