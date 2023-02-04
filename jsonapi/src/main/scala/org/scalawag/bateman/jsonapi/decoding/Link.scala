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

import org.scalawag.bateman.json.decoding.{Decoder, JAny, JObject, JString}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.{SourceTag, semiauto}
import org.scalawag.bateman.jsonapi.encoding
import shapeless.tag.@@

import scala.util.Try

sealed trait Link {
  def toEncoding: encoding.Link
}

object Link {
  implicit val decoder: Decoder[JAny, Link] = { (in, context) =>
    in.forType {
      case JString => Decoder[JAny, BareLink].decode(in, context)
      case JObject => Decoder[JAny, RichLink].decode(in, context)
    }
  }
}

final case class BareLink(href: JString) extends Link {
  def toEncoding: encoding.BareLink =
    encoding.BareLink(
      href = href.value
    )

  override def toString: String = Try(toEncoding.toString).getOrElse(super.toString())
}

object BareLink {
  implicit val decoder: Decoder[JString, BareLink] =
    Decoder[JString, JString].map(BareLink.apply)

}

final case class RichLink(src: JSource @@ SourceTag, href: Option[JString] = None, meta: Option[Meta] = None)
    extends Link
    with HasMeta {
  def toEncoding: encoding.RichLink =
    encoding.RichLink(
      href = href.map(_.value),
      meta = meta.map(_.toEncoding)
    )

  override def toString: String = Try(toEncoding.toString).getOrElse(super.toString())
}

object RichLink {
  implicit val decoder: Decoder[JObject, RichLink] = semiauto.deriveDecoderForCaseClass[RichLink, Any]()
}
