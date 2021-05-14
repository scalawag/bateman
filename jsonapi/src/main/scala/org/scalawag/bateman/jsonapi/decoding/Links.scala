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

import cats.syntax.validated._
import cats.syntax.apply._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.decoding.{DecodeResult, Decoder, JObject, JString, UnspecifiedField}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.jsonapi.encoding

final case class Links(src: JObject, mappings: Map[JString, Link]) {
  private val bareMap: Map[String, Link] = mappings.map { case (k, v) => k.value -> v }
  def get(key: String): Option[Link] = bareMap.get(key)

  def toEncoding: Map[String, encoding.Link] = mappings map { case (k, v) => k.value -> v.toEncoding }
}

object Links {
  implicit val decoder: Decoder[JObject, Links] = Decoder { in =>
    (
      DecodeResult.pure(in),
      in.as[Map[JString, Link]]
    ).mapN(Links.apply)
  }
}

trait HasLinks {
  val src: JSource
  val links: Option[Links]

  def requiredLinks: DecodeResult[Links] =
    links.map(_.validNec).getOrElse(UnspecifiedField(src.root, "links").invalidNec)
}
