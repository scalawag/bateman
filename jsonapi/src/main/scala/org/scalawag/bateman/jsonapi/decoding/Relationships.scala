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

import cats.syntax.apply._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.decoding.{DecodeResult, Decoder, JAny, JObject, JString}
import org.scalawag.bateman.jsonapi.encoding

import scala.util.Try

final case class Relationships(src: JObject, mappings: Map[JString, Relationship]) {
  private val bareMap: Map[String, Relationship] = mappings.map { case (k, v) => k.value -> v }
  def get(key: String): Option[Relationship] = bareMap.get(key)

  def toEncoding: Map[String, encoding.Relationship] = mappings map { case (k, v) => k.value -> v.toEncoding }

  override def toString: String = s"Relationships: ${Try(toEncoding.toJAny.spaces2).getOrElse(super.toString())}"
}

object Relationships {
  implicit val decoder: Decoder[JObject, Relationships] = Decoder { in =>
    (
      DecodeResult.pure(in),
      in.as[Map[JString, Relationship]]
    ).mapN(Relationships.apply)
  }
}
