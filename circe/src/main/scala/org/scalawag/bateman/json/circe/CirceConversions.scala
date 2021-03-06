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

package org.scalawag.bateman.json.circe

import io.circe.{Json, JsonNumber}
import org.scalawag.bateman.json.encoding._

object CirceConversions {
  def toCirce(in: JAny): Json =
    in match {
      case JNull            => Json.Null
      case JString(s)       => Json.fromString(s)
      case JNumber(n)       => Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(n)) // TODO: check this somehow
      case JBoolean(b)      => Json.fromBoolean(b)
      case JArray(arr @ _*) => Json.fromValues(arr.map(toCirce))
      case JObject(ff @ _*) => Json.fromFields(ff.map(f => f._1 -> toCirce(f._2)))
    }

  def fromCirce(in: Json): JAny =
    if (in.isNull)
      JNull
    else if (in.isString)
      JString(in.asString.get)
    else if (in.isBoolean)
      JBoolean(in.asBoolean.get)
    else if (in.isNumber)
      JNumber.unsafe(in.asNumber.get.toString)
    else if (in.isArray)
      JArray(in.asArray.get.map(fromCirce): _*)
    else
      JObject(in.asObject.get.toList.map { case (k, v) => k -> fromCirce(v) }: _*)
}
