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

import cats.data.NonEmptyChain
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.{JError, JErrorFormatters, JLocation, JObject, JPointer, JString}
import org.scalawag.bateman.jsonapi.encoding.Inclusions.Key

final case class JsonApiTypeMismatch(cur: JFocus[JString], expected: NonEmptyChain[String])
    extends JError
    with JErrorFormatters {
  override def pointer: JPointer = cur.pointer
  override def location: Option[JLocation] = cur.value.location
  override val description: String =
    s"A JSON:API resource object of type ${formatOrList(expected.iterator.map(x => s"'$x'"))} is expected here, " +
      s"but one of type '${cur.value.value}' was found instead."
}

object JsonApiTypeMismatch {
  def apply(cur: JFocus[JString], expected: String): JsonApiTypeMismatch =
    JsonApiTypeMismatch(cur, NonEmptyChain.one(expected))
}

final case class DuplicateResourceObjectDefinition(
    ref: Key,
    obj: JFocus[JObject],
    others: NonEmptyChain[JFocus[JObject]]
) extends JError
    with JErrorFormatters {
  override def location: Option[JLocation] = obj.value.location
  override def pointer: JPointer = obj.pointer
  override val description: String =
    (obj +: others).iterator
      .map(ro => s"${ro.pointer}${optionalLocation(ro.value.location)}")
      .mkString(
        s"""The resource object $ref is defined multiple times in this document.\n - """,
        "\n - ",
        "\n"
      )
}

final case class MissingIncludedResourceObject(cur: JFocus[JObject], key: Key) extends JError {
  override def location: Option[JLocation] = cur.value.location
  override def pointer: JPointer = cur.pointer
  override val description: String =
    s"""Missing included resource object definition $key"""
}
