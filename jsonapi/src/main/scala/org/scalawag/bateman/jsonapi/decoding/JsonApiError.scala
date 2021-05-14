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

import cats.data.NonEmptyChain
import org.scalawag.bateman.json.decoding.{DecodeError, ErrorFormatters, JLocation, JPointer}

final case class JsonApiTypeMismatch(resource: ResourceLike, expected: NonEmptyChain[String])
    extends DecodeError
    with ErrorFormatters {
  override def pointer: JPointer = resource.src.root.pointer
  override def location: JLocation = resource.src.root.location
  override val description: String =
    s"A JSON:API resource object of type ${formatOrList(expected.iterator.map(x => s"'$x'"))} is expected here, " +
      s"but one of type '${resource.`type`.value}' was found instead."
}

object JsonApiTypeMismatch {
  def apply(resource: ResourceLike, expected: String): JsonApiTypeMismatch =
    JsonApiTypeMismatch(resource, NonEmptyChain.one(expected))
}

final case class DuplicateResourceObjectDefinition(
    obj: ResourceObject,
    others: NonEmptyChain[ResourceObject]
) extends DecodeError
    with ErrorFormatters {
  override def location: JLocation = obj.src.root.location
  override def pointer: JPointer = obj.src.root.pointer
  override val description: String =
    (obj +: others).iterator
      .map(ro => s"${ro.src.root.pointer} (${ro.src.root.location}")
      .mkString(
        s"""The resource object (type="${obj.`type`.value}", id="${obj.id}") is defined multiple times in this document.\n - """,
        "\n - ",
        "\n"
      )
}

final case class MissingIncludedResourceObject(identifier: ResourceIdentifier) extends DecodeError {
  override def location: JLocation = identifier.src.root.location
  override def pointer: JPointer = identifier.src.root.pointer
  override val description: String =
    s"""Missing included resource object definition (type="${identifier.`type`.value}", id="${identifier.id.value}")"""
}
