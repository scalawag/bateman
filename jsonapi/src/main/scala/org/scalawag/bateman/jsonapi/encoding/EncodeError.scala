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

package org.scalawag.bateman.jsonapi.encoding

import org.scalawag.bateman.json.syntax._

/** Indicates an problem with the encoding parameters. This normally indicates user error, assuming that the values
  * came from the HTTP query parameters and are passed directly to the encoder. They may indicate a programmer error,
  * if the values were hard-coded or were otherwise derived or validated.
  */
trait EncodeError {
  def toJsonApiError: Error
}

case class InvalidIncludePath(path: String) extends EncodeError {
  override def toJsonApiError: Error =
    Error(
      status = Some("400"),
      code = Some("invalid_include_path"),
      detail = Some(s"The include path '$path' is invalid."),
      source = Some(ErrorSource.Parameter("include")),
      meta = Some(Seq("path" -> path.toJAny))
    )
}

case class IncludeTooLong(limit: Int) extends EncodeError {
  override def toJsonApiError: Error =
    Error(
      status = Some("400"),
      code = Some("include_too_long"),
      detail = Some(s"The include parameter is longer than the limit of $limit characters."),
      source = Some(ErrorSource.Parameter("include")),
      meta = Some(Seq("limit" -> limit.toJAny))
    )
}

case class IncludePathTooDeep(path: String, limit: Int) extends EncodeError {
  override def toJsonApiError: Error =
    Error(
      status = Some("400"),
      code = Some("include_path_too_deep"),
      detail = Some(s"The include path '$path' is deeper than the limit of $limit relationships."),
      source = Some(ErrorSource.Parameter("include")),
      meta = Some(Seq("path" -> path.toJAny, "limit" -> limit.toJAny))
    )
}

case class UnavailableIncludePath(path: String) extends EncodeError {
  override def toJsonApiError: Error =
    Error(
      status = Some("400"),
      code = Some("unavailable_include_path"),
      detail = Some(s"The include path '$path' is unavailable through this endpoint."),
      source = Some(ErrorSource.Parameter("include")),
      meta = Some(Seq("path" -> path.toJAny))
    )
}

case class InvalidFieldName(resourceType: String, field: String) extends EncodeError {
  override def toJsonApiError: Error =
    Error(
      status = Some("400"),
      code = Some("invalid_field_specified"),
      detail = Some(s"The resource type '$resourceType' does not have a field '$field'."),
      source = Some(ErrorSource.Parameter("fields")),
      meta = Some(Seq("resource_type" -> resourceType.toJAny, "field" -> field.toJAny))
    )
}
