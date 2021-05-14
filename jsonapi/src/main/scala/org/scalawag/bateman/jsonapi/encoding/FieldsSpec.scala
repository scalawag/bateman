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

package org.scalawag.bateman.jsonapi.encoding

sealed trait FieldsSpec {
  def explicitFields(resourceType: String): Set[String]
  def includeField(resourceType: String, field: String): Boolean
}

/** Using this to encode can never cause an EncodeError. */
sealed trait InfallibleFieldsSpec extends FieldsSpec {
  override def explicitFields(resourceType: String): Set[String] = Set.empty
}

/** Used to track sparse fieldsets requested by the caller. See https://jsonapi.org/format/#fetching-sparse-fieldsets
  *
  * Note that by specifying field names, you make it possible for encoding to fail due to an unrecognized field name.
  * Contrast this with the [[InfallibleFieldsSpec]]s.
  *
  * @param fields
  * @param default should a field be included if its resourceType is not specified in the map?
  */
case class FallibleFieldsSpec(fields: Map[String, Set[String]], default: Boolean = true) extends FieldsSpec {

  override def explicitFields(resourceType: String): Set[String] = fields.getOrElse(resourceType, Set.empty)

  def includeField(resourceType: String, field: String): Boolean =
    fields.get(resourceType) match {
      case None                  => default
      case Some(resourceTypeMap) => resourceTypeMap.contains(field)
    }
}

object FieldsSpec {
  def apply(fields: Map[String, Set[String]], default: Boolean = true): FieldsSpec =
    FallibleFieldsSpec(fields, default)

  case object All extends InfallibleFieldsSpec {
    override def includeField(resourceType: String, field: String): Boolean = true
  }

  case object None extends InfallibleFieldsSpec {
    override def includeField(resourceType: String, field: String): Boolean = false
  }
}
