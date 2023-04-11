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

import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields

/** Used to request sparse fieldsets from the encoder. See https://jsonapi.org/format/#fetching-sparse-fieldsets.
  * Given a resource type and a field name, this can tell you whether or not that field should be included in the
  * encoded JSON:API document.
  */
trait FieldsSpec {
  type FieldsType <: Fields
  def forResourceType(resourceType: String): FieldsType
}

object FieldsSpec {

  /**
    * @param fields the set of field names to include when encoding the document, per resource type
    * @param default the fields to use for any resource type that's not in the specified map
    */
  def apply(fields: Map[String, Fields], default: Fields = Fields.All): FieldsSpec =
    new FieldsSpec {
      override type FieldsType = Fields
      override def forResourceType(resourceType: String): Fields = fields.getOrElse(resourceType, default)
    }

  sealed trait Fields {
    def apply(fieldName: String): Boolean
  }

  object Fields {

    /** An explicit set of field names to include when encoding the resource.
      *
      * Note that by specifying field names, you make it possible for encoding to fail due to an unrecognized
      * field name. Contrast this with the [[Infallible]]s.
      *
      * @param fields
      */
    case class Explicit(fields: Set[String]) extends Fields {
      override def apply(fieldName: String): Boolean = fields(fieldName)
    }

    object Explicit {
      def apply(fields: String*): Explicit = Explicit(fields.toSet)
    }

    sealed trait Infallible extends Fields

    case object All extends Infallible {
      override def apply(fieldName: String): Boolean = true
    }

    case object None extends Infallible {
      override def apply(fieldName: String): Boolean = false
    }
  }

  /** "Infallible", in this context, means that this FieldsSpec can never cause an error because it doesn't have any
    * explicit references to any resource fields (which could be invalid/unknown).
    */

  trait Infallible extends FieldsSpec {
    override type FieldsType = Fields.Infallible
  }

  object Infallible {
    def apply(default: Fields.Infallible = Fields.All, fields: Map[String, Fields.Infallible] = Map.empty): Infallible =
      fields.getOrElse(_, default)
  }

  case object All extends Infallible {
    override def forResourceType(resourceType: String): Fields.Infallible = Fields.All
  }

  case object None extends Infallible {
    override def forResourceType(resourceType: String): Fields.Infallible = Fields.None
  }
}
