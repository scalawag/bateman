// bateman -- Copyright 2021-2026 -- Justin Patterson
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

package org.scalawag.bateman.json.generic.encoding

import org.scalawag.bateman.json.{JAny, JObject, ProgrammerError}

object DiscriminatorMerge {

  /** Merges accumulated discriminator fields into the base-encoded object.
    *
    * Discriminator fields appear first (preserving their ordering), followed by base-only fields.
    * When both objects contain a field with the same name:
    *   - If both values are JObjects, they are merged recursively (for nested discriminators like `meta.status`).
    *   - If both values are equal (ignoring location), the collision is benign and the discriminator value is kept.
    *   - If both values differ, a ProgrammerError is thrown (the case class encoded a data field that conflicts
    *     with the discriminator).
    */
  def mergeDiscriminators(discriminators: JObject, base: JObject): JObject = {
    if (discriminators.fieldList.isEmpty) return base

    val baseFieldMap = base.fieldList.map(f => f.name.value -> f.value).toMap
    val discFieldNames = discriminators.fieldList.map(_.name.value).toSet

    // Discriminator fields first, merging where both sides have the same field name.
    val mergedDiscFields: List[(String, JAny)] = discriminators.fieldList.map { df =>
      val name = df.name.value
      baseFieldMap.get(name) match {
        case Some(baseValue) =>
          (df.value, baseValue) match {
            case (dObj: JObject, bObj: JObject) =>
              name -> mergeDiscriminators(dObj, bObj)
            case (d, b) if d.stripLocation == b.stripLocation =>
              name -> d
            case (d, b) =>
              throw ProgrammerError(
                s"""|discriminator conflict...
                    |The concrete encoder has already encoded something in the location of the discriminator.
                    |This is not necessarily a problem except that it has a different value than what the
                    |abstract encoder wants to set it to.
                    |  field: $name
                    |  concrete value: ${b.render}
                    |  abstract value: ${d.render}""".stripMargin
              )
          }
        case None =>
          name -> df.value
      }
    }

    // Base-only fields follow in their original order.
    val baseOnlyFields: List[(String, JAny)] =
      base.fieldList.filterNot(f => discFieldNames(f.name.value)).map(f => f.name.value -> f.value)

    JObject((mergedDiscFields ++ baseOnlyFields): _*)
  }
}
