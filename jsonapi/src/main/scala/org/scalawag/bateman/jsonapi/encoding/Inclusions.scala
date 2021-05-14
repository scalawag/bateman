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

import cats.Monoid
import cats.syntax.monoid._
import org.scalawag.bateman.json.ProgrammerError

case class Inclusions(private val objectMap: Map[String, Map[String, List[ResourceObject]]]) {
  def +(ro: ResourceObject) = this combine Inclusions(Map(ro.`type` -> Map(ro.id -> List(ro))))
  def ++(ro: List[ResourceObject]) = this combine Inclusions(ro: _*)
  def ++(that: Inclusions) = this combine that

  def contains(id: ResourceIdentifier): Boolean =
    objectMap.getOrElse(id.`type`, Map.empty).get(id.id).exists(_.nonEmpty)

  /** Unsafe. If you have programming errors (the same id but two different states), this will throw. */
  def objects: Iterable[ResourceObject] =
    for {
      byType <- objectMap.values
      byId <- byType.values
      byIdDistinct = byId.distinct
      if byIdDistinct.nonEmpty
    } yield {
      if (byIdDistinct.length > 1)
        throw ProgrammerError("inconsistent duplicate resource objects: " + byIdDistinct)
      else
        byIdDistinct.head
    }
}

object Inclusions {
  val empty: Inclusions = new Inclusions(Map.empty)

  def apply(resourceObjects: ResourceObject*): Inclusions = resourceObjects.foldLeft(Inclusions.empty)(_ + _)

  implicit val monoid: Monoid[Inclusions] = new Monoid[Inclusions] {
    override def empty: Inclusions = Inclusions.empty
    override def combine(x: Inclusions, y: Inclusions): Inclusions = Inclusions(x.objectMap combine y.objectMap)
  }
}
