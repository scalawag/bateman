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

package org.scalawag.bateman.jsonapi.generic.encoding

import cats.data.Validated.{Invalid, Valid}
import org.scalawag.bateman.json.encoding.JAny
import org.scalawag.bateman.json.validIfEmpty
import org.scalawag.bateman.jsonapi.encoding
import org.scalawag.bateman.jsonapi.encoding._

/** When we generate an encoder for a ResourceLike, we need a place to store the fields the encoding _before_ we've
  * finished. We can't just use the target type because, it may have constraints that we can't fulfill. For example,
  * a ResourceObject _must_ have an `id`, so until we've encoded the ID, we can't instantiate one.
  *
  * Also, while we traverse the generic representation of our case class (the HList), we need to gather some
  * information about the fields we're expecting. This allows the encoder to handle situations where something
  * is not handled by one of the individual hcons encoders. An example for ResourceObjects is include paths. The HNil
  * encoder needs to know that there was _not_ a field with a particular name to be able to detect the error.
  */

final case class PartialResource(
    id: Option[String] = None,
    metas: Map[String, JAny] = Map.empty,
    attributes: Map[String, JAny] = Map.empty,
    relationships: Map[String, encoding.Relationship] = Map.empty,
    inclusions: Inclusions = Inclusions.empty,
    deferrals: Set[DeferredEncoding] = Set.empty,
    errors: Set[EncodeError] = Set.empty
) {
  def addAttribute(name: String, value: JAny): PartialResource =
    this.copy(attributes = this.attributes + (name -> value))
  def addMeta(name: String, value: JAny): PartialResource =
    this.copy(metas = this.metas + (name -> value))

  def addRelationship(name: String, relationship: Relationship): PartialResource =
    this.copy(relationships = this.relationships + (name -> relationship))
  def addRelationship(name: String, relationship: Option[Relationship]): PartialResource =
    relationship.map(addRelationship(name, _)).getOrElse(this)
  def addInclusion(resourceObject: ResourceObject): PartialResource =
    this.copy(inclusions = this.inclusions + resourceObject)
  def addInclusion(resourceObject: Option[ResourceObject]): PartialResource =
    resourceObject.map(addInclusion).getOrElse(this)
  def addInclusions(resourceObjects: List[ResourceObject]): PartialResource =
    this.copy(inclusions = this.inclusions ++ resourceObjects)
  def addInclusions(inclusions: Inclusions): PartialResource =
    this.copy(inclusions = this.inclusions ++ inclusions)
  def addDeferredEncoding(deferredEncoding: DeferredEncoding): PartialResource =
    addDeferredEncodings(Iterable(deferredEncoding))
  def addDeferredEncoding(deferredEncoding: Option[DeferredEncoding]): PartialResource =
    addDeferredEncodings(deferredEncoding.toList)
  def addDeferredEncodings(deferredEncodings: Iterable[DeferredEncoding]): PartialResource =
    this.copy(deferrals = this.deferrals ++ deferredEncodings)

  def whenValid[A](r: EncodeResult[A])(fn: A => PartialResource): PartialResource =
    r match {
      case Valid(a)    => fn(a)
      case Invalid(ee) => this.copy(errors = errors ++ ee.iterator)
    }

  def toEncodeResult: EncodeResult[PartialResource] = validIfEmpty(errors, this)
}
