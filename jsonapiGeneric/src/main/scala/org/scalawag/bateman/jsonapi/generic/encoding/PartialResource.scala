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

package org.scalawag.bateman.jsonapi.generic.encoding

import cats.syntax.semigroup._
import org.scalawag.bateman.json.{JAny, JObject, rightIfEmpty}
import org.scalawag.bateman.jsonapi._
import org.scalawag.bateman.jsonapi.encoding.{EncodeError, EncodeResult, Inclusions}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.Encoded

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
    resourceType: String,
    id: Option[String] = None,
    lid: Option[String] = None,
    metas: List[(String, JAny)] = Nil,
    attributes: List[(String, JAny)] = Nil,
    relationships: List[(String, JAny)] = Nil,
    inclusions: Inclusions = Inclusions.empty,
    errors: Set[EncodeError] = Set.empty
) {
  def addAttribute(name: String, value: JAny): PartialResource =
    this.copy(attributes = (name -> value) :: this.attributes)
  def addMeta(name: String, value: JAny): PartialResource =
    this.copy(metas = (name -> value) :: this.metas)

  def addRelationship(name: String, value: JAny): PartialResource =
    this.copy(relationships = (name -> value) :: this.relationships)
  def addRelationship(name: String, relationship: Option[JAny]): PartialResource =
    relationship.map(addRelationship(name, _)).getOrElse(this)
  def addInclusions(inclusions: Iterable[JObject]): PartialResource = {
    this.copy(inclusions = inclusions.foldLeft(this.inclusions)(_ + _))
  }
  def addInclusions(inclusions: Inclusions): PartialResource =
    this.copy(inclusions = this.inclusions combine inclusions)

  def addError(error: EncodeError): PartialResource =
    this.copy(errors = this.errors + error)

  def whenValid[A](r: EncodeResult[A])(fn: A => PartialResource): PartialResource =
    r match {
      case Right(a) => fn(a)
      case Left(ee) => this.copy(errors = errors ++ ee.iterator)
    }

  def toRootObject: JObject =
    JObject.flatten(
      Some("type" -> resourceType.toJAny),
      if (id.isEmpty) None else Some("id" -> id.get.toJAny),
      if (lid.isEmpty) None else Some("lid" -> lid.get.toJAny),
      if (attributes.isEmpty) None else Some("attributes" -> JObject(attributes: _*)),
      if (relationships.isEmpty) None else Some("relationships" -> JObject(relationships: _*)),
      if (metas.isEmpty) None else Some("meta" -> JObject(metas: _*)),
    )

  def toEncoded: EncodeResult[Encoded] = rightIfEmpty(errors, Encoded(toRootObject, inclusions))
}
