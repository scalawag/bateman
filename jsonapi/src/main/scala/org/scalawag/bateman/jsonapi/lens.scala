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

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.lens.{focus, _}
import cats.syntax.either._
import cats.data.NonEmptyChain
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.jsonapi.encoding.Inclusions
import org.scalawag.bateman.jsonapi.encoding.Inclusions.Key

package object lens {
  val about: CreatableJLens[JAny, JString] = focus ~> "about" ~> narrow[JString]
  val attributes: CreatableJLens[JAny, JObject] = focus ~> "attributes" ~> narrow[JObject]
  val code: CreatableJLens[JAny, JString] = focus ~> "code" ~> narrow[JString]
  val data: CreatableJLens[JAny, JAny] = "data"
  val describedby: CreatableJLens[JAny, JString] = focus ~> "describedby" ~> narrow[JString]
  val detail: CreatableJLens[JAny, JString] = focus ~> "detail" ~> narrow[JString]
  val errors: CreatableJLens[JAny, JArray] = focus ~> "errors" ~> narrow[JArray]
  val ext: CreatableJLens[JAny, JArray] = focus ~> "ext" ~> narrow[JArray]
  val header: CreatableJLens[JAny, JString] = focus ~> "header" ~> narrow[JString]
  val href: CreatableJLens[JAny, JString] = focus ~> "href" ~> narrow[JString]
  val hreflang: CreatableJLens[JAny, JAny] = focus ~> "hreflang"
  val id: CreatableJLens[JAny, JString] = focus ~> "id" ~> narrow[JString]
  val included: CreatableJLens[JAny, JArray] = focus ~> "included" ~> narrow[JArray]
  val jsonapi: CreatableJLens[JAny, JObject] = focus ~> "jsonapi" ~> narrow[JObject]
  val lid: CreatableJLens[JAny, JString] = focus ~> "lid" ~> narrow[JString]
  val links: CreatableJLens[JAny, JObject] = focus ~> "links" ~> narrow[JObject]
  val linkType: CreatableJLens[JAny, JString] = focus ~> "type" ~> narrow[JString]
  val meta: CreatableJLens[JAny, JObject] = focus ~> "meta" ~> narrow[JObject]
  val parameter: CreatableJLens[JAny, JString] = focus ~> "parameter" ~> narrow[JString]
  val pointer: CreatableJLens[JAny, JString] = focus ~> "pointer" ~> narrow[JString]
  val profile: CreatableJLens[JAny, JArray] = focus ~> "profile" ~> narrow[JArray]
  val rel: CreatableJLens[JAny, JString] = focus ~> "rel" ~> narrow[JString]
  val relationships: CreatableJLens[JAny, JObject] = focus ~> "relationships" ~> narrow[JObject]
  val resourceType: CreatableJLens[JAny, JString] = focus ~> "type" ~> narrow[JString]
  val source: CreatableJLens[JAny, JObject] = focus ~> "source" ~> narrow[JObject]
  val status: CreatableJLens[JAny, JString] = focus ~> "status" ~> narrow[JString]
  val title: CreatableJLens[JAny, JString] = focus ~> "title" ~> narrow[JString]
  val version: CreatableJLens[JAny, JString] = focus ~> "version" ~> narrow[JString]

  def meta(name: String): CreatableJLens[JAny, JAny] = meta ~> name
  def attribute(name: String): CreatableJLens[JAny, JAny] = attributes ~> name
  def relationship(name: String): CreatableJLens[JAny, JObject] = relationships ~> name ~> narrow[JObject]

  val nullableIncludedRef: IdJLens[JAny, JAny] = {

    case in @ JFocus.Value(_: JNull) =>
      in.rightNec

    case in @ JFocus.Value(_: JObject) =>
      in.asObject.flatMap { inObject =>
        // Decode the focused value as an IncludeKey.
        inObject.decode[Key].flatMap { targetKey =>
          // Find all matching resource objects that match this resource identifier.
          val matchingResourceObjects = Inclusions.findResourceObjects(in.root).map(_.getOrElse(targetKey, Nil))

          matchingResourceObjects.flatMap {
            // Found one match. Decode it with the resource object decoder that was passed in.
            case List(fo) =>
              fo.rightNec
            // Everything else indicates an error.
            case Nil =>
              MissingIncludedResourceObject(inObject, targetKey).leftNec
            case fo :: fos =>
              DuplicateResourceObjectDefinition(targetKey, fo, NonEmptyChain.fromSeq(fos).get).leftNec
          }
        }
      }

    case in =>
      JsonTypeMismatch(in, JNull, JObject).leftNec
  }

  // Defer to the nullable lens as long as the focused value is an object.
  val includedRef: IdJLens[JAny, JObject] = _.asObject.flatMap(nullableIncludedRef).flatMap(_.asObject)
}
