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

package org.scalawag.bateman.jsonapi

import cats.syntax.validated._
import org.scalawag.bateman.json.Nullable
import org.scalawag.bateman.json.decoding.{DecodeResult, JAny, JObject, JString}
import org.scalawag.bateman.json.decoding.query.{Query, TraverseQuery, root}
import org.scalawag.bateman.jsonapi.decoding.{
  Document,
  HasData,
  HasLinks,
  HasMeta,
  PrimaryData,
  Relationship,
  RelationshipData,
  ResourceIdentifier,
  ResourceIdentifierLike,
  ResourceLike,
  ResourceObject,
  ResourceObjectLike,
  ResourceObjectOptionalId
}

package object query {
  def meta[Context]: Query[HasMeta, JObject, Context] = Query { (from, context) => from.requiredMeta.map(_.src) }

  def links[Context]: Query[HasLinks, JObject, Context] = Query { (from, context) => from.requiredLinks.map(_.src) }

  trait DataPlaceholder

  object data extends DataPlaceholder {
    implicit def forPrimary(p: DataPlaceholder): Query[Document, PrimaryData, Any] =
      Query { (in, _) => in.requiredData }
    implicit def forRelationship(p: DataPlaceholder): Query[Relationship, RelationshipData, Any] =
      Query { (in, _) => in.requiredData }
  }

  trait RequiredPlaceholder

  object required extends RequiredPlaceholder {
    implicit def forPrimary(p: RequiredPlaceholder): Query[PrimaryData, ResourceLike, Any] =
      Query { (in, _) => in.required }
    implicit def forRelationship(p: RequiredPlaceholder): Query[RelationshipData, ResourceIdentifier, Any] =
      Query { (in, _) => in.required }
  }

  trait NullablePlaceholder

  object nullable extends NullablePlaceholder {
    implicit def forPrimary(p: NullablePlaceholder): TraverseQuery[Nullable, PrimaryData, ResourceLike, Any] =
      TraverseQuery { (in, _) => in.nullable }
    implicit def forRelationship(
        p: NullablePlaceholder
    ): TraverseQuery[Nullable, RelationshipData, ResourceIdentifier, Any] =
      TraverseQuery { (in, _) => in.nullable }
  }

  trait MultiplePlaceholder

  object multiple extends MultiplePlaceholder {
    implicit def forPrimary(p: MultiplePlaceholder): TraverseQuery[List, PrimaryData, ResourceLike, Any] =
      TraverseQuery { (in, _) => in.multiple }
    implicit def forRelationship(
        p: MultiplePlaceholder
    ): TraverseQuery[List, RelationshipData, ResourceIdentifier, Any] = TraverseQuery { (in, _) => in.multiple }
  }

  val id: Query[ResourceLike, JString, Any] =
    Query { (from, _) => from.requiredId }
  val resourceType: Query[ResourceLike, JString, Any] =
    Query { (from, _) => from.`type`.validNec }
  def attribute(name: String): Query[ResourceObjectLike, JAny, Any] =
    Query { (from, _) => from.requiredAttribute(name) }
  def relationship(name: String): Query[ResourceObjectLike, Relationship, Any] =
    Query { (from, _) => from.requiredRelationship(name) }
  def meta(name: String): Query[ResourceLike, JAny, Any] =
    Query { (from, _) => from.requiredMeta(name) }

  val included: Query[ResourceIdentifier, ResourceObject, Document] =
    Query { (from, document) =>
      document.requiredIncluded(from)
    }
}
