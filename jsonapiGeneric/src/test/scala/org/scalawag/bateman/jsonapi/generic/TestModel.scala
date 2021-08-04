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

package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.generic.{Config, SourceTag}
import org.scalawag.bateman.jsonapi.Model.Color
import org.scalawag.bateman.jsonapi.decoding.{ResourceIdentifier, ResourceObject}
import org.scalawag.bateman.jsonapi.generic.decoding.{CaseClassResourceDecoder, JSource, TraitResourceDecoder}
import shapeless.tag.@@

import java.util.UUID

object TestModel {

  sealed trait Entity

  final case class Drawing(
      id: UUID @@ IdTag,
      title: String @@ AttributeTag,
      primary: Shape.Id @@ RelationshipTag,
      subtitle: Option[String] @@ AttributeTag,
      shapes: List[Shape.Id] @@ RelationshipTag,
      favorite: Option[Shape] @@ RelationshipTag = None
  ) extends Entity

  object Drawing {
    final case class Id(id: UUID @@ IdTag)
  }

  sealed trait Shape extends Entity {
    def label: Option[String] @@ AttributeTag

    def color: List[Color] @@ AttributeTag
  }

  object Shape {
    sealed trait Id
  }

  final case class Circle(
      id: UUID @@ IdTag,
      label: Option[String] @@ AttributeTag,
      color: List[Color] @@ AttributeTag,
      radius: Int @@ AttributeTag
  ) extends Shape

  object Circle {
    final case class Id(id: UUID @@ IdTag) extends Shape.Id
  }

  final case class Square(
      id: UUID @@ IdTag,
      label: Option[String] @@ AttributeTag,
      color: List[Color] @@ AttributeTag,
      sideLength: Int @@ AttributeTag,
      json: Option[JSource] @@ SourceTag = None
  ) extends Shape

  object Square {
    final case class Id(id: UUID @@ IdTag) extends Shape.Id
  }

  final case class Triangle(
      id: UUID @@ IdTag,
      label: Option[String] @@ AttributeTag,
      color: List[Color] @@ AttributeTag
  ) extends Shape

  object Triangle {
    final case class Id(id: UUID @@ IdTag) extends Shape.Id
  }

  final case class Portfolio(
      id: UUID @@ IdTag,
      full: Drawing @@ RelationshipTag,
      sparse: Option[Drawing.Id] @@ RelationshipTag
  ) extends Entity

  def makeDecoders(implicit config: Config) =
    new Object {
      implicit val squareIdDecoder: CaseClassResourceDecoder[ResourceIdentifier, Square.Id] =
        semiauto.deriveResourceIdentifierDecoderForCaseClass[Square.Id]("square")
      implicit val triangleIdDecoder: CaseClassResourceDecoder[ResourceIdentifier, Triangle.Id] =
        semiauto.deriveResourceIdentifierDecoderForCaseClass[Triangle.Id]("triangle")
      implicit val circleIdDecoder: CaseClassResourceDecoder[ResourceIdentifier, Circle.Id] =
        semiauto.deriveResourceIdentifierDecoderForCaseClass[Circle.Id]("circle")
      implicit val shapeIdDecoder: TraitResourceDecoder[ResourceIdentifier, Shape.Id] =
        semiauto.deriveResourceIdentifierDecoderForTrait[Shape.Id]()

      implicit val squareDecoder: CaseClassResourceDecoder[ResourceObject, Square] =
        semiauto.deriveResourceObjectDecoderForCaseClass[Square]("square")
      implicit val triangleDecoder: CaseClassResourceDecoder[ResourceObject, Triangle] =
        semiauto.deriveResourceObjectDecoderForCaseClass[Triangle]("triangle")
      implicit val circleDecoder: CaseClassResourceDecoder[ResourceObject, Circle] =
        semiauto.deriveResourceObjectDecoderForCaseClass[Circle]("circle")
      implicit val shapeDecoder: TraitResourceDecoder[ResourceObject, Shape] =
        semiauto.deriveResourceObjectDecoderForTrait[Shape]()

      implicit val drawingDecoder: CaseClassResourceDecoder[ResourceObject, Drawing] =
        semiauto.deriveResourceObjectDecoderForCaseClass[Drawing]("drawing")
    }
}
