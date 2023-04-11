//// bateman -- Copyright 2021 -- Justin Patterson
////
//// Licensed under the Apache License, Version 2.0 (the "License");
//// you may not use this file except in compliance with the License.
//// You may obtain a copy of the License at
////
//// http://www.apache.org/licenses/LICENSE-2.0
////
//// Unless required by applicable law or agreed to in writing, software
//// distributed under the License is distributed on an "AS IS" BASIS,
//// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//// See the License for the specific language governing permissions and
//// limitations under the License.
//
//package org.scalawag.bateman.jsonapi.generic
//
//import org.scalawag.bateman.json.generic.{Config, SourceTag}
//import org.scalawag.bateman.jsonapi.Model.Color
//import org.scalawag.bateman.jsonapi.decoding.{ResourceIdentifier, ResourceObject}
//import org.scalawag.bateman.jsonapi.generic.decoding.{JObjectDecoder, TraitResourceDecoder}
//import shapeless.tag.@@
//
//import java.util.UUID
//
//object TestModel {
//
//  sealed trait Entity
//
//  final case class Drawing(
//      @Id id: UUID,
//      @Attribute title: String,
//      @Relationship primary: Shape.Id,
//      @Attribute subtitle: Option[String],
//      @Relationship shapes: List[Shape.Id],
//      @Relationship favorite: Option[Shape] = None
//  ) extends Entity
//
//  object Drawing {
//    final case class Id(@Id id: UUID)
//  }
//
//  sealed trait Shape extends Entity {
//    def @Attribute label: Option[String]
//
//    def @Attribute color: List[Color]
//  }
//
//  object Shape {
//    sealed trait Id
//  }
//
//  final case class Circle(
//      @Id id: UUID,
//      @Attribute label: Option[String],
//      @Attribute color: List[Color],
//      @Attribute radius: Int
//  ) extends Shape
//
//  object Circle {
//    final case class Id(@Id id: UUID) extends Shape.Id
//  }
//
//  final case class Square(
//      @Id id: UUID,
//      @Attribute label: Option[String],
//      @Attribute color: List[Color],
//      @Attribute sideLength: Int,
//      @Source json: Option[JSource] = None
//  ) extends Shape
//
//  object Square {
//    final case class Id(@Id id: UUID) extends Shape.Id
//  }
//
//  final case class Triangle(
//      @Id id: UUID,
//      @Attribute label: Option[String],
//      @Attribute color: List[Color]
//  ) extends Shape
//
//  object Triangle {
//    final case class Id(@Id id: UUID) extends Shape.Id
//  }
//
//  final case class Portfolio(
//      @Id id: UUID,
//      @Relationship full: Drawing,
//      @Relationship sparse: Option[Drawing.Id]
//  ) extends Entity
//
//  def makeDecoders(implicit config: Config) =
//    new Object {
//      implicit val squareIdDecoder: JObjectDecoder[ResourceIdentifier, Square.Id] =
//        semiauto.deriveResourceIdentifierDecoderForCaseClass[Square.Id]("square")
//      implicit val triangleIdDecoder: JObjectDecoder[ResourceIdentifier, Triangle.Id] =
//        semiauto.deriveResourceIdentifierDecoderForCaseClass[Triangle.Id]("triangle")
//      implicit val circleIdDecoder: JObjectDecoder[ResourceIdentifier, Circle.Id] =
//        semiauto.deriveResourceIdentifierDecoderForCaseClass[Circle.Id]("circle")
//      implicit val shapeIdDecoder: TraitResourceDecoder[ResourceIdentifier, Shape.Id] =
//        semiauto.deriveResourceIdentifierDecoderForTrait[Shape.Id]()
//
//      implicit val squareDecoder: JObjectDecoder[ResourceObject, Square] =
//        semiauto.deriveResourceObjectDecoderForCaseClass[Square]("square")
//      implicit val triangleDecoder: JObjectDecoder[ResourceObject, Triangle] =
//        semiauto.deriveResourceObjectDecoderForCaseClass[Triangle]("triangle")
//      implicit val circleDecoder: JObjectDecoder[ResourceObject, Circle] =
//        semiauto.deriveResourceObjectDecoderForCaseClass[Circle]("circle")
//      implicit val shapeDecoder: TraitResourceDecoder[ResourceObject, Shape] =
//        semiauto.deriveResourceObjectDecoderForTrait[Shape]()
//
//      implicit val drawingDecoder: JObjectDecoder[ResourceObject, Drawing] =
//        semiauto.deriveResourceObjectDecoderForCaseClass[Drawing]("drawing")
//    }
//}
