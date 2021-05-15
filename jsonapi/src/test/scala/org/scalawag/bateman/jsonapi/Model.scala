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

import cats.data.NonEmptyChain
import cats.syntax.apply._
import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.json.decoding.{
  ContextualDecoder,
  Decoder,
  InvalidValue,
  JAny,
  JObject,
  JString,
  JsonTypeMismatch
}
import org.scalawag.bateman.json.encoding.Encoder
import org.scalawag.bateman.jsonapi.decoding.{
  CustomResourceIdentifier,
  CustomResourceObject,
  Document,
  JsonApiTypeMismatch,
  ResourceIdentifier,
  ResourceIdentifierLike,
  ResourceObject
}
import org.scalawag.bateman.jsonapi.query._

import java.util.UUID

object Model {

  sealed trait Entity

  final case class Drawing(
      id: UUID,
      title: String,
      primary: Shape.Id,
      subtitle: Option[String],
      shapes: List[Shape.Id],
      favorite: Option[Shape]
  ) extends Entity

  object Drawing {
    private val RESOURCE_TYPE = "Drawing"

    final case class Id(id: UUID)

    object Id {
      implicit val decoder: Decoder[ResourceIdentifier, Id] =
        CustomResourceIdentifier.generateDecoder(RESOURCE_TYPE) { from =>
          Decoder[JString, UUID].decode(from.id).map(Id(_))
        }
    }

    implicit val drawingDecoder: ContextualDecoder[ResourceObject, Drawing, Document] =
      CustomResourceObject.generateContextualDecoder(
        RESOURCE_TYPE
      ) { (in, document) =>
        (
          in.query(_ ~> id ~> as[UUID]),
          in.query(_ ~> attribute("title") ~> as[String]),
          in.query(_ ~> relationship("primary") ~> data ~> required ~> as[Shape.Id]),
          in.query(_ ~> attribute("sub_title") ~> as[Option[String]]),
          in.tquery(_ ~> relationship("sh_apes") ~> data ~> multiple ~> as[Shape.Id]),
          in.ctquery(document)(_ ~> relationship("favorite") ~> data ~> optional ~> included ~> as[Shape]),
        ).mapN(Drawing.apply)
      }
  }

  sealed trait Color

  object Color {
    implicit val colorDecoder: Decoder[JAny, Color] = Decoder { in =>
      in match {
        case s: JString if s.value == "red"   => Red.validNec
        case s: JString if s.value == "green" => Green.validNec
        case s: JString if s.value == "blue"  => Blue.validNec
        case s: JString                       => InvalidValue(s, "must be 'green', 'blue' or 'red'").invalidNec
        case o: JObject                       => Decoder[JObject, RGB].decode(o)
        case x                                => JsonTypeMismatch(x, NonEmptyChain(JObject, JString)).invalidNec
      }
    }

    import org.scalawag.bateman.json.encoding
    implicit val colorEncoder: Encoder[Color, org.scalawag.bateman.json.encoding.JAny] = {
      case RGB(r, g, b) =>
        encoding.JObject("r" -> encoding.JNumber(r), "g" -> encoding.JNumber(g), "b" -> encoding.JNumber(b))
      case c => encoding.JString(c.toString.toLowerCase)
    }
  }

  case object Red extends Color

  case object Green extends Color

  case object Blue extends Color

  final case class RGB(r: Int, g: Int, b: Int) extends Color

  object RGB {
    implicit val decoder: Decoder[JObject, RGB] = Decoder { in =>
      (
        in.query(_ ~> "r" ~> as[Int]),
        in.query(_ ~> "g" ~> as[Int]),
        in.query(_ ~> "b" ~> as[Int])
      ).mapN(RGB.apply)
    }
  }

  sealed trait Shape extends Entity {
    def label: Option[String]
    def color: List[Color]
  }

  object Shape {
    implicit val decoder: Decoder[ResourceObject, Shape] = Decoder { in =>
      in.`type`.value match {
        case "TRIANGLE" => in.query(_ ~> as[Triangle])
        case "Square"   => in.query(_ ~> as[Square])
        case "circle"   => in.query(_ ~> as[Circle])
        case x          => JsonApiTypeMismatch(in, NonEmptyChain("TRIANGLE", "Square", "circle")).invalidNec
      }
    }

    sealed trait Id

    object Id {
      implicit val decoder: Decoder[ResourceIdentifier, Id] = Decoder { in =>
        in.`type`.value match {
          case "TRIANGLE" => in.query(_ ~> as[Triangle.Id])
          case "Square"   => in.query(_ ~> as[Square.Id])
          case "circle"   => in.query(_ ~> as[Circle.Id])
          case x          => JsonApiTypeMismatch(in, NonEmptyChain("TRIANGLE", "Square", "circle")).invalidNec
        }
      }
    }
  }

  final case class Circle(
      id: UUID,
      label: Option[String],
      color: List[Color],
      radius: Int
  ) extends Shape

  object Circle {
    val RESOURCE_TYPE = "circle"

    implicit val decoder: Decoder[ResourceObject, Circle] =
      CustomResourceObject.generateDecoder(RESOURCE_TYPE) { in =>
        (
          in.query(_ ~> id ~> as[UUID]),
          in.query(_ ~> attribute("label") ~> as[Option[String]]),
          in.query(_ ~> attribute("color") ~> as[List[Color]]),
          in.query(_ ~> attribute("radius") ~> as[Int])
        ).mapN(Circle.apply)
      }

    final case class Id(id: UUID) extends Shape.Id

    object Id {
      implicit val decoder: Decoder[ResourceIdentifier, Id] =
        CustomResourceIdentifier.generateDecoder(RESOURCE_TYPE) { from =>
          Decoder[JString, UUID].decode(from.id).map(Id(_))
        }
    }
  }

  final case class Square(
      id: UUID,
      label: Option[String],
      color: List[Color],
      sideLength: Int
  ) extends Shape

  object Square {
    val RESOURCE_TYPE = "Square"

    final case class Id(id: UUID) extends Shape.Id

    implicit val decoder: Decoder[ResourceObject, Square] =
      CustomResourceObject.generateDecoder(RESOURCE_TYPE) { in =>
        (
          in.query(_ ~> id ~> as[UUID]),
          in.query(_ ~> attribute("label") ~> as[Option[String]]),
          in.query(_ ~> attribute("color") ~> as[List[Color]]),
          in.query(_ ~> attribute("side_length") ~> as[Int])
        ).mapN(Square.apply)
      }

    object Id {
      implicit val decoder: Decoder[ResourceIdentifier, Id] =
        CustomResourceIdentifier.generateDecoder(RESOURCE_TYPE) { from =>
          Decoder[JString, UUID].decode(from.id).map(Id(_))
        }
    }
  }

  final case class Triangle(
      id: UUID,
      label: Option[String],
      color: List[Color]
  ) extends Shape

  object Triangle {
    val RESOURCE_TYPE = "TRIANGLE"

    implicit val decoder: Decoder[ResourceObject, Triangle] =
      CustomResourceObject.generateDecoder(RESOURCE_TYPE) { in =>
        (
          in.query(_ ~> id ~> as[UUID]),
          in.query(_ ~> attribute("label") ~> as[Option[String]]),
          in.query(_ ~> attribute("color") ~> as[List[Color]])
        ).mapN(Triangle.apply)
      }

    final case class Id(id: UUID) extends Shape.Id

    object Id {
      implicit val decoder: Decoder[ResourceIdentifier, Id] =
        CustomResourceIdentifier.generateDecoder(RESOURCE_TYPE) { from =>
          Decoder[JString, UUID].decode(from.id).map(Id(_))
        }
    }

  }

  final case class Portfolio(
      id: UUID,
      full: Drawing,
      sparse: Option[Drawing.Id]
  ) extends Entity

  object Portfolio {
    implicit val decoder: ContextualDecoder[ResourceObject, Portfolio, Document] =
      CustomResourceObject.generateContextualDecoder("Portfolio") { (in, document) =>
        (
          in.query(_ ~> id ~> as[UUID]),
          in.cquery(document)(_ ~> relationship("full") ~> data ~> required ~> included ~> as[Drawing]),
          in.ctquery(document)(_ ~> relationship("sparse") ~> data ~> optional ~> as[Drawing.Id])
        ).mapN(Portfolio.apply)
      }
  }
}
