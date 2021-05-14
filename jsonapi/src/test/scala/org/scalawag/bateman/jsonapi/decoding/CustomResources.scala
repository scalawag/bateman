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

package org.scalawag.bateman.jsonapi.decoding

import cats.syntax.apply._
import org.scalawag.bateman.json.decoding.{ContextualDecoder, DecodeResult, Decoder}

object CustomResourceObject {
  def generateDecoder[A <: ResourceObjectLike, B](
      resourceType: String
  )(fn: A => DecodeResult[B]): Decoder[A, B] =
    Decoder { in =>
      (
        in.validateType(resourceType),
        fn(in)
      ).tupled.map(_._2)
    }

  def generateContextualDecoder[A <: ResourceObjectLike, B, Context](
      resourceType: String
  )(fn: (A, Document) => DecodeResult[B]): ContextualDecoder[A, B, Document] = { (in, document) =>
    (
      in.validateType(resourceType),
      fn(in, document)
    ).tupled.map(_._2)
  }
}

object CustomResourceIdentifier {
  def generateDecoder[A](
      resourceType: String
  )(fn: ResourceIdentifier => DecodeResult[A]): Decoder[ResourceIdentifier, A] = { (in, context) =>
    (in.validateType(resourceType), fn(in)).tupled.map(_._2)
  }
}
