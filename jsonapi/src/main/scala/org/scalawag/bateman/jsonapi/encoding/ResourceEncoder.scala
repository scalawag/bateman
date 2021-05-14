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

import cats.syntax.validated._
import org.scalawag.bateman.json.ProgrammerError
import org.scalawag.bateman.json.encoding.Encoder
import org.scalawag.bateman.jsonapi.ResourceCodec
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.{Encoded, PartiallyEncoded}

trait ResourceEncoder[In, +Out <: ResourceLike] extends Encoder[In, Out] {
  def encodeResource(
      in: In,
      includeSpec: IncludeSpec = IncludeSpec.Opportunistically,
      fieldsSpec: FieldsSpec = FieldsSpec.All
  ): EncodeResult[PartiallyEncoded[Out]]

  def encodeInfallibly(
      in: In,
      includeSpec: InfallibleIncludeSpec = IncludeSpec.Opportunistically,
      fieldsSpec: InfallibleFieldsSpec = FieldsSpec.All
  ): Encoded[Out] = {
    val penc = encodeResource(in, includeSpec: IncludeSpec, fieldsSpec: FieldsSpec).getOrElse(
      throw ProgrammerError("An encoding operation with infallible parameters shouldn't fail!")
    )
    if (penc.deferrals.nonEmpty)
      throw ProgrammerError(
        "An encoding operation with an InfallibleIncludeSpec shouldn't return a PartiallyEncoded with deferrals!"
      )
    else
      Encoded(penc.root, penc.inclusions)
  }

  override def encode(in: In): Out =
    encodeInfallibly(in, IncludeSpec.Never, FieldsSpec.All).root
}

object ResourceEncoder {
  def apply[In, Out <: ResourceLike](implicit enc: ResourceEncoder[In, Out]): ResourceEncoder[In, Out] = enc

  trait EncodedLike[+A <: ResourceLike] {
    val root: A
    val inclusions: Inclusions
  }

  case class Encoded[+A <: ResourceLike](root: A, inclusions: Inclusions = Inclusions.empty) extends EncodedLike[A]

  case class PartiallyEncoded[+A <: ResourceLike](
      root: A,
      inclusions: Inclusions = Inclusions.empty,
      deferrals: Set[DeferredEncoding] = Set.empty
  ) extends EncodedLike[A]

  implicit val identityIdentifierEncoder: ResourceEncoder[ResourceIdentifier, ResourceIdentifier] = { (in, _, _) =>
    PartiallyEncoded(in).validNec
  }

  // How it knows to look for a ResourceCodec when it needs a ResourceEncoder

  implicit def fromResourceCodec[In, Out <: ResourceLike](implicit
      codec: ResourceCodec[Nothing, In, Out]
  ): ResourceEncoder[In, Out] = codec

  // This is used to decode relatives in derived decoders when we don't know if the caller is providing a decoder
  // from ResourceIdentifier or from ResourceObject (after looking it up in included). When we actually do the
  // decoding, we have to know which is is to give it the appropriate input, but this allows us to know that at
  // least one of those decoders exists for compile-time checking.

  implicit def toIdentifierEncoder[A](implicit
      dec: ResourceEncoder[A, ResourceIdentifier]
  ): ResourceEncoder[A, ResourceIdentifierLike] = { (_, _, _) =>
    throw new IllegalArgumentException(
      "This encoder (toIdentifierEncoder) shouldn't actually be used to encode. It's only for static type checking to ensure there's either a ResourceIdentifierEncoder or a ResourceObjectEncoder"
    )
  }

  implicit def toObjectEncoder[A](implicit
      dec: ResourceEncoder[A, ResourceObject]
  ): ResourceEncoder[A, ResourceIdentifierLike] = { (_, _, _) =>
    throw new IllegalArgumentException(
      "This encoder (toObjectEncoder) shouldn't actually be used to encode. It's only for static type checking to ensure there's either a ResourceIdentifierEncoder or a ResourceObjectEncoder"
    )
  }

  // TODO: is this how we should do this? It seems weird.
  implicit val transcoder: ResourceEncoder[ResourceIdentifier, ResourceIdentifierLike] = { (in, _, _) =>
    PartiallyEncoded(in, Inclusions.empty, Set.empty).validNec
  }
}
