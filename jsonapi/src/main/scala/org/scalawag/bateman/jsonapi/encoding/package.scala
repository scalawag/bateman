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

import cats.Id
import cats.data.{NonEmptyChain, ValidatedNec}
import org.scalawag.bateman.json.encoding.JAny
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.{Encoded, PartiallyEncoded}

package object encoding {
  type EncodeResult[+A] = ValidatedNec[EncodeError, A]

  object EncodeResult {
    def formatErrorReport(errors: NonEmptyChain[EncodeError]): String =
      errors.map(_.toJsonApiError.detail).iterator.mkString(" - ", "\n - ", "")
  }

  class AliasEncoderCompanion[Out <: ResourceLike] {
    def apply[In](implicit enc: ResourceEncoder[In, Out]): ResourceEncoder[In, Out] = enc

    def encodeResource[In](
        in: In,
        includeSpec: IncludeSpec = IncludeSpec.Never,
        fieldsSpec: FieldsSpec = FieldsSpec.All
    )(implicit
        enc: ResourceEncoder[In, Out]
    ): EncodeResult[PartiallyEncoded[Out]] = enc.encodeResource(in, includeSpec, fieldsSpec)

    def encodeInfallibly[In](
        in: In,
        includeSpec: InfallibleIncludeSpec = IncludeSpec.Opportunistically,
        fieldsSpec: InfallibleFieldsSpec = FieldsSpec.All
    )(implicit
        enc: ResourceEncoder[In, Out]
    ): Encoded[Out] = enc.encodeInfallibly(in, includeSpec, fieldsSpec)

    def encode[In](in: In)(implicit
        enc: ResourceEncoder[In, Out]
    ): Out = enc.encode(in)
  }

  type ResourceIdentifierEncoder[A] = ResourceEncoder[A, ResourceIdentifier]

  object ResourceIdentifierEncoder extends AliasEncoderCompanion[ResourceIdentifier]

  type ResourceObjectEncoder[A] = ResourceEncoder[A, ResourceObject]

  object ResourceObjectEncoder extends AliasEncoderCompanion[ResourceObject]

  type ResourceObjectOptionalIdEncoder[A] = ResourceEncoder[A, ResourceObjectOptionalId]

  object ResourceObjectOptionalIdEncoder extends AliasEncoderCompanion[ResourceObjectOptionalId]

  type Loader[F[_]] = ResourceIdentifier => F[Option[ResourceObject]]

  object Loader {
    def none: Loader[Id] = { _ => None }
  }
}
