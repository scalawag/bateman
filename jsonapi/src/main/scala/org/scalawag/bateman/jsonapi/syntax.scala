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

import org.scalawag.bateman.json.{JObject, noneIfEmpty}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.encoding.{
  EncodeResult,
  FieldsSpec,
  IncludeSpec,
  InfallibleIncludeSpec,
  ResourceEncoder
}

object syntax {
  implicit class AnyBatemanJsonApiOps[A](a: A) {
    def toDocument(implicit enc: ResourceEncoder[A]): JObject = toDocument()

    def toDocument(
        includeSpec: InfallibleIncludeSpec = IncludeSpec.Opportunistically,
        fieldsSpec: FieldsSpec.Infallible = FieldsSpec.All
    )(implicit enc: ResourceEncoder[A]): JObject = {
      val encoded = enc.encodeInfallibly(a, includeSpec, fieldsSpec)
      JObject.flatten(
        Some("data" -> encoded.root),
        noneIfEmpty(encoded.inclusions.objects).map("included" -> _.toList.toJAny)
      )
    }

    def toDocument(includeSpec: IncludeSpec, fieldsSpec: FieldsSpec)(implicit
        enc: ResourceEncoder[A]
    ): EncodeResult[JObject] =
      enc.encodeResource(a, includeSpec, fieldsSpec).map { encoded =>
        JObject.flatten(
          Some("data" -> encoded.root),
          noneIfEmpty(encoded.inclusions.objects).map("included" -> _.toList.toJAny)
        )
      }
  }
}
