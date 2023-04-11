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

import org.scalawag.bateman.json.JObject
import org.scalawag.bateman.json.generic.{CaseClassInfo, MemberLabels}
import org.scalawag.bateman.jsonapi.encoding.{FieldsSpec, ResourceEncoder}
import org.scalawag.bateman.jsonapi.encoding.IncludeSpec
import org.scalawag.bateman.jsonapi.generic.encoding.HListResourceEncoderFactory.{Input, Params}
import shapeless.{AllAnnotations, Default, Generic, HList}

import scala.reflect.ClassTag

trait CaseClassResourceEncoderFactory[In] {
  def apply(params: Params): ResourceEncoder[In]
}

object CaseClassResourceEncoderFactory {
  implicit def forCaseClass[CaseClass: ClassTag, Generic <: HList, Defaults <: HList, Annots <: HList](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      annots: AllAnnotations.Aux[CaseClass, Annots],
      genericEncoderFactory: HListResourceEncoderFactory[Generic, Defaults, Annots],
  ): CaseClassResourceEncoderFactory[CaseClass] =
    params => {
      val info = CaseClassInfo(defaults(), fieldNames())
      val paramResourceType = params.resourceTypeFor[CaseClass]
      val genericEncoder = genericEncoderFactory(info, params)

      (in, includeSpec, fieldsSpec, discriminators) => {
        val input = Input(generic.to(in), paramResourceType, includeSpec, fieldsSpec)
        genericEncoder.encode(input, discriminators).toEncoded
      }
    }
}
