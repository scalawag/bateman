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

package org.scalawag.bateman.jsonapi.generic.decoding

import cats.syntax.either._
import cats.syntax.parallel._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.generic.decoding.HListDecoderFactory.Input
import org.scalawag.bateman.json.{JAny, JObject, JObjectDecoder, JResult}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, MemberLabels}
import org.scalawag.bateman.jsonapi.{JsonApiTypeMismatch, lens}
import org.scalawag.bateman.jsonapi.generic.decoding.HListResourceDecoderFactory.Params
import shapeless.{AllAnnotations, Default, Generic, HList}

import scala.reflect.ClassTag

trait CaseClassResourceDecoderFactory[B] {
  def apply(config: Config, resourceTypeOverride: Option[String]): JObjectDecoder[B]
}

object CaseClassResourceDecoderFactory {

  implicit def caseClassDecoder[CaseClass: ClassTag, Generic <: HList, Defaults <: HList, Annots <: HList](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      annots: AllAnnotations.Aux[CaseClass, Annots],
      hlistDecoderFactory: HListResourceDecoderFactory[Generic, Defaults, Annots],
  ): CaseClassResourceDecoderFactory[CaseClass] = {
    val classInfo = CaseClassInfo(defaults(), fieldNames())

    (resourceTypeOverride, config) =>
      val params = Params(resourceTypeOverride, config)
      val hlistDecoder = hlistDecoderFactory(classInfo, params)
      val resourceType = params.resourceTypeFor[CaseClass]

      (in, discriminatorFields) => {
        val typeCheck: JResult[Unit] =
          in(lens.resourceType).flatMap { inType =>
            if (inType.value.value != resourceType)
              JsonApiTypeMismatch(inType, resourceType).leftNec
            else
              ().rightNec
          }

        // Use distinct here to deduplicate errors that result from the typeCheck and ones that come from
        // TypeAnn annotated fields.
        (typeCheck, hlistDecoder.decode(Input(in, discriminatorFields)).map(_.out).map(generic.from)).parTupled
          .map(_._2)
          .leftMap(_.distinct)
      }
  }
}
