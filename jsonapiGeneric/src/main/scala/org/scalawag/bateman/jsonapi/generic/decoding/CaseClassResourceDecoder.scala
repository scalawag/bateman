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

package org.scalawag.bateman.jsonapi.generic.decoding

import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.{DecodeResult, JPointer}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, MemberLabels}
import org.scalawag.bateman.jsonapi.decoding.{Document, JsonApiTypeMismatch, ResourceLike}
import org.scalawag.bateman.jsonapi.generic.decoding.HListResourceDecoderFactoryFactory.{Input, Params}
import shapeless.{Default, Generic, HList}

import scala.reflect.ClassTag

trait CaseClassResourceDecoder[In <: ResourceLike, Out] extends ResourceDecoder[In, Out] {
  def resourceType: String
}

object CaseClassResourceDecoder {
  def apply[In <: ResourceLike, Out](implicit
      dec: CaseClassResourceDecoder[In, Out]
  ): CaseClassResourceDecoder[In, Out] = dec
}

trait CaseClassResourceDecoderFactory[A <: ResourceLike, B] {
  def apply(resourceTypeOverride: Option[String], config: Config): CaseClassResourceDecoder[A, B]
}

object CaseClassResourceDecoderFactory {

  implicit def caseClassDecoder[In <: ResourceLike, CaseClass: ClassTag, Generic <: HList, Defaults <: HList](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      hlistDecoderFactoryFactory: HListResourceDecoderFactoryFactory[In, Generic, Defaults],
  ): CaseClassResourceDecoderFactory[In, CaseClass] = {
    val classInfo = CaseClassInfo(defaults(), fieldNames())
    val hlistDecoderFactory = hlistDecoderFactoryFactory(classInfo)

    (resourceTypeOverride, config) =>
      val params = Params(config, resourceTypeOverride)
      val hlistDecoder = hlistDecoderFactory(params)

      new CaseClassResourceDecoder[In, CaseClass] {
        override val resourceType: String = params.resourceTypeFor[CaseClass]

        override def decode(in: In, context: Document): DecodeResult[CaseClass] =
          if (in.`type`.value != resourceType)
            JsonApiTypeMismatch(in, resourceType).invalidNec
          else
            hlistDecoder.decode(Input(in, context)).map(_.out).map(generic.from)
      }
  }
}
