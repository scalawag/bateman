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

import org.scalawag.bateman.json
import org.scalawag.bateman.jsonapi
import org.scalawag.bateman.json.decoding.ContextualDecoder
import org.scalawag.bateman.json.encoding.Encoder
import org.scalawag.bateman.jsonapi.decoding.{ResourceDecoder, ResourceObjectOptionalId}
import org.scalawag.bateman.jsonapi.generic.decoding.CaseClassResourceDecoderFactory
import shapeless.Lazy

import scala.reflect.ClassTag

object auto {
  implicit def deriveDecoder[A](implicit
      classTag: ClassTag[A],
      decoderFactory: Lazy[CaseClassResourceDecoderFactory[ResourceObjectOptionalId, A]]
  ): ResourceDecoder[jsonapi.decoding.ResourceObjectOptionalId, A] =
    ??? //semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass(classTag.runtimeClass.getSimpleName)
//  implicit def deriveEncoder[A](implicit encoder: Lazy[DerivedEncoder[A]]): Encoder[A, JObject] = semiauto.deriveEncoder
}
