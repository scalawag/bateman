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

package org.scalawag.bateman.json.generic.encoding

import org.scalawag.bateman.json.encoding.{Encoder, JAny, JObject}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, DiscriminatorValueParam, SourceTag}
import org.scalawag.bateman.json.generic.encoding.HListEncoderFactoryFactory.{Input, Params}
import shapeless.{::, HList, HNil, Lazy}
import shapeless.tag.@@

import scala.reflect.{ClassTag, classTag}

trait HListEncoder[In <: HList, Defaults <: HList] {
  def encode(input: Input[In]): JObject
}

trait HListEncoderFactory[In <: HList, Defaults <: HList] {
  def apply(params: Params): HListEncoder[In, Defaults]
}

trait HListEncoderFactoryFactory[In <: HList, Defaults <: HList] {
  def apply(typeInfo: CaseClassInfo[Defaults]): HListEncoderFactory[In, Defaults]
}

object HListEncoderFactoryFactory {
  final case class Params(config: Config, discriminatorValueOverride: Option[String]) extends DiscriminatorValueParam

  final case class Input[In <: HList](in: In)

  object Input {
    implicit class InputOps[H, T <: HList](input: Input[H :: T]) {
      def tail: Input[T] = input.copy(in = input.in.tail)
    }
  }

  /** An encoder that produces the JObject representing HNil, which is just an empty JObject.  */

  implicit val hnilEncoder: HListEncoderFactoryFactory[HNil, HNil] = { _ => _ => _ => JObject() }

  /** Generates an encoder for a labelled [[HList]] whose head is a [[Head]] wrapped in an [[Option]].
    *
    * @param headEncoder can encode a [[Head]] to a [[JAny]]
    * @param tailEncoder can encode the remaining items in the input [[HList]]
    * @param config configures how the input should be encoded
    * @tparam Head the type of the head (without its containing [[Option]])
    * @tparam Tail the type of the tail, for which there must be an encoder
    * @return the generated encoder
    */

  implicit def hconsEncoder[Head, Tail <: HList, DefaultsTail <: HList](implicit
      headEncoder: Lazy[Encoder[Head, JAny]],
      tailEncoderFactoryFactory: HListEncoderFactoryFactory[Tail, DefaultsTail]
  ): HListEncoderFactoryFactory[Head :: Tail, Option[Head] :: DefaultsTail] =
    info => {
      val tailEncoderFactory = tailEncoderFactoryFactory(info.tail)

      params => {
        val tailEncoder = tailEncoderFactory(params)

        input => {
          val fieldName = params.config.fieldNameMapping(info.fieldNames.head)
          val encodedTail = tailEncoder.encode(input.tail)

          // See if the input value is the same as the default value and, if so configured, skip it.
          if (params.config.encodeDefaultValues || !info.defaults.head.contains(input.in.head))
            JObject((fieldName -> headEncoder.value.encode(input.in.head)) +: encodedTail.fields: _*)
          else
            encodedTail
        }
      }
    }

  /** Generates an encoder for a labelled [[HList]] whose head is any type and is tagged with [[SourceTag]].
    * Source fields don't get encoded. Just this just defers to the tail encoder.
    *
    * @param tailEncoder can encode the remaining items in the input [[HList]]
    * @tparam Head the type of the head (without its containing [[Option]])
    * @tparam Tail the type of the tail, for which there must be an encoder
    * @return the generated encoder
    */

  implicit def hconsSourceEncoder[Head, Tail <: HList, DefaultsTail <: HList](implicit
      tailEncoderFactoryFactory: HListEncoderFactoryFactory[Tail, DefaultsTail],
  ): HListEncoderFactoryFactory[(Head @@ SourceTag) :: Tail, Option[Head @@ SourceTag] :: DefaultsTail] =
    info => {
      val tailEncoderFactory = tailEncoderFactoryFactory(info.tail)

      params => {
        val tailEncoder = tailEncoderFactory(params)

        input => {
          // Never contribute anything to this encoding... just return what the tail encoder said.
          tailEncoder.encode(input.tail)
        }
      }
    }
}
