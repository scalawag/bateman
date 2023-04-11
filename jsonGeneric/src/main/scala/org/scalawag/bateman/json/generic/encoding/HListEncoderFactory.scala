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

package org.scalawag.bateman.json.generic.encoding

import org.scalawag.bateman.json.{Encoder, JAny, JField, JObject, JString}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import org.scalawag.bateman.json.generic.encoding.HListEncoderFactory.Input
import shapeless.{::, HList, HNil, Lazy}

trait HListEncoderFactory[In <: HList, Defaults <: HList, Annots <: HList] {
  def apply(typeInfo: CaseClassInfo[Defaults], config: Config): HListEncoder[In, Defaults, Annots]
}

object HListEncoderFactory {
  final case class Input[In <: HList](in: In)

  object Input {
    implicit class InputOps[H, T <: HList](input: Input[H :: T]) {
      def tail: Input[T] = input.copy(in = input.in.tail)
      def wrapSomeHead: Input[Option[H] :: T] = input.copy(in = Some(input.in.head) :: input.in.tail)
    }
  }

  /** An encoder that produces the JObject representing HNil, which is just an empty JObject.  */

  implicit val hnilEncoder: HListEncoderFactory[HNil, HNil, HNil] = (_, _) => (_, discriminators) => {
    discriminators
  }

  private[generic] def prepend(key: String, value: JAny, obj: JObject): JObject =
    obj.copy(fieldList = obj.fieldList.::(JField(JString(key), value)))

  private def maybePrepend(key: String, valueOpt: Option[JAny], obj: JObject): JObject =
    valueOpt match {
      case Some(value) => prepend(key, value, obj)
      case None        => obj
    }

  implicit def forSomifiedHCons[H, T <: HList, DT <: HList, A <: HList](implicit
      factory: HListEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, A]
  ): HListEncoderFactory[H :: T, Option[H] :: DT, A] =
    (info, params) =>
      (input, discriminators) => factory.apply(info.doubleSomeHead, params).encode(input.wrapSomeHead, discriminators)

  /** Generates an encoder for an [[HList]] whose head is a [[H]] wrapped in an [[Option]].
    * Note that this instance also handles the case where the head is not wrapped in an instance due to the
    * presence of the [[forSomifiedHCons]] conversion.
    *
    * @param headEncoder can encode a [[H]] to a [[JAny]], used to encode the head
    * @param tailEncoderFactory can create an encoder for tail of the [[HList]]
    * @tparam H the type of the head (without its containing [[Option]])
    * @tparam T the type of the tail, for which there must also be an encoder
    * @return the generated factory
    */

  implicit def forHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headEncoder: Lazy[Encoder[H, JAny]],
      tailEncoderFactory: HListEncoderFactory[T, DT, AT]
  ): HListEncoderFactory[Option[H] :: T, Option[Option[H]] :: DT, HNil :: AT] =
    (info, config) => {
      val tailEncoder = tailEncoderFactory(info.tail, config)
      lazy val defaultEncoded = info.defaults.head.map(_.map(headEncoder.value.encode))

      (input, discriminators) => {
        val fieldName = config.fieldNameMapping(info.fieldNames.head)
        val encodedTail = tailEncoder.encode(input.tail, discriminators)
        lazy val inputEncoded = input.in.head.map(headEncoder.value.encode)

        // See if the input value is the same as the default value and, if so configured, skip it. Encode them both
        // to account for differences in the location -- just to make sure we're only skipping exactly what we would
        // have encoded.
        if (config.encodeDefaultValues || !defaultEncoded.contains(inputEncoded))
          maybePrepend(fieldName, inputEncoded, encodedTail)
        else
          encodedTail
      }
    }

  /** Generates an encoder for a labelled [[HList]] whose head is any type and is tagged with [[SourceTag]].
    * Source fields don't get encoded. Just this just defers to the tail encoder.
    *
    * @param tailEncoderFactory can encode the remaining items in the input [[HList]]
    * @tparam H the type of the head (without its containing [[Option]])
    * @tparam T the type of the tail, for which there must be an encoder
    * @return the generated encoder
    */

  implicit def forSourceHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      tailEncoderFactory: HListEncoderFactory[T, DT, AT],
  ): HListEncoderFactory[H :: T, Option[H] :: DT, (Source :: HNil) :: AT] =
    (info, params) => {
      val tailEncoder = tailEncoderFactory(info.tail, params)

      (input, discriminators) => {
        // Never contribute anything to this encoding... just return what the tail encoder said.
        tailEncoder.encode(input.tail, discriminators)
      }
    }
}
