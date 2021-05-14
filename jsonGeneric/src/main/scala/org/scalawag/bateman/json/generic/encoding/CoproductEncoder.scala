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

import org.scalawag.bateman.json.encoding.{JObject, JString}
import org.scalawag.bateman.json.generic.{Config, DiscriminatorFieldParam, TraitInfo}
import org.scalawag.bateman.json.generic.encoding.CoproductEncoderFactoryFactory.Params
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy}

trait CoproductEncoder[In <: Coproduct] {
  def encode(input: In): JObject
}

trait CoproductEncoderFactory[In <: Coproduct] {
  def apply(params: Params): CoproductEncoder[In]
}

trait CoproductEncoderFactoryFactory[In <: Coproduct] {
  def apply(traitInfo: TraitInfo): CoproductEncoderFactory[In]
}

object CoproductEncoderFactoryFactory {
  final case class Params(config: Config, discriminatorFieldOverride: Option[String]) extends DiscriminatorFieldParam

  /** An encoder for CNil. In practice, this decoder is never used because it fails if there's not an encoder for
    * every concrete type, but it's required to exist to fulfill the tail encoder parameter of [[coproductEncoder]].
    */

  implicit val forCNil: CoproductEncoderFactoryFactory[CNil] = { _ => _ => _ => ??? }

  /** Generates an encoder for the generic representation of a sealed trait. It uses the name of the class, passed
    * through the classNameMapping of the [[Config]] object, as the value of the discriminator field (which is also
    * set in the [[Config]] object).
    *
    * @param witness allows compile-time access to the name of the class
    * @param headEncoder
    * @param tailEncoder
    * @param config
    * @tparam Key the name of the class at the head of the input list
    * @tparam Head the type of the class at the head of the input list
    * @tparam Tail the type of the tail (as a Coproduct)
    * @return the generated encoder
    */
  implicit def forCCons[Head, Tail <: Coproduct](implicit
      headEncoder: Lazy[CaseClassEncoder[Head]],
      tailEncoderFactoryFactory: CoproductEncoderFactoryFactory[Tail]
  ): CoproductEncoderFactoryFactory[Head :+: Tail] =
    traitInfo => {
      val concreteClassName = traitInfo.subclassNames.head
      val tailEncoderFactory = tailEncoderFactoryFactory(traitInfo.tail)

      params => {
        val discriminatorField = params.discriminatorField
        val tailEncoder = tailEncoderFactory(params)

        {
          // This is not the decoder for the instance we've received as input. Defer to the tail encoder.
          case Inr(t) =>
            tailEncoder.encode(t)

          // This is the type we've received as input, so encode it.
          case Inl(h) =>
            val encoded = headEncoder.value.encode(h)

            // Detect whether the discriminator field is also a field added by the subclass encoder.
            // This is checked by the macro, but just in case they skip the macro.
            if (encoded.fields.exists(_._1 == discriminatorField))
              throw DiscriminatorFieldCollision(discriminatorField, concreteClassName)

            // Put the discriminator at the beginning of the encoded objects fields. This is just my personal preference.
            JObject((discriminatorField -> JString(headEncoder.value.discriminatorValue)) +: encoded.fields: _*)
        }
      }
    }
}
