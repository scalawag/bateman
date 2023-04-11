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

import cats.syntax.semigroup._
import cats.instances.map._
import org.scalawag.bateman.json.{JAny, JObject, JObjectEncoder}
import org.scalawag.bateman.json.generic.{Config, TraitDeriverParams}
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy}

import scala.reflect.{ClassTag, classTag}

trait CoproductEncoderFactory[In <: Coproduct] {
  def apply(params: TraitDeriverParams[JObjectEncoder]): CoproductEncoder[In]
}

object CoproductEncoderFactory {
  private val unreachableFactory = new CoproductEncoder[CNil] {
    // Nothing needs to be implemented here because this is only used in a branch that can't be reached (CNil).
    override def encode(input: CNil, discriminators: JObject): JObject = ???
    override def discriminatorValues: Map[JAny, List[ClassTag[_]]] = Map.empty
  }

  /** An encoder for CNil. In practice, this decoder is never used because it fails if there's not an encoder for
    * every concrete type, but it's required to exist to fulfill the tail encoder parameter of [[coproductEncoder]].
    */

  implicit val forCNil: CoproductEncoderFactory[CNil] = _ => unreachableFactory

  /** Generates an encoder for the generic representation of a sealed trait. It uses the name of the class, passed
    * through the classNameMapping of the [[Config]] object, as the value of the discriminator field (which is also
    * set in the [[Config]] object).
    *
    * @param lazyHeadEncoder
    * @param tailEncoderFactory
    * @tparam Head the type of the class at the head of the input list
    * @tparam Tail the type of the tail (as a Coproduct)
    * @return the generated encoder
    */
  implicit def forCCons[Head: ClassTag, Tail <: Coproduct](implicit
      lazyHeadEncoder: Lazy[JObjectEncoder[Head]],
      tailEncoderFactory: CoproductEncoderFactory[Tail],
  ): CoproductEncoderFactory[Head :+: Tail] =
    params => {
      val headClass = classTag[Head]

      import params.implicitConfig
      implicit val headEncoder = lazyHeadEncoder.value
      val disc = params.discriminator[Head]

      val tailEncoder = tailEncoderFactory(params)

      new CoproductEncoder[Head :+: Tail] {
        override def encode(input: Head :+: Tail, discriminators: JObject): JObject =
          input match {
            case Inr(t) =>
              // This is not the decoder for the instance we've received as input. Defer to the tail encoder.
              tailEncoder.encode(t, discriminators)

            case Inl(h) =>
              // This is the type we've received as input, so encode it and then add our discriminator to it.
              // If we got back an encoder from the discriminator, use it. Otherwise, use the one we got implicitly.
              val effectiveHeadEncoder = disc.explicit.getOrElse(headEncoder)
              val newDiscriminatorValue = discriminators
              params.addDiscriminator(effectiveHeadEncoder.encode(h, newDiscriminatorValue), disc.value)
          }

        override def discriminatorValues: Map[JAny, List[ClassTag[_]]] = {
          val tailDiscriminators = tailEncoder.discriminatorValues
          Map[JAny, List[ClassTag[_]]](disc.value -> List(headClass)) combine tailDiscriminators
        }
      }
    }
}
