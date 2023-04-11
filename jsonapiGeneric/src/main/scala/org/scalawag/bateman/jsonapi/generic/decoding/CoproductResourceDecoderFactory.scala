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

import cats.syntax.semigroup._
import cats.instances.map._
import cats.syntax.either._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.{JAny, JObject, JObjectDecoder, JResult}
import org.scalawag.bateman.json.generic.{Config, TraitDeriverParams, decoding}
import org.scalawag.bateman.json.generic.decoding.CoproductDecoderFactory.Input
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy}

import scala.reflect.{ClassTag, classTag}


trait CoproductResourceDecoderFactory[To <: Coproduct] {

  /** Creates a decoder that decodes a JObject into an instance of type [[To]]
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def apply(params: TraitDeriverParams[JObjectDecoder]): CoproductResourceDecoder[To]
}

object CoproductResourceDecoderFactory {

  /** Fails the decoding process with a helpful error message. This decoder only gets used in situations where none
    * of the [[CoproductResourceDecoder]]s in the stack were chosen to decode, based on the incoming discriminator value.
    */

  implicit val forCNil: CoproductResourceDecoderFactory[CNil] = _ /* params are unused here */ =>
    new CoproductResourceDecoder[CNil] {

      // If we got to here, it means that none of the subclass decoders could decode this (based on its discriminator).
      // So, get the discriminator value and return an error that contains that bad discriminator as well as all the ones
      // that could have been handled.
      override def decode(input: Input): JResult[CNil] =
        decoding.InvalidDiscriminator(input.discriminatorFocus, input.discriminatorValuesHandled.toSet).leftNec

      override def discriminatorValues: Map[JAny, List[ClassTag[_]]] = Map.empty
    }

  /** Decodes the input JObject to an instance of type [[H]] if the discriminator value matches the one handled
    * by [[lazyHeadDecoder]]. If the discriminator value doesn't match, defers to [[tailDecoderFactory]].
    *
    * @param lazyHeadDecoder decodes a value of type [[H]] from the input [[JObject]]
    * @param tailDecoderFactory generates a decoder for the tail of the target [[Coproduct]]
    * @tparam H the head of the target [[Coproduct]], which is the type that headDecoder can decode
    * @tparam T the tail of the target [[Coproduct]], which contains the types of the remaining decoders that can
    *           be tried if the discriminator doesn't match
    * @return the generated decoder
    */
  implicit def forCCons[H: ClassTag, T <: Coproduct](implicit
      lazyHeadDecoder: Lazy[JObjectDecoder[H]],
      tailDecoderFactory: CoproductResourceDecoderFactory[T],
  ): CoproductResourceDecoderFactory[H :+: T] = {
    val headClass = classTag[H]

    params => {
      // Just pull the config into implicit scope.
      import params.implicitConfig
      // Build the tail decoder that will be used if headDecoder is not appropriate (based on the discriminator).
      val tailDecoder = tailDecoderFactory(params)
      // What we expect the discriminator value to be if the headDecoder is to be used (based on params).
      implicit val headDecoder: JObjectDecoder[H] = lazyHeadDecoder.value
      val disc = params.discriminator[H]
      val expectedDiscriminatorValue = disc.value

      // If we got back a decoder from the discriminator call, it means there's another decoder we should be using
      // for the head. If not, just use the one that we got implicitly.
      val effectiveHeadDecoder = disc.explicit.getOrElse(headDecoder)

      new CoproductResourceDecoder[H :+: T] {
        override def decode(input: Input): JResult[H :+: T] = {
          // Check the discriminator value in the input to see if it's what this decoder expects.
          // If so, attempt to decode. If not, defer to the tailDecoder.
          if (input.discriminatorFocus.value.stripLocation == expectedDiscriminatorValue)
            effectiveHeadDecoder.decode(input.in, input.consumedDiscriminatorFields).map(Inl(_))
          else
            tailDecoder
              .decode(input.withDiscriminatorValueHandled(expectedDiscriminatorValue))
              .map(Inr(_))
        }

        override def discriminatorValues: Map[JAny, List[ClassTag[_]]] = {
          // My discriminator map includes the value that I can handle, plus the values that tailDecoder can handle.
          val tailDiscriminators = tailDecoder.discriminatorValues
          Map[JAny, List[ClassTag[_]]](expectedDiscriminatorValue -> List(headClass)) combine tailDiscriminators
        }
      }
    }
  }
}
