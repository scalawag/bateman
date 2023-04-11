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

package org.scalawag.bateman.json.generic.decoding

import cats.syntax.semigroup._
import cats.instances.map._
import cats.syntax.either._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.{JAny, JObject, JObjectDecoder, JResult}
import org.scalawag.bateman.json.generic.{Config, TraitDeriverParams, decoding}
import org.scalawag.bateman.json.generic.decoding.CoproductDecoderFactory.Input
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy}

import scala.reflect.{ClassTag, classTag}

/** Represents an automatically-derived [[Coproduct]] decoder. A [[CoproductDecoder]] is essentially a delegate
  * decoder (called the headDecoder) and a fallback decoder to try if the headDecoder is not appropriate, based
  * on the discriminator value (called the tailDecoder). This fallback itself is another [[CoproductDecoder]], so
  * the structure is recursive. Usually, the delegate decoder will be concrete, but that's not necessarily the case
  * (i.e., when there are multiple levels of discriminators being used).
  *
  * Beginning at the head and working to the tail, the actual discriminator value is compared against the one in
  * the input until a discriminator match is made. At that point, the search stops and the delegate is used to
  * decode, whether it works or not. If the search reaches the end of the structure without finding a discriminator
  * match, an invalid discriminator is reported. This error uses the data gathered during the traversal in the
  * Input object to report discriminator values that _would_ have been decoded had they been used.
  *
  * @tparam A the target type of the decoder (a trait)
  */

trait CoproductDecoder[A <: Coproduct] {

  /** Decodes the input to an instance of type [[A]].
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def decode(input: Input): JResult[A]

  /** Returns a map of discriminator values to [[ClassTag]]s that can be decoded by this decoder. */
  def discriminatorValues: Map[JAny, List[ClassTag[_]]]
}

trait CoproductDecoderFactory[To <: Coproduct] {

  /** Creates a decoder that decodes a JObject into an instance of type [[To]]
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def apply(params: TraitDeriverParams[JObjectDecoder]): CoproductDecoder[To]
}

object CoproductDecoderFactory {

  /** Encapsulates all the input needed to decode a [[JObject]] into a [[Coproduct]] (trait).
    *
    * @param in the object being decoded
    * @param discriminatorFocus the focus to the discriminator for this abstract decoder
    * @param consumedDiscriminatorFields a set of foci that have already been used to arrive at this abstract decoder.
    *                                    This should only have values when there are multiple levels of discrimination.
    *                                    It is (possibly) needed to report errors once a concrete decoder is chosen.
    * @param discriminatorValuesHandled a list of all discriminator values that, before this point in the decoder
    *                                   selection process, would have resulted in another decoder being chosen. This
    *                                   is used in the reporting of bad discriminators to list ones that may have
    *                                   worked.
    */
  final case class Input(
      in: JFocus[JObject],
      discriminatorFocus: JFocus[JAny],
      consumedDiscriminatorFields: Set[JFocus[JAny]],
      discriminatorValuesHandled: List[JAny]
  ) {
    def withDiscriminatorValueHandled(value: JAny): Input =
      this.copy(discriminatorValuesHandled = value :: this.discriminatorValuesHandled)
  }

  /** Fails the decoding process with a helpful error message. This decoder only gets used in situations where none
    * of the [[CoproductDecoder]]s in the stack were chosen to decode, based on the incoming discriminator value.
    */

  implicit val forCNil: CoproductDecoderFactory[CNil] = _ /* params are unused here */ =>
    new CoproductDecoder[CNil] {

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
      tailDecoderFactory: CoproductDecoderFactory[T],
  ): CoproductDecoderFactory[H :+: T] = {
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

      new CoproductDecoder[H :+: T] {
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
