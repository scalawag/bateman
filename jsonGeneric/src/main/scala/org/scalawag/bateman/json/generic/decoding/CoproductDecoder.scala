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

package org.scalawag.bateman.json.generic.decoding

import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.{DecodeResult, JObject}
import org.scalawag.bateman.json.generic.{Config, DiscriminatorFieldParam, TraitInfo, decoding}
import org.scalawag.bateman.json.generic.decoding.CoproductDecoderFactoryFactory.{Input, Params}
import shapeless.labelled.{FieldType, field}
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy, Witness}

/** Represents an automatically-derived [[Coproduct]] decoder.
  *
  * @tparam In the input type of the decoder
  * @tparam To the target type of the OutHeadder
  * @tparam Context the context in which this decoder operates
  */

trait CoproductDecoder[In, To <: Coproduct, Context] {

  /** Decodes the input [[In]] to an instance of type [[To]].
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def decode(input: Input[In, Context]): DecodeResult[To]
}

trait CoproductDecoderFactory[In, To <: Coproduct, Context] {

  /** Creates a decoder that decodes a JObject into an instance of type [[To]]
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def apply(params: Params): CoproductDecoder[JObject, To, Context]
}

trait CoproductDecoderFactoryFactory[In, To <: Coproduct, Context] {
  def apply(traitInfo: TraitInfo): CoproductDecoderFactory[In, To, Context]
}

object CoproductDecoderFactoryFactory {

  final case class Params(config: Config, discriminatorFieldOverride: Option[String]) extends DiscriminatorFieldParam

  /** Encapsulates all the input needed to decode an [[In]] to a [[Coproduct]].
    *
    * @param in the input object, to be decoded
    * @param context the context needed by this decoder to operate
    * @param discriminatorValuesHandled tracks the discriminator values that would have been handled by one of the
    *                                   [[CoproductDecoder]]s in the stack for error reporting
    * @tparam Context the type of the context in which this decoder operates
    */
  final case class Input[In, Context](
      in: In,
      context: Context,
      discriminatorValuesHandled: Set[String] = Set.empty
  ) {
    def withDiscriminatorValueHandled(value: String): Input[In, Context] =
      this.copy(discriminatorValuesHandled = this.discriminatorValuesHandled + value)
  }

  /** Fails to decodes a JObject to [[CNil]]. This decoder only gets used in situations where none of the
    * [[CoproductDecoder]]s in the stack were able to handle the incoming discriminator value.
    *
    * @tparam Context the context in which this decoder operates
    * @return the derived decoder
    */

  implicit def forCNil[Context]: CoproductDecoderFactoryFactory[JObject, CNil, Context] = { _ => params => input =>
    // If we got to here, it means that none of the subclass decoders could decode this (based on its discriminator).
    // So, get the discriminator value and return an error that contains that bad discriminator as well as all the ones
    // that could have been handled.
    input.in.apply(params.discriminatorField).andThen(_.asString).andThen { actualDiscriminatorValue =>
      decoding.InvalidDiscriminator(actualDiscriminatorValue, input.discriminatorValuesHandled.toList).invalidNec
    }
  }

  /** Decodes the input JObject to an instance of type [[OutHead]] if the discriminator value matches the name handled
    * by the [[headDecoder]], as indicated by [[Key]]. If the discriminator value doesn't match, defers to
    * [[tailDecoder]].
    *
    * @param headDecoder decodes a value of type [[OutHead]] and name [[Key]] from the input [[JObject]]
    * @param tailDecoder generates a decoder for the tail of the target [[Coproduct]]
    * @param config      configures the behavior of the generated decoder
    * @tparam OutHead the head of the target [[Coproduct]]
    * @tparam OutTail the tail of the target [[Coproduct]]
    * @tparam Context the context in which this decoder operates
    * @return the generated decoder
    */
  implicit def forCCons[OutHead, OutTail <: Coproduct, Context](implicit
      headDecoder: Lazy[CaseClassDecoder[OutHead, Context]],
      tailDecoderFactoryFactory: CoproductDecoderFactoryFactory[JObject, OutTail, Context]
  ): CoproductDecoderFactoryFactory[JObject, OutHead :+: OutTail, Context] =
    traitInfo => {
      val tailDecoderFactory = tailDecoderFactoryFactory(traitInfo.tail)

      params => {
        val tailDecoder = tailDecoderFactory(params)
        val discriminatorField = params.discriminatorField
        val expectedDiscriminatorValue = headDecoder.value.discriminatorValue

        input => {
          input.in.apply(discriminatorField).andThen(_.asString).map(_.value).andThen { actualDiscriminatorValue =>
            if (actualDiscriminatorValue == expectedDiscriminatorValue)
              headDecoder.value.decode(input.in, input.context, Some(discriminatorField)).map(Inl(_))
            else
              tailDecoder
                .decode(input.withDiscriminatorValueHandled(expectedDiscriminatorValue))
                .map(Inr(_))
          }
        }
      }
    }
}
