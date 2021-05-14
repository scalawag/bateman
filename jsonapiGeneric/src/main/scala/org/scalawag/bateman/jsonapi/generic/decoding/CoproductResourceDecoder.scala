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

import cats.data.NonEmptyChain
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.{DecodeResult, JObject}
import org.scalawag.bateman.json.generic.{Config, TraitInfo}
import org.scalawag.bateman.jsonapi.generic.decoding.CoproductResourceDecoderFactoryFactory.{Input, Params}
import org.scalawag.bateman.jsonapi.decoding.{Document, JsonApiTypeMismatch, ResourceLike}
import org.scalawag.bateman.jsonapi.generic.decoding.CoproductResourceDecoderFactoryFactory.Params
import shapeless.{:+:, Coproduct, Lazy, CNil, Inl, Inr}

/** Represents an automatically-derived [[Coproduct]] decoder.
  *
  * @tparam In the input type of the decoder
  * @tparam Out the target type of the decoder
  */

trait CoproductResourceDecoder[In <: ResourceLike, Out] {

  /** Decodes the input [[In]] to an instance of type [[Out]].
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def decode(input: Input[In]): DecodeResult[Out]
}

trait CoproductResourceDecoderFactory[In <: ResourceLike, Out] {
  def apply(params: Params): CoproductResourceDecoder[In, Out]
}

trait CoproductResourceDecoderFactoryFactory[In <: ResourceLike, Out] {
  def apply(traitInfo: TraitInfo): CoproductResourceDecoderFactory[In, Out]
}

object CoproductResourceDecoderFactoryFactory {
  final case class Input[In](
      in: In,
      document: Document,
      reourceTypesHandled: Set[String] = Set.empty
  ) {
    def withResourceTypeHandled(value: String): Input[In] =
      this.copy(reourceTypesHandled = this.reourceTypesHandled + value)
  }

  final case class Params(config: Config)

  /*
  /** Encapsulates all the input needed to decode an [[In]] to a [[Coproduct]].
   *
   * @param in                   the input object, to be decoded
   * @param context              the context needed by this decoder to operate
   * @param resourceTypesHandled tracks the discriminator values that would have been handled by one of the
   *                             [[CoproductResourceDecoder]]s in the stack for error reporting
   */
  final case class Input[In <: ResourceLike](in: In, context: Document, resourceTypesHandled: Set[String] = Set.empty) {
    def withResourceTypesHandled(values: NonEmptyChain[String]): Input[In] =
      this.copy(resourceTypesHandled = this.resourceTypesHandled ++ values.iterator)
  }

  /** Fails to decodes a resource to a [[CNil]]. This decoder only gets used in situations where none of the
   * [[CoproductResourceDecoder]]s in the stack were able to handle the incoming resource type.
   *
   * @param config configures the behavior of the generated decoder
   * @tparam Context the context in which this decoder operates
   * @return the derived decoder
   */
   */

  implicit def forCNil[In <: ResourceLike]: CoproductResourceDecoderFactoryFactory[In, CNil] = { _ => _ => input =>
    JsonApiTypeMismatch(
      input.in,
      NonEmptyChain.fromSeq(input.reourceTypesHandled.toList.sorted).get
    ).invalidNec
  }

  /** Decodes the input JObject to an instance of type [[OutHead]] if the discriminator value matches the name handled
    * by the `headDecoder`, as indicated by [[Key]]. If the discriminator value doesn't match, defers to
    * `tailDecoder`.
    *
    * @param witness     gives compile-time access to the name of the case class at the head of the target [[Coproduct]]
    * @param headDecoder decodes a value of type [[OutHead]] and name [[Key]] from the input [[JObject]]
    * @param tailDecoder generates a decoder for the tail of the target [[Coproduct]]
    * @param config      configures the behavior of the generated decoder
    * @tparam Key the symbolic name of the head of the target [[Coproduct]]
    * @tparam OutHead the head of the target [[Coproduct]]
    * @tparam OutTail the tail of the target [[Coproduct]]
    * @return the generated decoder
    */
  implicit def forCCons[In <: ResourceLike, OutHead, OutTail <: Coproduct](implicit
      headDecoder: Lazy[CaseClassResourceDecoder[In, OutHead]],
      tailDecoderFactoryFactory: CoproductResourceDecoderFactoryFactory[In, OutTail]
  ): CoproductResourceDecoderFactoryFactory[In, OutHead :+: OutTail] =
    traitInfo => {
      val tailDecoderFactory = tailDecoderFactoryFactory(traitInfo.tail)

      params => {
        val tailDecoder = tailDecoderFactory(params)
        val expectedResourceType = headDecoder.value.resourceType

        input => {
          if (input.in.`type`.value == expectedResourceType)
            headDecoder.value.decode(input.in, input.document).map(Inl(_))
          else
            tailDecoder.decode(input.withResourceTypeHandled(expectedResourceType)).map(Inr(_))
        }
      }
    }
}
