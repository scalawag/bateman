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
import cats.syntax.apply._
import org.scalawag.bateman.json.decoding.{
  ContextualDecoder,
  DecodeResult,
  JAny,
  JObject,
  JPointer,
  MissingValue,
  UnexpectedValue
}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, DiscriminatorValueParam, SourceTag}
import org.scalawag.bateman.json.generic.decoding.HListDecoderFactoryFactory.{Input, Output, Params}
import org.scalawag.bateman.json.validIfEmpty
import shapeless.tag.@@
import shapeless.{::, HList, HNil, Lazy}

trait HListDecoder[Out <: HList, Context] {
  def decode(input: Input[Context]): DecodeResult[Output[Out]]
}

/** Represents an automatically-derived HList decoder.
  *
  * @tparam Out the target type of the decoder
  * @tparam Context the context in which this decoder operates
  */
trait HListDecoderFactory[Out <: HList, Context] {

  /** Creates a decoder that decodes a JObject into an instance of type [[Out]]
    *
    * @param input encapsulates all the input necessary for a decode call
    * @return the target type, if successful, or errors
    */
  def apply(params: Params): HListDecoder[Out, Context]
}

trait HListDecoderFactoryFactory[Out <: HList, Default <: HList, Context] {
  def apply(typeInfo: CaseClassInfo[Default]): HListDecoderFactory[Out, Context]
}

object HListDecoderFactoryFactory {

  final case class Params(config: Config, discriminatorValueOverride: Option[String]) extends DiscriminatorValueParam

  /** Encapsulates all the input needed to decode a JObject into an [[HList]].
    *
    * @param in the input object, to be decoded
    * @param context the context needed by this decoder to operate
    * @param defaults the default values to be used
    * @param fieldsHandled tracks the fields that have been "handled" (meaning used by the decoder) for error detection
    * @tparam Defaults the type of the defaults which, in our usage, always corresponds to the output HList type
    * @tparam Context the type of the context in which this decoder operates
    */
  final case class Input[Context](
      in: JObject,
      context: Context,
      discriminatorField: Option[String],
      fieldsHandled: Set[String] = Set.empty,
      fieldPointers: Map[String, JPointer] = Map.empty,
  ) {

    /** Stores position information for this field to be passed on to the decoded object (if it has a SourceTag field).
      * This also marks a field as having been used by the decoder. This enables the decoder to (optionally) fail if
      * any fields are not handled.
      */
    def withFieldHandled(name: String, scalaName: String, source: JPointer): Input[Context] =
      this.copy(fieldsHandled = this.fieldsHandled + name, fieldPointers = this.fieldPointers + (scalaName -> source))
  }

  final case class Output[Out <: HList](out: Out, fieldSources: Map[String, JPointer])

  /** Decodes a JObject to [[HNil]]. Optionally fails (depending on configuration) if it detects fields in the
    * incoming JObject that have not been consumed by another decoder in this stack of [[HListDecoder]]s.
    *
    * @tparam Context the context in which this decoder operates
    * @return the derived decoder
    */
  implicit def hnilDecoder[Context]: HListDecoderFactoryFactory[HNil, HNil, Context] =
    _ => {
      params => {
        new HListDecoder[HNil, Context] {
          override def decode(input: Input[Context]): DecodeResult[Output[HNil]] = {
            val output = Output[HNil](HNil, input.fieldPointers)
            if (params.config.allowUnknownFields)
              output.validNec
            else {
              val validKeys = input.fieldsHandled ++ input.discriminatorField
              validIfEmpty(
                input.in.fieldList.filterNot(n => validKeys(n.name.value)).map(_.value).map(UnexpectedValue),
                output
              )
            }
          }
        }
      }
    }

  /** Generically decodes an [[HList]] with head of type [[OutHead]] and name [[Key]] from an input [[JObject]].
    *
    * @param headDecoder               decodes a value of type [[OutHead]] (with the appropriate name)from the input [[JObject]]
    * @param tailDecoderFactoryFactory generates a decoder for the tail of the target [[HList]]
    * @tparam OutHead the head of the target [[HList]]
    * @tparam OutTail the tail of the target [[HList]]
    * @tparam DefaultTail the tail of the default values [[HList]]
    * @tparam Context the context in which this decoder operates
    * @return the generated decoder
    */
  implicit def hconsDecoder[OutHead, OutTail <: HList, DefaultTail <: HList, Context](implicit
      headDecoder: Lazy[ContextualDecoder[JAny, OutHead, Context]],
      tailDecoderFactoryFactory: HListDecoderFactoryFactory[OutTail, DefaultTail, Context]
  ): HListDecoderFactoryFactory[OutHead :: OutTail, Option[OutHead] :: DefaultTail, Context] =
    headDecoderFactoryFactory[OutHead, OutTail, DefaultTail, Context](JPointer.Root / _)(
      headDecoder.value.decode
    )

  /** Generically decodes an [[HList]] with head of type [[OutHead]] (a supertype of [[JObject]]) tagged as [[SourceTag]]
    * by using the input itself as the head of the output [[HList]].
    *
    * @param tailDecoder generates a decoder for the tail of the target [[HList]]
    * @tparam OutHead the head of the target [[HList]]
    * @tparam OutTail the tail of the target [[HList]]
    * @tparam DefaultTail the tail of the default values [[HList]]
    * @tparam Context the context in which this decoder operates
    * @return the generated decoder
    */
  implicit def hconsSourceDecoder[OutTail <: HList, DefaultTail <: HList, Context](implicit
      tailDecoder: HListDecoderFactoryFactory[OutTail, DefaultTail, Context]
  ): HListDecoderFactoryFactory[(JSource @@ SourceTag) :: OutTail, Option[
    JSource @@ SourceTag
  ] :: DefaultTail, Context] =
    headSourceDecoderFactoryFactory[JSource @@ SourceTag, OutTail, DefaultTail, Context](identity(_))

  /** Generically decodes an [[HList]] with head of type [[Option]] [ [[OutHead]] ] tagged as [[SourceTag]] (where
    * [[OutHead]] is a supertype of [[JObject]]) by using the input itself (wrapped in a [[Some]]) as the head of
    * the output [[HList]].
    *
    * @param tailDecoderFactoryFactory generates a decoder for the tail of the target [[HList]]
    * @tparam OutHead the head of the target [[HList]]
    * @tparam OutTail the tail of the target [[HList]]
    * @tparam DefaultTail the tail of the default values [[HList]]
    * @tparam Context the context in which this decoder operates
    * @return the generated decoder
    */
  implicit def hconsSourceOptionDecoder[OutTail <: HList, DefaultTail <: HList, Context](implicit
      tailDecoderFactoryFactory: HListDecoderFactoryFactory[OutTail, DefaultTail, Context]
  ): HListDecoderFactoryFactory[(Option[JSource] @@ SourceTag) :: OutTail, Option[
    Option[JSource] @@ SourceTag
  ] :: DefaultTail, Context] =
    headSourceDecoderFactoryFactory[Option[JSource] @@ SourceTag, OutTail, DefaultTail, Context](Some(_))

  def headSourceDecoderFactoryFactory[OutHead, OutTail <: HList, DefaultTail <: HList, Context](
      headDecoder: JSource => OutHead
  )(implicit
      tailDecoderFactoryFactory: HListDecoderFactoryFactory[OutTail, DefaultTail, Context]
  ): HListDecoderFactoryFactory[OutHead :: OutTail, Option[OutHead] :: DefaultTail, Context] =
    typeInfo => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoderFactory = tailDecoderFactoryFactory(typeInfo.tail)

      params => {
        val tailDecoder = tailDecoderFactory(params)
        val jsonFieldName = params.config.fieldNameMapping(typeInfo.fieldNames.head)

        input => {
          val tailResult = tailDecoder.decode(input.withFieldHandled(jsonFieldName, scalaFieldName, JPointer.Root))

          tailResult.map { tailOutput =>
            tailOutput.copy(headDecoder(JSource(input.in, tailOutput.fieldSources)) :: tailOutput.out)
          }
        }
      }
    }

  def headDecoderFactoryFactory[OutHead, OutTail <: HList, DefaultTail <: HList, Context](
      pointerFn: String => JPointer
  )(
      headDecoder: (JAny, Context) => DecodeResult[OutHead]
  )(implicit
      tailDecoderFactoryFactory: HListDecoderFactoryFactory[OutTail, DefaultTail, Context]
  ): HListDecoderFactoryFactory[OutHead :: OutTail, Option[OutHead] :: DefaultTail, Context] =
    typeInfo => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoderFactory = tailDecoderFactoryFactory(typeInfo.tail)

      params => {
        val tailDecoder = tailDecoderFactory(params)
        val jsonFieldName = params.config.fieldNameMapping(typeInfo.fieldNames.head)
        val pointer = pointerFn(jsonFieldName)

        input => {
          val tailResult = tailDecoder.decode(input.withFieldHandled(jsonFieldName, scalaFieldName, pointer))
          val headResult =
            (params.config.useDefaultsForMissingFields, typeInfo.defaults.head) match {
              case (true, Some(d)) =>
                pointer.navigateOption(input.in).andThen {
                  case Some(headJson) => headDecoder(headJson, input.context)
                  case None           => d.validNec
                }
              case _ =>
                pointer.navigate(input.in).andThen(headDecoder(_, input.context))
            }

          (headResult, tailResult).mapN { (h, t) => t.copy(out = h :: t.out) }
        }
      }
    }
}
