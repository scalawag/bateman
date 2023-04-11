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

import cats.syntax.either._
import cats.syntax.parallel._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.{Decoder, JAny, JError, JObject, JResult, UnexpectedValue, rightIfEmpty}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import shapeless.{::, HList, HNil, Lazy}

trait HListDecoderFactory[Out <: HList, Default <: HList, Annotations <: HList] {
  def apply(typeInfo: CaseClassInfo[Default], config: Config): HListDecoder[Out]
}

object HListDecoderFactory {

  /** Encapsulates all the input needed to decode a JObject into an [[HList]].
    *
    * @param in                  the input object, to be decoded
    * @param discriminatorFields fields of the input object that have already been used as discriminators to arrive
    *                            at a JObjectDecoder implementation
    * @param fieldSources        names of the fields that have already been decoded and the focus whence they came
    */
  final case class Input(
      in: JFocus[JObject],
      discriminatorFields: Set[JFocus[JAny]],
      fieldSources: Map[String, JFocus[JAny]] = Map.empty,
  ) {

    /** Stores position information for this field to be passed on to the decoded object (if it has a SourceTag field).
      * This also marks a field as having been used by the decoder. This enables the decoder to (optionally) fail if
      * any fields are not handled.
      */
    def withFieldFocus(name: String, source: Option[JFocus[JAny]]): Input =
      source.map(f => this.copy(fieldSources = this.fieldSources + (name -> f))).getOrElse(this)
  }

  /** This is the value that is passed from the forHNil at the end of the decoding structure back up to the
    *  original caller.
    *
    * @param out the HList containing all of the decoded values
    * @param fieldSources the foci whence came the values in the output HList
    */
  final case class Output[Out <: HList](out: Out, fieldSources: Map[String, JFocus[JAny]])

  //===================================================================================================================
  // Type Class Instances

  /** Decodes a JObject to [[HNil]]. Optionally fails (depending on configuration) if it detects fields in the
    * incoming JObject that have not been consumed by another decoder in this stack of [[HListDecoder]]s.
    *
    * @return the derived decoder
    */
  implicit val forHNil: HListDecoderFactory[HNil, HNil, HNil] = (_, config) => {
    input => {
      val output = Output[HNil](HNil, input.fieldSources)
      if (config.allowUnknownFields)
        output.rightNec[JError]
      else {
        val validKeys = input.fieldSources.values.toSet ++ input.discriminatorFields
        rightIfEmpty(
          input.in.fields
            .filterNot(validKeys)
            .map(UnexpectedValue),
          output
        )
      }
    }
  }

  /** Generically decodes an [[HList]] with head of type  [[H]] from an input [[JObject]]. The corresponding field
    * is determined by the configuration for this decoder (specifically [[Config.fieldNameMapping]]).
    *
    * @param headDecoder        decodes a value of type [[H]] (with the appropriate name)from the input [[JObject]]
    * @param tailDecoderFactory generates a decoder for the tail of the target [[HList]]
    * @tparam H  the head of the target [[HList]]
    * @tparam T  the tail of the target [[HList]]
    * @tparam DT the tail of the default values [[HList]]
    * @return the generated decoder
    */

  implicit def forHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[Decoder[JAny, H]],
      tailDecoderFactory: HListDecoderFactory[T, DT, AT]
  ): HListDecoderFactory[H :: T, Option[H] :: DT, HNil :: AT] =
    (typeInfo, config) => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoder = tailDecoderFactory(typeInfo.tail, config)
      val jsonFieldName = config.fieldNameMapping(typeInfo.fieldNames.head)

      input => {
        input.in.fieldOption(jsonFieldName).flatMap { headFocusOpt =>
          val tailResult = tailDecoder.decode(input.withFieldFocus(scalaFieldName, headFocusOpt))
          val headResult: JResult[H] =
            (config.useDefaultsForMissingFields, typeInfo.defaults.head) match {
              // We have a default value that we can use if there's no field in the input object.
              case (true, Some(d)) =>
                headFocusOpt match {
                  case Some(headFocus) => headDecoder.value.decode(headFocus)
                  case None            => d.rightNec[JError]
                }
              case _ =>
                input.in.field(jsonFieldName).flatMap(headDecoder.value.decode)
            }

          (headResult, tailResult).parMapN { (h, t) => t.copy(out = h :: t.out) }
        }
      }
    }

  /** Generically decodes an [[HList]] with head of type [[Option]] [ [[H]] ] from an input [[JObject]]. If the
    * corresponding field is not present in the incoming JSON object, the field is decoded as `None`. Otherwise,
    * it's decoded as a `Some` of whatever the decoder returns. The corresponding field is determined by the
    * configuration for this decoder (specifically [[Config.fieldNameMapping]]).
    *
    * @param headDecoder        decodes a value of type [[H]] (with the appropriate name)from the input [[JObject]]
    * @param tailDecoderFactory generates a decoder for the tail of the target [[HList]]
    * @tparam H  the head of the target [[HList]]
    * @tparam T  the tail of the target [[HList]]
    * @tparam DT the tail of the default values [[HList]]
    * @return the generated decoder
    */

  implicit def forOptionHCons[H, T <: HList, DT <: HList, AT <: HList](implicit
      headDecoder: Lazy[Decoder[JAny, H]],
      tailDecoderFactory: HListDecoderFactory[T, DT, AT]
  ): HListDecoderFactory[Option[H] :: T, Option[Option[H]] :: DT, HNil :: AT] =
    (typeInfo, config) => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoder = tailDecoderFactory(typeInfo.tail, config)
      val jsonFieldName = config.fieldNameMapping(typeInfo.fieldNames.head)

      input => {
        input.in.fieldOption(jsonFieldName).flatMap { headFocusOpt =>
          val tailResult = tailDecoder.decode(input.withFieldFocus(scalaFieldName, headFocusOpt))
          val headResult: JResult[Option[H]] =
            headFocusOpt match {
              case Some(out) => headDecoder.value.decode(out).map(Some(_))
              case None      => None.rightNec
            }

          (headResult, tailResult).parMapN { (h, t) => t.copy(out = h :: t.out) }

          (headResult, tailResult).parMapN { (h, t) => t.copy(out = h :: t.out) }
        }
      }
    }

  /** Generically decodes an [[HList]] with head of type [[Option]] [ [[JSource]] ] annotated with [[Source]]
    * by generating an [[JSource]] instance from the decoding metadata and adding it to the head of the output
    * [[HList]].
    *
    * @param tailDecoderFactory generates a decoder for the tail of the target [[HList]]
    * @tparam T  the tail of the target [[HList]]
    * @tparam DT the tail of the default values [[HList]]
    * @return the generated decoder
    */

  implicit def forSourceHCons[T <: HList, DT <: HList, AT <: HList](implicit
      tailDecoder: HListDecoderFactory[T, DT, AT]
  ): HListDecoderFactory[JSource :: T, Option[JSource] :: DT, (Source :: HNil) :: AT] =
    forSourceHConsCommon[JSource, T, DT, AT](identity)

  /** Generically decodes an [[HList]] with head of type [[Option]] [ [[JSource]] ] annotated with [[Source]]
    * by generating an [[JSource]] instance from the decoding metadata and adding it (wrapped in a [[Some]]) to
    * the head of the output [[HList]].
    *
    * @param tailDecoderFactory generates a decoder for the tail of the target [[HList]]
    * @tparam T  the tail of the target [[HList]]
    * @tparam DT the tail of the default values [[HList]]
    * @return the generated decoder
    */
  implicit def forOptionSourceHCons[T <: HList, DT <: HList, AT <: HList](implicit
      tailDecoderFactory: HListDecoderFactory[T, DT, AT]
  ): HListDecoderFactory[Option[JSource] :: T, Option[Option[JSource]] :: DT, (Source :: HNil) :: AT] =
    forSourceHConsCommon[Option[JSource], T, DT, AT](Some(_))

  private def forSourceHConsCommon[H, T <: HList, DT <: HList, AT <: HList](
      headDecoder: JSource => H
  )(implicit
      tailDecoderFactory: HListDecoderFactory[T, DT, AT]
  ): HListDecoderFactory[H :: T, Option[H] :: DT, (Source :: HNil) :: AT] =
    (typeInfo, config) => {
      val scalaFieldName = typeInfo.fieldNames.head
      val tailDecoder = tailDecoderFactory(typeInfo.tail, config)

      input => {
        val tailResult = tailDecoder.decode(input)

        tailResult.map { tailOutput =>
          tailOutput.copy(headDecoder(JSource(input.in, tailOutput.fieldSources)) :: tailOutput.out)
        }
      }
    }
}
